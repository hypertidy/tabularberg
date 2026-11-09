dbi_repo <- function() {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("blob")
  local_vfx(parent.frame())
}

test_that("tables, reads and SQL", {
  r <- dbi_repo()
  con <- DBI::dbConnect(Icechunk(), r$path, authorize = r$auth)
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbIsValid(con))
  tabs <- DBI::dbListTables(con)
  expect_true(all(c("meta", "arrays", "sources", "branches", "ancestry", "refs_from_bin") %in% tabs))
  expect_true(DBI::dbExistsTable(con, "refs_from_bin"))
  refs <- DBI::dbReadTable(con, "refs_from_bin")
  expect_identical(names(refs), c("dim_0", "dim_1", "present", "path", "offset", "size", "info", "data"))
  expect_identical(DBI::dbListFields(con, "refs_from_bin"), names(refs))
  q <- DBI::dbGetQuery(con, "SELECT path, count(*) AS n, sum(size) AS bytes FROM refs_from_bin GROUP BY path")
  expect_equal(q$n, nrow(refs))
  expect_equal(q$bytes, sum(refs$size))
  rs <- DBI::dbSendQuery(con, "SELECT * FROM branches")
  expect_identical(DBI::dbFetch(rs)$name, "main")
  expect_true(DBI::dbHasCompleted(rs)); DBI::dbClearResult(rs)
  expect_error(DBI::dbExecute(con, "DELETE FROM refs_from_bin"), "read-only")
  expect_error(DBI::dbReadTable(con, "nope"), "no table")
})

test_that("transactions: append, commit, rollback, autocommit, time travel", {
  r <- dbi_repo()
  con <- DBI::dbConnect(Icechunk(), r$path, authorize = r$auth)
  on.exit(DBI::dbDisconnect(con))
  snap0 <- DBI::dbGetInfo(con)$snapshot
  a <- DBI::dbGetQuery(con, "SELECT * FROM arrays WHERE \"array\" = 'from_bin'")
  a$array <- "copy"
  rows <- DBI::dbGetQuery(con, "SELECT dim_0, dim_1, path, \"offset\", size FROM refs_from_bin")

  DBI::dbBegin(con)
  expect_error(DBI::dbBegin(con), "already open")
  expect_equal(DBI::dbAppendTable(con, "arrays", a), 1)
  expect_equal(DBI::dbAppendTable(con, "refs_copy", rows), nrow(rows))
  expect_true(DBI::dbGetInfo(con)$in_transaction)
  snap1 <- DBI::dbCommit(con, message = "copy via DBI")
  expect_false(DBI::dbGetInfo(con)$in_transaction)
  expect_identical(DBI::dbGetInfo(con)$snapshot, snap1)
  s <- ic_session_readonly(ic_repo_open(r$path, authorize = r$auth), branch = "main")
  expect_identical(ic_get(s, "copy/c/1/0"), ic_get(s, "from_bin/c/1/0"))

  DBI::dbBegin(con)
  DBI::dbAppendTable(con, "refs_copy", rows[1, ])
  DBI::dbRollback(con)
  expect_identical(DBI::dbGetInfo(con)$snapshot, snap1)

  DBI::dbAppendTable(con, "refs_copy", rows[1, ])  # autocommit
  anc <- DBI::dbReadTable(con, "ancestry")
  expect_match(anc$message[1], "DBI: 1 rows into refs_copy")
  expect_identical(anc$message[2], "copy via DBI")

  old <- DBI::dbConnect(Icechunk(), r$path, snapshot = snap0, authorize = r$auth)
  expect_false(DBI::dbExistsTable(old, "refs_copy"))
  expect_error(DBI::dbBegin(old), "pinned")
  DBI::dbDisconnect(old)
  expect_error(DBI::dbAppendTable(con, "refs_nothere", rows), "no array")
})

test_that("two writers: the second commit conflicts, rollback recovers", {
  r <- dbi_repo()
  a <- DBI::dbConnect(Icechunk(), r$path, authorize = r$auth)
  b <- DBI::dbConnect(Icechunk(), r$path, authorize = r$auth)
  on.exit({ DBI::dbDisconnect(a); DBI::dbDisconnect(b) })
  rows <- DBI::dbGetQuery(a, "SELECT dim_0, dim_1, path, \"offset\", size FROM refs_from_bin")
  DBI::dbBegin(a); DBI::dbBegin(b)
  DBI::dbAppendTable(a, "refs_from_bin", rows[1, ])
  DBI::dbAppendTable(b, "refs_from_bin", rows[2, ])
  DBI::dbCommit(a, message = "writer a")
  expect_error(DBI::dbCommit(b, message = "writer b"))
  expect_true(DBI::dbGetInfo(b)$in_transaction)
  DBI::dbRollback(b)
  expect_identical(DBI::dbReadTable(b, "ancestry")$message[1], "writer a")
})

test_that("dbCommit(rebase = TRUE): disjoint chunks rebase, same chunk needs a solver", {
  r <- dbi_repo()
  a <- DBI::dbConnect(Icechunk(), r$path, authorize = r$auth)
  b <- DBI::dbConnect(Icechunk(), r$path, authorize = r$auth)
  on.exit({ DBI::dbDisconnect(a); DBI::dbDisconnect(b) })
  rows <- DBI::dbGetQuery(a, "SELECT dim_0, dim_1, path, \"offset\", size FROM refs_from_bin ORDER BY dim_0")
  ref_of <- function(con, i, j) DBI::dbGetQuery(con, sprintf(
    "SELECT \"offset\" FROM refs_from_bin WHERE dim_0 = %d AND dim_1 = %d", i, j))$offset

  # different chunks: detect rebases cleanly, both commits land
  DBI::dbBegin(a); DBI::dbBegin(b)
  x <- rows[1, ]; x$offset <- rows$offset[2]
  y <- rows[3, ]; y$offset <- rows$offset[4]
  DBI::dbAppendTable(a, "refs_from_bin", x)
  DBI::dbAppendTable(b, "refs_from_bin", y)
  DBI::dbCommit(a, message = "a: chunk 0")
  expect_no_error(DBI::dbCommit(b, message = "b: chunk 2", rebase = TRUE))
  expect_identical(DBI::dbReadTable(b, "ancestry")$message[1:2], c("b: chunk 2", "a: chunk 0"))
  expect_equal(ref_of(b, x$dim_0, x$dim_1), rows$offset[2])
  expect_equal(ref_of(b, y$dim_0, y$dim_1), rows$offset[4])

  # same chunk: detect refuses and leaves the transaction open
  same <- function() {
    DBI::dbBegin(a); DBI::dbBegin(b)
    p <- rows[1, ]; p$offset <- rows$offset[1]
    q <- rows[1, ]; q$offset <- rows$offset[3]
    DBI::dbAppendTable(a, "refs_from_bin", p)
    DBI::dbAppendTable(b, "refs_from_bin", q)
    DBI::dbCommit(a, message = "a: chunk 0 again")
  }
  same()
  expect_error(DBI::dbCommit(b, message = "b", rebase = TRUE), class = "icechunk_conflict")
  expect_true(DBI::dbGetInfo(b)$in_transaction)
  DBI::dbRollback(b)

  # basic solver: ours keeps b's ref, theirs keeps a's
  same()
  DBI::dbCommit(b, message = "b ours", rebase = TRUE, solver = "basic", on_chunk = "ours")
  expect_equal(ref_of(b, rows$dim_0[1], rows$dim_1[1]), rows$offset[3])
  same()
  DBI::dbCommit(b, message = "b theirs", rebase = TRUE, solver = "basic", on_chunk = "theirs")
  expect_equal(ref_of(b, rows$dim_0[1], rows$dim_1[1]), rows$offset[1])
})

test_that("appended refs inherit the checksum of a known source", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("blob")
  fx <- local_vfx(); fx$virtual_prefix <- fx$auth
  src <- ic_session_readonly(ic_repo_open(fx$path, authorize = fx$virtual_prefix))
  br <- ic_to_byterefs(src)
  # private copy of the source, recorded with its mtime (so refs carry a checksum)
  d <- withr::local_tempdir()
  f <- file.path(d, "grid.bin")
  file.copy(sub("^file://", "", ic_chunk_refs(src, "/from_bin")$location[1]), f)
  now <- floor(as.numeric(file.mtime(f)))
  rf <- br$refs
  br$sources <- br$sources[0, ]; br$sources[1, ] <- list(1, f, NA, now, NA)
  br$refs <- function(a) { r <- rf(a); r$path[!is.na(r$path)] <- f; r$mtime <- now; r }
  rp <- file.path(withr::local_tempdir(), "r")
  ic_repo_from_byterefs(br, rp)
  auth <- paste0("file://", d, "/")

  con <- DBI::dbConnect(Icechunk(), rp, authorize = auth)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbReadTable(con, "sources")$mtime, now)
  a <- DBI::dbGetQuery(con, "SELECT * FROM arrays WHERE \"array\" = 'from_bin'"); a$array <- "copy"
  rows <- DBI::dbGetQuery(con, "SELECT dim_0, dim_1, path, \"offset\", size FROM refs_from_bin")
  expect_false(any(c("etag", "mtime") %in% names(rows)))
  DBI::dbBegin(con)
  DBI::dbAppendTable(con, "arrays", a)
  DBI::dbAppendTable(con, "refs_copy", rows)
  DBI::dbCommit(con, message = "copy")

  s <- ic_session_readonly(ic_repo_open(rp, authorize = auth), branch = "main")
  orig <- ic_chunk_refs(s, "/from_bin"); cp <- ic_chunk_refs(s, "/copy")
  expect_false(anyNA(cp$last_modified))
  expect_identical(cp$last_modified, orig$last_modified)
  expect_equal(DBI::dbReadTable(con, "sources")$mtime, now)  # round-trips without drift
  expect_type(ic_get(s, cp$key[1]), "raw")
  Sys.setFileTime(f, Sys.time() + 3600)
  s <- ic_session_readonly(ic_repo_open(rp, authorize = auth), branch = "main")
  expect_error(ic_get(s, cp$key[1]), "checksum|modified")

  # supplied values win; checksums = FALSE records none
  DBI::dbAppendTable(con, "refs_copy", transform(rows[1, ], mtime = now + 7200))
  s <- ic_session_readonly(ic_repo_open(rp, authorize = auth), branch = "main")
  k <- paste(rows$dim_0[1], rows$dim_1[1], sep = ",")
  lm <- ic_chunk_refs(s, "/copy"); lm <- lm$last_modified[lm$coord == k]
  expect_equal(as.numeric(lm), now + 7201)
  DBI::dbAppendTable(con, "refs_copy", rows[1, ], checksums = FALSE)
  s <- ic_session_readonly(ic_repo_open(rp, authorize = auth), branch = "main")
  lm <- ic_chunk_refs(s, "/copy"); expect_true(is.na(lm$last_modified[lm$coord == k]))
})

test_that("duckdb and sqlite engines agree", {
  skip_if_not_installed("duckdb")
  q <- "SELECT r.dim_0, r.dim_1, r.size, s.source_id FROM refs_from_bin r JOIN sources s ON s.source = r.path ORDER BY r.dim_0, r.dim_1"
  res <- lapply(c("duckdb", "sqlite"), function(e) {
    r <- dbi_repo()
    con <- DBI::dbConnect(Icechunk(), r$path, authorize = r$auth, sql = e)
    on.exit(DBI::dbDisconnect(con))
    expect_identical(DBI::dbGetInfo(con)$sql, e)
    x <- DBI::dbGetQuery(con, q)
    # a write invalidates what the engine holds
    DBI::dbAppendTable(con, "refs_from_bin", transform(DBI::dbGetQuery(con, "SELECT dim_0, dim_1, path, \"offset\", size FROM refs_from_bin LIMIT 1"), size = 1))
    list(x, DBI::dbGetQuery(con, "SELECT min(size) AS m FROM refs_from_bin")$m)
  })
  expect_equal(lapply(res[[1]][[1]], as.numeric), lapply(res[[2]][[1]], as.numeric))
  expect_equal(res[[1]][[2]], 1)
  expect_equal(res[[2]][[2]], 1)
})
