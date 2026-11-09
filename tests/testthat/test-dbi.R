dbi_repo <- function() {
  skip_if_no_fixtures()
  skip_if_not_installed("RSQLite"); skip_if_not_installed("blob")
  fx <- manifest()$fixtures$virtual_v2
  d <- withr::local_tempdir(.local_envir = parent.frame())
  file.copy(fx_path("virtual_v2"), d, recursive = TRUE)
  list(path = file.path(d, basename(fx_path("virtual_v2"))), auth = fx$virtual_prefix)
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
  a <- DBI::dbGetQuery(con, "SELECT * FROM arrays WHERE array = 'from_bin'")
  a$array <- "copy"
  rows <- DBI::dbGetQuery(con, "SELECT dim_0, dim_1, path, offset, size FROM refs_from_bin")

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
  rows <- DBI::dbGetQuery(a, "SELECT dim_0, dim_1, path, offset, size FROM refs_from_bin")
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
  rows <- DBI::dbGetQuery(a, "SELECT dim_0, dim_1, path, offset, size FROM refs_from_bin ORDER BY dim_0")
  ref_of <- function(con, i, j) DBI::dbGetQuery(con, sprintf(
    "SELECT offset FROM refs_from_bin WHERE dim_0 = %d AND dim_1 = %d", i, j))$offset

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
