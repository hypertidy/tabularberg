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
