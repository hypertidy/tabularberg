test_that("a repository built from nothing through DBI", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("blob")
  ex <- ic_example_repo()
  p <- withr::local_tempdir()
  con <- DBI::dbConnect(Icechunk(), file.path(p, "r"), create = TRUE,
                        containers = ic_container(ex$authorize, "local", path = ex$source), authorize = ex$authorize)
  on.exit(DBI::dbDisconnect(con))
  expect_false(any(DBI::dbListTables(con) == "refs_"))
  DBI::dbBegin(con)
  DBI::dbAppendTable(con, "arrays", data.frame(array = "grid", kind = "data", storage = "referenced",
    dims = '["y","x"]', shape = "[64,48]", chunks = "[16,48]", dtype = "<f4", fill = "NaN"))
  DBI::dbAppendTable(con, "refs_grid", data.frame(dim_0 = 0:3, dim_1 = 0L,
    path = file.path(ex$source, "grid.bin"), offset = (0:3) * 3072, size = 3072))
  DBI::dbCommit(con, "built")
  s <- ic_session_readonly(ic_repo_open(file.path(p, "r"), authorize = ex$authorize), branch = "main")
  expect_true(ic_exists(s, "zarr.json"))
  expect_equal(readBin(ic_get(s, "grid/c/3/0"), "double", 768, size = 4, endian = "little"), ex$values[2304 + 1:768])
})

test_that("byterefs SQLite round trip with an all-inline array", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("blob")
  ex <- ic_example_repo()
  s <- ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main")
  db <- withr::local_tempfile(fileext = ".sqlite")
  ic_to_byterefs(s, dsn = db)
  br <- ic_byterefs_read(db)
  expect_type(br$refs("small")$path, "character")
  r2 <- ic_repo_from_byterefs(br, withr::local_tempdir(), authorize = ex$authorize)
  cmp <- ic_byterefs_compare(ic_to_byterefs(s), ic_to_byterefs(ic_session_readonly(r2, branch = "main")))
  expect_true(all(cmp$ok))
  br$close()
})
