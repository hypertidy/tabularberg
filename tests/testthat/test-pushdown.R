split_repo <- function(env = parent.frame()) {
  skip_if_no_fixtures(); skip_if_not_installed("RSQLite"); skip_if_not_installed("blob")
  fx <- manifest()$fixtures$virtual_v2
  src <- ic_session_readonly(ic_repo_open(fx_path("virtual_v2"), authorize = fx$virtual_prefix))
  br <- ic_to_byterefs(src)
  rp <- file.path(withr::local_tempdir(.local_envir = env), "r")
  ic_repo_from_byterefs(br, rp, manifest_split = c("0" = 1), authorize = fx$virtual_prefix, checksums = FALSE)
  list(path = rp, auth = fx$virtual_prefix, src = src)
}

test_that("manifest_split rules", {
  expect_identical(.split_json(NULL), "")
  j <- jsonlite::fromJSON(.split_json(c(time = 1, "0" = 30)), simplifyVector = FALSE)
  expect_null(j[[1]]$path)
  expect_identical(j[[1]]$dims, list(list("time", 1L), list("0", 30L)))
  j <- jsonlite::fromJSON(.split_json(list(list(path = "^/sst$", dims = c("*" = 4)), list(dims = c(time = 1)))), simplifyVector = FALSE)
  expect_identical(j[[1]]$path, "^/sst$")
  expect_length(j, 2)
  expect_error(.split_json(c(1, 2)), "named")
  expect_error(.split_json(c(time = 0)), "positive")
})

test_that("split repository: one manifest per chunk row, slab reads agree with full", {
  r <- split_repo()
  expect_gt(length(list.files(file.path(r$path, "manifests"))), 2)
  s <- ic_session_readonly(ic_repo_open(r$path, authorize = r$auth), branch = "main")
  root <- .br_root(s)
  full <- .br_array(s, "/from_bin", root)$refs
  sl <- .br_array(s, "/from_bin", root, ranges = list(c(1, 3), c(0, Inf)))$refs
  want <- full[full$dim_0 >= 1 & full$dim_0 < 3, ]
  expect_equal(sl[order(sl$dim_0, sl$dim_1), 1:6], want[order(want$dim_0, want$dim_1), 1:6], ignore_attr = TRUE)
  expect_null(.br_array(s, "/from_bin", root, ranges = list(c(9, 9), c(0, 1)))$refs)
  # bytes still right through the split manifests
  for (k in c("from_bin/c/0/0", "from_bin/c/3/0")) expect_identical(ic_get(s, k), ic_get(r$src, k))
})

test_that("pushdown parser: accepts plain conjunctions, declines the rest", {
  f <- function(sql, nd = 3) .dbi_slab(sql, "refs_a", nd)
  expect_equal(f("SELECT * FROM refs_a WHERE dim_0 = 5")[[1]], c(5, 6))
  expect_equal(f('SELECT COUNT(*) FROM "refs_a" WHERE ("dim_0" = 200.0)')[[1]], c(200, 201))
  b <- f('SELECT * FROM "refs_a" WHERE ("dim_0" BETWEEN 10.0 AND 12.0) AND ("dim_2" < 4.0)')
  expect_equal(b[[1]], c(10, 13)); expect_equal(b[[2]], c(0, Inf)); expect_equal(b[[3]], c(0, 4))
  expect_equal(f("SELECT * FROM refs_a r WHERE r.dim_1 IN (10.0, 11.0, 14.0)")[[2]], c(10, 15))
  expect_equal(f("SELECT * FROM refs_a WHERE 3 <= dim_0 AND dim_0 < 7")[[1]], c(3, 7))
  expect_equal(f('SELECT * FROM "refs_a" AS "q01" WHERE (0 = 1)'), rep(list(c(0, 0)), 3))
  for (sql in c("SELECT * FROM refs_a WHERE dim_0 = 5 OR size > 3",
                "SELECT * FROM refs_a WHERE NOT dim_0 = 5",
                "SELECT * FROM (SELECT * FROM refs_a) WHERE dim_0 = 5",
                "SELECT * FROM refs_a a JOIN refs_a b USING (dim_0) WHERE a.dim_0 = 1",
                "SELECT * FROM refs_a WHERE dim_10 = 5",
                "SELECT * FROM refs_a WHERE path = 'dim_0 = 3'",
                "SELECT * FROM refs_a WHERE dim_0 = 2.5",
                "SELECT CASE WHEN dim_0 = 1 THEN 1 END FROM refs_a",
                "SELECT * FROM refs_a")) expect_null(f(sql), info = sql)
})

test_that("SQL with pushdown gives the same answers as without, on both engines", {
  r <- split_repo()
  qs <- c("SELECT dim_0, dim_1, \"offset\", size FROM refs_from_bin WHERE dim_0 = 2 ORDER BY dim_1",
          "SELECT count(*) AS n, sum(size) AS b FROM refs_from_bin WHERE dim_0 BETWEEN 1 AND 2 AND dim_1 < 1",
          "SELECT count(*) AS n FROM refs_from_bin WHERE dim_0 = 99")
  for (eng in c("sqlite", if (requireNamespace("duckdb", quietly = TRUE)) "duckdb")) {
    on <- DBI::dbConnect(Icechunk(), r$path, authorize = r$auth, sql = eng)
    off <- DBI::dbConnect(Icechunk(), r$path, authorize = r$auth, sql = eng, pushdown = FALSE)
    for (q in qs) {
      a <- DBI::dbGetQuery(on, q)
      expect_length(on@state$last_pushdown, 1L)
      expect_equal(lapply(a, as.numeric), lapply(DBI::dbGetQuery(off, q), as.numeric), info = paste(eng, q))
    }
    # a later full-table query is not answered from the slab
    expect_equal(DBI::dbGetQuery(on, "SELECT count(*) AS n FROM refs_from_bin")$n, nrow(DBI::dbReadTable(off, "refs_from_bin")))
    sl <- DBI::dbReadTable(on, "refs_from_bin", chunks = list(dim_0 = c(1, 3)))
    expect_true(all(sl$dim_0 %in% 1:2))
    DBI::dbDisconnect(on); DBI::dbDisconnect(off)
  }
})

test_that("ic_refs_arrow matches the R refs table", {
  skip_if_not_installed("nanoarrow")
  r <- split_repo()
  s <- ic_session_readonly(ic_repo_open(r$path, authorize = r$auth), branch = "main")
  a <- nanoarrow::convert_array_stream(ic_refs_arrow(s, "from_bin", batch_rows = 2L))
  b <- .br_array(s, "/from_bin", .br_root(s))$refs
  a <- a[order(a$dim_0, a$dim_1), ]; b <- b[order(b$dim_0, b$dim_1), ]
  expect_identical(names(a), c("dim_0", "dim_1", "present", "path", "offset", "size", "info", "data"))
  expect_identical(as.character(a$path), b$path)
  expect_equal(as.numeric(a$offset), b$offset)
  expect_equal(as.numeric(a$size), b$size)
  sl <- nanoarrow::convert_array_stream(ic_refs_arrow(s, "from_bin", chunks = list(dim_0 = 3)))
  expect_true(all(sl$dim_0 == 3))
  st <- ic_refs_arrow(s, "from_bin", chunks = list(dim_0 = c(7, 7)))
  expect_identical(nrow(nanoarrow::convert_array_stream(st)), 0L)
})

test_that("dbplyr: tbl() on refs tables, filters pushed down", {
  skip_if_not_installed("dbplyr"); skip_if_not_installed("dplyr")
  r <- split_repo()
  con <- DBI::dbConnect(Icechunk(), r$path, authorize = r$auth)
  on.exit(DBI::dbDisconnect(con))
  refs <- dplyr::tbl(con, "refs_from_bin")
  expect_identical(colnames(refs), DBI::dbListFields(con, "refs_from_bin"))
  got <- dplyr::collect(dplyr::summarise(dplyr::filter(refs, dim_0 == 2), n = dplyr::n(), b = sum(size, na.rm = TRUE)))
  expect_length(con@state$last_pushdown, 1L)
  want <- DBI::dbGetQuery(con, "SELECT count(*) AS n, sum(size) AS b FROM refs_from_bin WHERE dim_0 = 2")
  expect_equal(as.numeric(got$n), as.numeric(want$n)); expect_equal(got$b, want$b)
  j <- dplyr::collect(dplyr::inner_join(refs, dplyr::tbl(con, "sources"), by = c(path = "source")))
  expect_equal(nrow(j), nrow(DBI::dbReadTable(con, "refs_from_bin")))
})
