# local_vfx plus /nat: float64 64 x 64 in 32 x 32 chunks written through the
# session (8 KiB each, so native chunks, not inline)
nat_repo <- function(env = parent.frame()) {
  fx <- local_vfx(env)
  repo <- ic_repo_open(fx$path, authorize = fx$auth)
  s <- ic_session_writable(repo, "main")
  ic_set(s, "nat/zarr.json", charToRaw(
    '{"zarr_format":3,"node_type":"array","shape":[64,64],"data_type":"float64",
      "chunk_grid":{"name":"regular","configuration":{"chunk_shape":[32,32]}},
      "chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},
      "fill_value":0.0,"codecs":[{"name":"bytes","configuration":{"endian":"little"}}],
      "attributes":{},"dimension_names":["y","x"]}'))
  for (i in 0:1) for (j in 0:1)
    ic_set(s, sprintf("nat/c/%d/%d", i, j), writeBin(as.double(i * 10 + j + seq_len(1024)), raw(), size = 8, endian = "little"))
  ic_commit(s, "native array")
  fx
}

test_that("ic_refs_arrow: native chunk bytes, like the R refs view", {
  skip_if_not_installed("nanoarrow"); skip_if_not_installed("RSQLite")
  fx <- nat_repo()
  s <- ic_session_readonly(ic_repo_open(fx$path, authorize = fx$auth), branch = "main")
  a <- nanoarrow::convert_array_stream(ic_refs_arrow(s, "nat", batch_rows = 3L))
  b <- .br_array(s, "/nat", .br_root(s), native = TRUE)$refs
  o <- function(x) x[order(x$dim_0, x$dim_1), ]
  a <- o(a); b <- o(b)
  expect_identical(nrow(a), 4L)
  expect_true(all(is.na(a$path)))
  expect_identical(lapply(a$data, as.raw), lapply(b$data, as.raw))
  expect_identical(lengths(a$data), rep(8192L, 4))
  n <- nanoarrow::convert_array_stream(ic_refs_arrow(s, "nat", native = FALSE))
  expect_true(all(vapply(n$data, is.null, NA)))
  # a slab fetches only its own bytes
  sl <- nanoarrow::convert_array_stream(ic_refs_arrow(s, "nat", chunks = list(dim_0 = 1, dim_1 = 0)))
  expect_identical(as.raw(sl$data[[1]]), ic_get(s, "nat/c/1/0"))
})

test_that("ic_refs_arrow is lazy: batches on demand, a snapshot of the session, early release", {
  skip_if_not_installed("nanoarrow")
  fx <- nat_repo()
  repo <- ic_repo_open(fx$path, authorize = fx$auth)
  s <- ic_session_writable(repo, "main")
  st <- ic_refs_arrow(s, "from_bin", batch_rows = 1L)
  # written after the stream was made: not seen by it
  ic_set_virtual_refs(s, "/from_bin", data.frame(dim_0 = 0L, dim_1 = 0L, location = paste0(fx$auth, "grid.nc"),
                                                 offset = 2048, length = 3072))
  nb <- 0L; rows <- 0L; paths <- character()
  while (!is.null(b <- st$get_next())) {
    nb <- nb + 1L; d <- nanoarrow::convert_array(b); rows <- rows + nrow(d); paths <- c(paths, as.character(d$path))
  }
  expect_identical(nb, 4L); expect_identical(rows, 4L)
  expect_true(all(endsWith(paths, "grid.bin")))
  # released after one batch: the producer stops, the session stays usable
  st2 <- ic_refs_arrow(s, "nat", batch_rows = 1L)
  expect_false(is.null(st2$get_next()))
  st2$release()
  expect_type(ic_commit(s, "after an abandoned stream"), "character")
  s2 <- ic_session_readonly(repo, branch = "main")
  got <- nanoarrow::convert_array_stream(ic_refs_arrow(s2, "from_bin", chunks = list(dim_0 = 0)))
  expect_true(endsWith(as.character(got$path), "grid.nc"))
  expect_error(ic_refs_arrow(s2, "no_such_array"))
})

test_that("DuckDB Arrow path serves arrays with native chunks", {
  skip_if_not_installed("duckdb"); skip_if_not_installed("arrow"); skip_if_not_installed("nanoarrow")
  skip_if_not_installed("RSQLite"); skip_if_not_installed("blob")
  fx <- nat_repo()
  dk <- DBI::dbConnect(Icechunk(), fx$path, authorize = fx$auth, sql = "duckdb", arrow = TRUE)
  sq <- DBI::dbConnect(Icechunk(), fx$path, authorize = fx$auth, sql = "sqlite")
  on.exit({ DBI::dbDisconnect(dk); DBI::dbDisconnect(sq) })
  q <- function(f) sprintf("SELECT dim_0, dim_1, %s(data) AS n FROM refs_nat ORDER BY dim_0, dim_1", f)
  a <- DBI::dbGetQuery(dk, q("octet_length"))
  expect_true("refs_nat" %in% dk@state$sql_arrow)
  expect_equal(lapply(a, as.numeric), lapply(DBI::dbGetQuery(sq, q("length")), as.numeric))
  expect_equal(as.numeric(a$n), rep(8192, 4))
})

test_that("pushdown through joins: qualified predicates, carried across dim equalities", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("blob")
  fx <- nat_repo()
  qs <- c(
    # refs_nat must not inherit a.dim_0 = 1 (it joins on a.dim_1)
    "SELECT count(*) AS n FROM refs_from_bin a JOIN refs_nat b ON a.dim_1 = b.dim_0 WHERE a.dim_0 = 1",
    # b's predicate carried to a through the ON equality
    "SELECT a.dim_0, b.dim_1 FROM refs_from_bin a JOIN refs_nat b ON a.dim_0 = b.dim_0 WHERE b.dim_0 = 1 ORDER BY 1, 2",
    "SELECT count(*) AS n FROM refs_from_bin a LEFT JOIN refs_nat b ON a.dim_0 = b.dim_1 WHERE a.dim_0 BETWEEN 1 AND 3",
    "SELECT count(*) AS n FROM refs_from_bin AS a, refs_nat AS b WHERE a.dim_0 = b.dim_1 AND b.dim_1 < 1",
    "SELECT count(*) AS n FROM refs_from_bin a JOIN sources s ON a.path = s.source WHERE a.dim_0 >= 2"
  )
  pushed <- c(1L, 2L, 2L, 2L, 1L)
  for (eng in c("sqlite", if (requireNamespace("duckdb", quietly = TRUE)) "duckdb")) {
    off <- DBI::dbConnect(Icechunk(), fx$path, authorize = fx$auth, sql = eng, pushdown = FALSE)
    for (i in seq_along(qs)) {
      # a fresh connection each time: a table loaded whole serves later queries
      on <- DBI::dbConnect(Icechunk(), fx$path, authorize = fx$auth, sql = eng)
      a <- DBI::dbGetQuery(on, qs[i])
      expect_length(on@state$last_pushdown, pushed[i])
      expect_equal(lapply(a, as.numeric), lapply(DBI::dbGetQuery(off, qs[i]), as.numeric), info = paste(eng, qs[i]))
      DBI::dbDisconnect(on)
    }
    expect_gt(as.numeric(DBI::dbGetQuery(off, qs[1])$n), 0)
    DBI::dbDisconnect(off)
  }
})

test_that("dbplyr joins push filters down to both refs tables", {
  skip_if_not_installed("dbplyr"); skip_if_not_installed("dplyr"); skip_if_not_installed("RSQLite")
  fx <- nat_repo()
  con <- DBI::dbConnect(Icechunk(), fx$path, authorize = fx$auth)
  off <- DBI::dbConnect(Icechunk(), fx$path, authorize = fx$auth, pushdown = FALSE)
  on.exit({ DBI::dbDisconnect(con); DBI::dbDisconnect(off) })
  q <- function(cn) {
    a <- dplyr::select(dplyr::tbl(cn, "refs_from_bin"), dim_0, dim_1, size)
    b <- dplyr::select(dplyr::tbl(cn, "refs_nat"), dim_0, dim_1, data)
    j <- dplyr::filter(dplyr::inner_join(a, b, by = "dim_0", suffix = c("_a", "_b")), dim_0 == 1)
    dplyr::arrange(dplyr::collect(j), dim_0, dim_1_a, dim_1_b)
  }
  got <- q(con)
  expect_length(con@state$last_pushdown, 2L)
  want <- q(off)
  expect_equal(got[c("dim_0", "dim_1_a", "dim_1_b", "size")], want[c("dim_0", "dim_1_a", "dim_1_b", "size")])
  expect_identical(nrow(got), 2L)
})

test_that("pushdown parser on joins", {
  f <- function(sql, t) .dbi_slab(sql, t, 2)
  s <- "SELECT * FROM refs_a a JOIN refs_b b ON a.dim_1 = b.dim_0 WHERE a.dim_0 = 5"
  expect_equal(f(s, "refs_a"), list(c(5, 6), c(0, Inf)))
  expect_null(f(s, "refs_b"))                                   # a's predicate is not b's
  s <- "SELECT * FROM refs_a a JOIN refs_b b ON a.dim_1 = b.dim_0 WHERE a.dim_1 BETWEEN 2 AND 4 AND b.dim_1 < 3"
  expect_equal(f(s, "refs_b"), list(c(2, 5), c(0, 3)))
  s <- "SELECT\n `refs_a`.`dim_0` AS `dim_0`\nFROM `refs_a`\nINNER JOIN `refs_b`\n ON (`refs_a`.`dim_0` = `refs_b`.`dim_0`)\nWHERE (`refs_a`.`dim_0` = 1.0)"
  expect_equal(f(s, "refs_b"), list(c(1, 2), c(0, Inf)))
  s <- 'SELECT * FROM "refs_a" AS "LHS" LEFT JOIN "refs_b" AS "RHS" ON ("LHS"."dim_0" = "RHS"."dim_0") WHERE ("RHS"."dim_0" >= 3.0)'
  expect_equal(f(s, "refs_a"), list(c(3, Inf), c(0, Inf)))
  s <- "SELECT * FROM refs_a a JOIN refs_b b ON a.dim_0 = b.dim_1 JOIN refs_c c ON b.dim_1 = c.dim_0 WHERE c.dim_0 < 2"
  expect_equal(f(s, "refs_a"), list(c(0, 2), c(0, Inf)))       # two hops
  for (sql in c("SELECT * FROM refs_a, refs_b WHERE dim_0 = 3",           # unqualified, two tables
                "SELECT * FROM refs_a x JOIN refs_a y ON x.dim_0 = y.dim_0 WHERE x.dim_0 = 1",
                "SELECT * FROM refs_a a JOIN refs_b b USING (dim_0) WHERE a.dim_0 = 1",
                "SELECT * FROM refs_a a NATURAL JOIN refs_b b WHERE a.dim_0 = 1"))
    expect_null(f(sql, "refs_a"), info = sql)
})
