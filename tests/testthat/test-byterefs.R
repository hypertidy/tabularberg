skip_if_no_byterefs_deps <- function() {
  skip_if_not_installed("DBI"); skip_if_not_installed("RSQLite"); skip_if_not_installed("blob")
}

test_that("exact JSON doubles", {
  x <- c(-3.4028234663852886e+38, 0.1, 1 / 3, 6378273, 298.27941112306399, -1.00000002004088e+20)
  j <- .br_json(I(x))
  expect_identical(jsonlite::fromJSON(j), x)
  expect_false(grepl("0.10000000000000001", j, fixed = TRUE))
  expect_identical(.br_json(list(a = Inf, b = NaN, c = -Inf)), '{"a":"Infinity","b":"NaN","c":"-Infinity"}')
})

test_that("codec mapping round-trips", {
  dt <- .br_dtype("<i2")
  cd <- list(compressor = list(id = "zlib", level = 1L), filters = list(list(id = "shuffle", elementsize = 2L)))
  v3 <- .br_v3_codecs(cd, dt)
  expect_identical(vapply(v3, `[[`, "", "name"), c("bytes", "numcodecs.shuffle", "numcodecs.zlib"))
  expect_equal(.br_v2_codec(v3)$codec, cd)
  z <- .br_v3_codecs(list(compressor = list(id = "zstd", level = 1L), filters = NULL), .br_dtype(">f4"))
  expect_identical(z[[1]]$configuration$endian, "big")
  expect_identical(.br_numpy_dtype("float32", .br_v2_codec(z)$endian), ">f4")
  expect_identical(.br_numpy_dtype("uint8"), "|u1")
  expect_error(.br_v3_codecs(list(compressor = list(id = "lz4")), dt), "unsupported compressor")
})

test_that("remap resolve / unresolve", {
  rules <- data.frame(prefix = c("/vsicurl/https://", "/g/data/"), replacement = c("https://", "https://h/x/"))
  p <- c("/vsicurl/https://a.b/c.nc", "/g/data/q.nc", "/local/f.tif", NA)
  r <- .br_resolve(p, rules)
  expect_identical(r, c("https://a.b/c.nc", "https://h/x/q.nc", "file:///local/f.tif", NA))
  expect_identical(.br_unresolve(r, rules[2:1, ]), c("/vsicurl/https://a.b/c.nc", "/g/data/q.nc", "/local/f.tif", NA))
  expect_error(.br_resolve("/vsis3/b/k", rules[0, ]), "still a GDAL")
})

test_that("Icechunk -> byterefs -> Icechunk -> byterefs is the identity (virtual_v2 fixture)", {
  skip_if_no_fixtures(); skip_if_no_byterefs_deps()
  fx <- manifest()$fixtures$virtual_v2
  s <- ic_session_readonly(ic_repo_open(fx_path("virtual_v2"), authorize = fx$virtual_prefix))
  b1 <- ic_to_byterefs(s)
  expect_s3_class(b1, "byterefs")
  expect_true(all(startsWith(b1$sources$source, "/")))  # file:// -> verbatim path
  db <- withr::local_tempfile(fileext = ".sqlite")
  ic_to_byterefs(s, dsn = db)
  cmp <- ic_byterefs_compare(b1, db)
  expect_true(all(cmp$ok), info = paste(cmp$array[!cmp$ok], cmp$check[!cmp$ok]))

  rp <- withr::local_tempdir()
  repo <- ic_repo_from_byterefs(db, file.path(rp, "r"))
  s2 <- ic_session_readonly(ic_repo_open(file.path(rp, "r"), authorize = fx$virtual_prefix), branch = "main")
  b2 <- ic_to_byterefs(s2)
  cmp2 <- ic_byterefs_compare(b1, b2)
  expect_true(all(cmp2$ok), info = paste(cmp2$array[!cmp2$ok], cmp2$check[!cmp2$ok]))
  # bytes agree with the original repo, through the checksum
  for (k in c("from_bin/c/0/0", "from_bin/c/3/0")) expect_identical(ic_get(s2, k), ic_get(s, k))
  expect_match(ic_ancestry(repo)$metadata[1], "byterefs")
})

test_that("a changed source is refused through its checksum", {
  skip_if_no_fixtures(); skip_if_no_byterefs_deps()
  fx <- manifest()$fixtures$virtual_v2
  src <- ic_session_readonly(ic_repo_open(fx_path("virtual_v2"), authorize = fx$virtual_prefix))
  br <- ic_to_byterefs(src)
  # point at a private copy of the source, recorded with an old mtime
  d <- withr::local_tempdir()
  f <- file.path(d, basename(br$sources$source[1]))
  file.copy(br$sources$source[1], f)
  br$sources$source[1] <- f
  rf <- br$refs
  now <- floor(as.numeric(file.mtime(f)))
  br$refs <- function(a) { r <- rf(a); r$path[!is.na(r$path)] <- f; r$mtime <- now; r }
  br$.raw$refs <- lapply(br$.raw$refs, function(r) { r$path[!is.na(r$path)] <- f; r })
  rp <- file.path(withr::local_tempdir(), "r")
  ic_repo_from_byterefs(br, rp)
  s <- ic_session_readonly(ic_repo_open(rp, authorize = paste0("file://", d, "/")), branch = "main")
  key <- ic_chunk_refs(s, "/from_bin")$key[1]
  expect_type(ic_get(s, key), "raw")
  Sys.setFileTime(f, Sys.time() + 3600)
  s <- ic_session_readonly(ic_repo_open(rp, authorize = paste0("file://", d, "/")), branch = "main")
  expect_error(ic_get(s, key), "checksum|modified")
})

test_that("raad.virtualize store round trip (if present)", {
  skip_if_no_byterefs_deps(); skip_if_not_installed("nanoparquet")
  st <- path.expand("~/Git/raad.virtualize/work/ibcso-v1-local")
  skip_if_not(file.exists(file.path(st, "store.sqlite")), "raad.virtualize work store not present")
  br <- ic_byterefs_read(file.path(st, "store.sqlite"))
  on.exit(br$close())
  rp <- file.path(withr::local_tempdir(), "r")
  repo <- ic_repo_from_byterefs(br, rp)
  expect_equal(sum(attr(repo, "summary")$refs), 2187)
  back <- ic_to_byterefs(ic_session_readonly(repo, branch = "main"))
  cmp <- ic_byterefs_compare(br, back)
  expect_true(all(cmp$ok))
  cmpf <- ic_byterefs_compare(file.path(st, "out", "flat"), back)
  expect_true(all(cmpf$ok))
})
