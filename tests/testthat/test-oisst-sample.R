# The OISST sample from zarr-icechunk-assure, checked against its own
# contract (inst/extdata/oisst-sample/probe.json). The repositories were
# written by icechunk-python 2.1.0, a writer the fixtures/ corpus lacks.

meta <- function(s, key) jsonlite::fromJSON(rawToChar(ic_get(s, key)), simplifyVector = TRUE)

# zlib, then undo the shuffle (element size 2), then little-endian int16
decode_i16 <- function(raw, n) {
  b <- memDecompress(raw, "gzip")
  b <- as.vector(t(matrix(b, nrow = length(b) %/% 2L, ncol = 2L)))
  readBin(b, "integer", n = n, size = 2, endian = "little")
}

# the minimal tier has no S3 client, so s3:// chunks cannot be fetched at all
fetchable <- function(cell) cell != "s3" || "s3_object_store" %in% ic_compiled_features()

for (cell in c("https", "s3")) {
  test_that(sprintf("OISST sample matches its contract offline [%s]", cell), {
    ox <- ic_example_oisst(cell)
    P <- ox$probe
    repo <- ic_repo_open(ox$path)
    expect_setequal(ic_tags(repo)$name, unlist(P$tags))
    s0 <- ic_session_readonly(repo, tag = "as-virtualized")
    s1 <- ic_session_readonly(repo, tag = "cf-repaired")

    sst <- meta(s1, "sst/zarr.json")
    expect_equal(sst$shape, unlist(P$array$shape))
    expect_equal(sst$chunk_grid$configuration$chunk_shape, unlist(P$array$chunks))
    expect_equal(sst$codecs$name, unlist(P$array$codecs_v3))

    # the defect and its repair: metadata only
    expect_null(meta(s0, "time/zarr.json")$attributes$units)
    expect_identical(meta(s1, "time/zarr.json")$attributes$units, P$time$units_correct)
    d <- ic_diff(repo, from = list(tag = "as-virtualized"), to = list(tag = "cf-repaired"))
    expect_identical(unlist(d$updated_arrays), "/time")

    # coordinates are native: readable with no authorisation
    expect_gt(length(ic_get(s1, "lat/c/0")), 0L)

    refs <- ic_chunk_refs(s1, "/sst")
    expect_equal(nrow(refs), P$sources$n)
    expect_true(all(startsWith(refs$location, P$cells[[cell]]$ref_prefix)))

    # an unauthorised read is refused before any network access (with no
    # S3 client compiled in, it fails for that reason instead)
    e <- tryCatch(ic_get(s1, "sst/c/0/0/0/0"), error = identity)
    expect_s3_class(e, "icechunk_error")
    if (fetchable(cell)) expect_match(conditionMessage(e), P$cells[[cell]]$ref_prefix, fixed = TRUE)
  })

  test_that(sprintf("OISST probe value read from NOAA [%s]", cell), {
    skip_on_cran()
    skip_if_not(fetchable(cell), "no S3 client in this build tier")
    skip_if_not_installed("curl")  # skip_if_offline() needs it
    skip_if_offline("www.ncei.noaa.gov")
    ox <- ic_example_oisst(cell)
    P <- ox$probe
    s <- ic_session_readonly(ic_repo_open(ox$path, authorize = ox$authorize), tag = "cf-repaired")
    ny <- P$array$shape[[3]]
    nx <- P$array$shape[[4]]
    v <- decode_i16(ic_get(s, "sst/c/0/0/0/0"), nx * ny)
    expect_identical(v[P$probe$iy_stored * nx + P$probe$ix + 1L], as.integer(P$probe$sst_packed))
  })
}

test_that("the refs the https repository was built from are shipped", {
  skip_if_not_installed("nanoparquet")
  ox <- ic_example_oisst("https")
  dir <- system.file("extdata", "oisst-sample", "refs-https", package = "tabularberg")
  pq <- nanoparquet::read_parquet(file.path(dir, "sst", "refs.0.parq"))
  pq <- pq[!is.na(pq$path), ]        # padding rows are format, not data
  refs <- ic_chunk_refs(ic_session_readonly(ic_repo_open(ox$path), tag = "cf-repaired"), "/sst")
  refs <- refs[order(as.integer(sub(",.*", "", refs$coord))), ]
  expect_identical(refs$location, pq$path)
  expect_equal(refs$offset, pq$offset)
  expect_equal(refs$length, pq$size)
})
