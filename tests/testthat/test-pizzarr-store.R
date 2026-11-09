skip_if_no_pizzarr <- function() { skip_if_not_installed("pizzarr"); skip_if_not_installed("R6") }

test_that("pizzarr's R path reads an Icechunk session through ic_pizzarr_store()", {
  skip_if_no_pizzarr()
  v <- local_vfx()
  s <- ic_session_readonly(ic_repo_open(v$path, authorize = v$auth), branch = "main")
  st <- ic_pizzarr_store(s)
  expect_s3_class(st, "Store")
  expect_null(st$get_store_identifier())
  expect_setequal(st$listdir(""), c("from_bin", "from_nc", "zarr.json"))
  expect_false(st$contains_item("from_bin/.zarray"))
  expect_true(st$contains_item("from_bin/zarr.json"))
  expect_error(st$get_item("nope/zarr.json"), "KeyError")
  want <- matrix(v$values, 64, 48, byrow = TRUE)
  for (a in c("from_bin", "from_nc")) {
    got <- pizzarr::zarr_open(st, mode = "r", path = a)$get_item("...")$data
    expect_equal(got, want, ignore_attr = TRUE)
  }
})

test_that("pizzarr writes go into the session and commit", {
  skip_if_no_pizzarr()
  repo <- ic_repo_create(withr::local_tempdir())
  s <- ic_session_writable(repo, "main")
  st <- ic_pizzarr_store(s)
  g <- pizzarr::zarr_open_group(st, mode = "w", zarr_format = 3L)
  a <- g$create_dataset("x", shape = c(20L, 30L), chunks = c(10L, 10L), dtype = "<f8", fill_value = -1,
                        compressor = pizzarr::GzipCodec$new())
  vals <- array(as.double(seq_len(600)), c(20, 30))
  a$set_item("...", vals)
  expect_true(ic_session_info(s)$has_uncommitted_changes)
  ic_commit(s, "from pizzarr R path")
  s2 <- ic_session_readonly(repo, branch = "main")
  expect_equal(pizzarr::zarr_open(ic_pizzarr_store(s2), mode = "r", path = "x")$get_item("...")$data, vals)
  expect_equal(nrow(ic_chunk_refs(s2, "/x")), 6)
  ro <- ic_pizzarr_store(s2)
  expect_error(ro$set_item("y/zarr.json", charToRaw("{}")), "read-only")
  # mode = "w" clears through rmdir, in a new session
  s3 <- ic_session_writable(repo, "main")
  pizzarr::zarr_open_group(ic_pizzarr_store(s3), mode = "w", zarr_format = 3L)
  expect_false("x" %in% ic_pizzarr_store(s3)$listdir(""))
  expect_error(ic_pizzarr_store("not a session"), "icechunk_session")
})
