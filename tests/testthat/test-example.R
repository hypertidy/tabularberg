test_that("ic_example_repo builds what its docs describe", {
  ex <- ic_example_repo()
  repo <- ic_repo_open(ex$path, authorize = ex$authorize)
  s <- ic_session_readonly(repo, branch = "main")
  expect_setequal(ic_nodes(s)$path, c("/", "/from_bin", "/from_nc", "/small"))
  expect_identical(ic_tags(repo)$name, "v1")
  v <- readBin(ic_get(s, "from_bin/c/1/0"), "double", 768, size = 4, endian = "little")
  expect_equal(v, ex$values[768 + seq_len(768)])
  expect_equal(readBin(ic_get(s, "small/c/1/1"), "double", 6), c(16, 17, 18, 22, 23, 24))
  expect_error(ic_example_repo(ex$path), "already exists")
})

test_that("ic_diff takes version lists or a snapshot id", {
  ex <- ic_example_repo()
  repo <- ic_repo_open(ex$path)
  a <- ic_diff(repo, list(tag = "v1"), list(branch = "main"))
  b <- ic_diff(repo, ic_lookup(repo, tag = "v1"), ic_lookup(repo, branch = "main"))
  expect_identical(a$new_arrays, "/small")
  expect_identical(a, b)
  expect_error(ic_diff(repo, list(foo = 1), "x"), "list\\(branch")
})

test_that("ic_chunk_refs(chunks =) is the slab of the whole table", {
  ex <- ic_example_repo()
  s <- ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main")
  for (a in c("/from_bin", "/small")) {
    all <- ic_chunk_refs(s, a, inline = TRUE)
    slab <- ic_chunk_refs(s, a, inline = TRUE, chunks = list())
    expect_identical(names(slab), names(all))
    o <- match(all$key, slab$key)
    expect_false(anyNA(o))
    expect_equal(slab[o, ], all, ignore_attr = "row.names")
    one <- ic_chunk_refs(s, a, chunks = list(dim_0 = 1))
    expect_setequal(one$key, all$key[startsWith(all$coord, "1,") | all$coord == "1"])
  }
})
