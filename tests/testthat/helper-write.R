arr_meta <- function(shape, chunks, dtype = "int32", fill = 0) {
  jsonlite::toJSON(list(
    zarr_format = 3L, node_type = "array", shape = I(as.integer(shape)), data_type = dtype,
    chunk_grid = list(name = "regular", configuration = list(chunk_shape = I(as.integer(chunks)))),
    chunk_key_encoding = list(name = "default", configuration = list(separator = "/")),
    fill_value = fill, codecs = list(list(name = "bytes", configuration = list(endian = "little")))
  ), auto_unbox = TRUE)
}
grp_meta <- function() '{"zarr_format":3,"node_type":"group","attributes":{}}'
i32 <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "little")
new_repo <- function() {
  d <- tempfile("icrepo")
  repo <- ic_repo_create(ic_storage_local(d))
  s <- ic_session_writable(repo)
  ic_set(s, "zarr.json", grp_meta())
  ic_set(s, "x/zarr.json", arr_meta(c(4, 4), c(2, 2)))
  for (i in 0:1) for (j in 0:1) ic_set(s, sprintf("x/c/%d/%d", i, j), i32(rep(i * 2 + j, 4)))
  id <- ic_commit(s, "init", metadata = list(author = "test"))
  list(repo = repo, dir = d, id = id)
}
