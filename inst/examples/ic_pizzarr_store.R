if (requireNamespace("pizzarr", quietly = TRUE) && requireNamespace("R6", quietly = TRUE)) {
  ex <- ic_example_repo()
  s <- ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main")
  st <- ic_pizzarr_store(s)
  print(st)
  z <- pizzarr::zarr_open(st, mode = "r", path = "small")
  print(z$get_item("...")$data)
  # the virtual array, decoded from the source file's bytes
  v <- pizzarr::zarr_open(st, mode = "r", path = "from_nc")$get_item("...")$data
  all.equal(as.vector(t(v)), ex$values)
}
