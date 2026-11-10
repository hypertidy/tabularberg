ex <- ic_example_repo()
repo <- ic_repo_open(ex$path, authorize = ex$authorize)
s <- ic_session_writable(repo, "main")
grid <- paste0(ex$authorize, "grid.bin")

# point from_bin's last chunk at the first block of grid.bin instead
ic_set_virtual_ref(s, "from_bin/c/3/0", grid, offset = 0, length = 3072)

# or many at once, one row per chunk (dim_* are chunk indices)
ic_set_virtual_refs(s, "/from_bin", data.frame(
  dim_0 = 0:1, dim_1 = 0L, location = grid, offset = c(3072, 0), length = 3072))
ic_chunk_refs(s, "/from_bin")[, c("coord", "offset")]
ic_commit(s, "rearranged from_bin")
