ex <- ic_example_repo()
s <- ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main")
br <- ic_to_byterefs(s)
br$arrays[, c("array", "kind", "storage", "shape", "chunks")]
br$refs("from_bin")[, c("dim_0", "dim_1", "path", "offset", "size")]
br$sources
