ex <- ic_example_repo()
s <- ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main")
ic_nodes(s)
ic_chunk_refs(s, "/from_bin")[, c("coord", "kind", "location", "offset", "length")]
ic_chunk_refs(s, "/from_bin", chunks = list(dim_0 = 0))[, c("coord", "kind")]
ic_chunk_refs(s, inline = TRUE)[, c("path", "coord", "kind", "length")]
ic_virtual_locations(s)
