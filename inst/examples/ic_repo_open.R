# create a repository, write a group, commit
path <- tempfile("ic-repo-open-")
repo <- ic_repo_create(path)
s <- ic_session_writable(repo, "main")
ic_set(s, "zarr.json", charToRaw('{"zarr_format":3,"node_type":"group","attributes":{}}'))
ic_commit(s, "an empty group")
ic_repo_exists(path)

# open it again; a repository with virtual chunk containers needs them
# authorised before their chunks are read
ex <- ic_example_repo()
repo <- ic_repo_open(ex$path, authorize = ex$authorize)
repo
ic_repo_config(repo)$virtual_chunk_containers[[1]]$url_prefix
unlink(path, recursive = TRUE)
