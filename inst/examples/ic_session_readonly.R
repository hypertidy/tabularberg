ex <- ic_example_repo()
repo <- ic_repo_open(ex$path, authorize = ex$authorize)

# read the branch head, a tag, or a snapshot
s <- ic_session_readonly(repo, branch = "main")
ic_session_info(s)
then <- ic_session_readonly(repo, tag = "v1")
ic_nodes(then)$path

# a writable session collects changes until ic_commit()
w <- ic_session_writable(repo, "main")
ic_set(w, "zarr.json", charToRaw('{"zarr_format":3,"node_type":"group","attributes":{"edited":true}}'))
ic_session_info(w)$has_uncommitted_changes
ic_discard(w)
