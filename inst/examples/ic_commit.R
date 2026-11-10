ex <- ic_example_repo()
repo <- ic_repo_open(ex$path, authorize = ex$authorize)
s <- ic_session_writable(repo, "main")
ic_set(s, "zarr.json", charToRaw('{"zarr_format":3,"node_type":"group","attributes":{"v":2}}'))
ic_status(s)
id <- ic_commit(s, "attributes, take one", metadata = list(author = "example"))
ic_session_info(s)$read_only     # a committed session is done

# replace that commit instead of adding another
s <- ic_session_writable(repo, "main")
ic_set(s, "zarr.json", charToRaw('{"zarr_format":3,"node_type":"group","attributes":{"v":3}}'))
ic_amend(s, "attributes, take two")
ic_ancestry(repo, branch = "main")$message[1:2]

# two writers: the second commit conflicts, rebase onto the new head
a <- ic_session_writable(repo, "main")
b <- ic_session_writable(repo, "main")
ic_set(a, "small/c/0/0", writeBin(as.double(1:6), raw(), size = 8, endian = "little"))
ic_set(b, "small/c/1/1", writeBin(as.double(7:12), raw(), size = 8, endian = "little"))
ic_commit(a, "writer a")
res <- tryCatch(ic_commit(b, "writer b"), icechunk_conflict = function(e) e)
class(res)
ic_rebase(b)
ic_commit(b, "writer b, rebased")
