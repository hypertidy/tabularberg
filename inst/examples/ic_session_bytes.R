ex <- ic_example_repo()
repo <- ic_repo_open(ex$path, authorize = ex$authorize)
s <- ic_session_writable(repo, "main")

# hand a session to another process (or package) as bytes ...
b <- ic_session_bytes(s)
b
s2 <- ic_session_from_bytes(b)
ic_set(s2, "small/c/0/0", writeBin(as.double(6:1), raw(), size = 8, endian = "little"))

# ... and merge its changes back before committing
ic_session_merge(s, ic_session_bytes(s2))
ic_status(s)
ic_commit(s, "written elsewhere")
