ex <- ic_example_repo()
repo <- ic_repo_open(ex$path, authorize = ex$authorize)
br <- ic_to_byterefs(ic_session_readonly(repo, branch = "main"))

# edit the table, then write it into a session of another repository
i <- which(br$arrays$array == "from_bin")
br$arrays$attrs[[i]] <- list(units = "K")
dst <- ic_repo_create(tempfile("dst-"), containers = ic_byterefs_containers(br),
                      authorize = ex$authorize)
w <- ic_session_writable(dst, "main")
ic_write_byterefs(w, br)
ic_commit(w, "from byterefs")
jsonlite::fromJSON(rawToChar(ic_get(w, "from_bin/zarr.json")))$attributes
