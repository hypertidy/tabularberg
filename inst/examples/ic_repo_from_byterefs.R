ex <- ic_example_repo()
s <- ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main")
br <- ic_to_byterefs(s)

# a new repository from the references: same chunks, same bytes
dst <- tempfile("copy-")
repo2 <- ic_repo_from_byterefs(br, dst, authorize = ex$authorize, message = "a copy")
s2 <- ic_session_readonly(repo2, branch = "main")
identical(ic_get(s2, "from_bin/c/2/0"), ic_get(s, "from_bin/c/2/0"))
ic_byterefs_compare(br, ic_to_byterefs(s2))

# with one manifest per chunk row of dimension 0
dst3 <- tempfile("split-")
ic_repo_from_byterefs(br, dst3, authorize = ex$authorize, manifest_split = c("0" = 1))
length(list.files(file.path(dst3, "manifests")))
