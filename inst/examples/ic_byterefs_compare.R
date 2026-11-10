ex <- ic_example_repo()
repo <- ic_repo_open(ex$path, authorize = ex$authorize)
now <- ic_to_byterefs(ic_session_readonly(repo, branch = "main"))
then <- ic_to_byterefs(ic_session_readonly(repo, tag = "v1"))
cmp <- ic_byterefs_compare(then, now)
cmp[!cmp$ok, ]
