ex <- ic_example_repo()
br <- ic_to_byterefs(ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main"))
ic_byterefs_containers(br)
