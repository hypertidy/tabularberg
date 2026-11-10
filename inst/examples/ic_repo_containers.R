ex <- ic_example_repo()
repo <- ic_repo_open(ex$path)
ic_repo_containers(repo)           # declared, not yet authorised
repo <- ic_repo_open(ex$path, authorize = "file://")
ic_repo_containers(repo)           # a broader prefix covers it
