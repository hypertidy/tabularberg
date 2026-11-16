ex <- ic_example_repo()
repo <- ic_repo_open(ex$path, authorize = ex$authorize)
ic_branches(repo)
ic_tags(repo)
ic_ancestry(repo, branch = "main")[, c("snapshot_id", "message")]

# a branch from the tagged snapshot, and what changed since
v1 <- ic_lookup(repo, tag = "v1")
ic_branch_create(repo, "dev", v1)
ic_diff(repo, from = list(tag = "v1"), to = list(branch = "main"))
ic_branch_delete(repo, "dev")

# a real repository: a metadata repair recorded as a commit, no references rewritten
oisst <- ic_repo_open(ic_example_oisst()$path)
ic_ancestry(oisst, tag = "cf-repaired")[, c("snapshot_id", "message")]
ic_diff(oisst, from = list(tag = "as-virtualized"), to = list(tag = "cf-repaired"))
