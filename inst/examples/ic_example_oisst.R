ox <- ic_example_oisst("https")
repo <- ic_repo_open(ox$path)
ic_tags(repo)

# the repair between the two tags touched metadata only
ic_diff(repo, from = list(tag = "as-virtualized"), to = list(tag = "cf-repaired"))

# coordinates are stored in the repository; data chunks point at NOAA
s <- ic_session_readonly(repo, tag = "cf-repaired")
ic_chunk_refs(s, "/sst")[1:2, c("key", "location", "length")]
\donttest{
# reading a data chunk needs the container authorised (and the network)
s <- ic_session_readonly(ic_repo_open(ox$path, authorize = ox$authorize), tag = "cf-repaired")
length(ic_get(s, "sst/c/0/0/0/0"))
}
