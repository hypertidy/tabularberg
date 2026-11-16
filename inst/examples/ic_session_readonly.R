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

# one array's metadata at two tags of a real repository
oisst <- ic_repo_open(ic_example_oisst()$path)
units <- function(tag) {
  meta <- ic_get(ic_session_readonly(oisst, tag = tag), "time/zarr.json")
  jsonlite::fromJSON(rawToChar(meta))$attributes$units
}
units("as-virtualized")
units("cf-repaired")
