ex <- ic_example_repo()
s <- ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main")
ic_list(s, "small")
ic_exists(s, "small/zarr.json")
jsonlite::fromJSON(rawToChar(ic_get(s, "small/zarr.json")))$shape

# a chunk's bytes: the virtual ones come from the source file
x <- ic_get(s, "from_bin/c/1/0")
head(readBin(x, "double", 3072, size = 4, endian = "little"))
# a byte range of a chunk
readBin(ic_get(s, "from_nc/c/0/0", offset = 0, length = 8), "double", 2, size = 4)
lengths(ic_get_many(s, c("small/c/0/0", "small/c/1/1", "small/c/9/9")))

# writes go to a writable session
w <- ic_session_writable(ic_repo_open(ex$path), "main")
ic_set(w, "small/c/0/0", writeBin(as.double(101:106), raw(), size = 8, endian = "little"))
ic_delete(w, "small/c/1/1")
ic_status(w)
