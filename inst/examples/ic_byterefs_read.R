ex <- ic_example_repo()
s <- ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main")
# write the store's references as a byterefs SQLite file, read it back
if (requireNamespace("RSQLite", quietly = TRUE)) {
  db <- tempfile(fileext = ".sqlite")
  ic_to_byterefs(s, dsn = db)
  br <- ic_byterefs_read(db)
  print(br)
  print(br$arrays[, c("array", "kind", "shape", "chunks", "dtype")])
  print(head(br$refs("from_bin")))
}
