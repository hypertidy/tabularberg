if (requireNamespace("RSQLite", quietly = TRUE)) {
  library(DBI)
  ex <- ic_example_repo()
  con <- dbConnect(Icechunk(), ex$path, authorize = ex$authorize)
  print(con)
  print(dbListTables(con))
  print(dbGetQuery(con, 'SELECT "array", kind, shape, chunks, dtype FROM arrays'))
  # chunk references: one row per chunk; WHERE on dim_k reads only that slab
  print(dbGetQuery(con, 'SELECT dim_0, path, "offset", size FROM refs_from_bin WHERE dim_0 >= 2'))
  print(dbReadTable(con, "ancestry")[, c("snapshot_id", "message")])

  # write: append references in a transaction, which becomes one commit
  dbBegin(con)
  dbAppendTable(con, "refs_from_bin", data.frame(
    dim_0 = 3L, dim_1 = 0L, path = file.path(ex$source, "grid.bin"), offset = 0, size = 3072))
  dbCommit(con, "point the last block at the first")
  print(dbGetQuery(con, 'SELECT dim_0, "offset" FROM refs_from_bin'))

  # an earlier state, read-only
  old <- dbConnect(Icechunk(), ex$path, tag = "v1", authorize = ex$authorize)
  print(dbListTables(old))
  dbDisconnect(old); dbDisconnect(con)
}
