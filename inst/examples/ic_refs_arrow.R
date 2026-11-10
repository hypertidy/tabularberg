ex <- ic_example_repo()
s <- ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main")
if (requireNamespace("nanoarrow", quietly = TRUE)) {
  st <- ic_refs_arrow(s, "from_bin", batch_rows = 2L)
  print(st$get_schema())
  # batches are built as they are pulled
  while (!is.null(b <- st$get_next())) print(nanoarrow::convert_array(b)[, c("dim_0", "offset", "size")])
  # one slab, with chunk bytes for inline/native chunks
  print(nanoarrow::convert_array_stream(ic_refs_arrow(s, "small", chunks = list(dim_0 = 1))))
}
