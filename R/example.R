#' A small example repository
#'
#' Builds a self-contained Icechunk repository in a temporary directory, for
#' examples and experiments. Nothing is downloaded. It holds:
#'
#' * `/from_bin`: float32, 64 x 48, chunks 16 x 48. Four virtual
#'   references into a raw binary file `grid.bin`, one per 16-row block.
#' * `/from_nc`: float32, 64 x 48, one chunk. A virtual reference into
#'   `grid.nc` at offset 2048 (a 2048-byte header, then the same values).
#' * `/small`: float64, 4 x 6, chunks 2 x 3, values `1:24` in C order,
#'   written through the session in a second commit. Its chunks are small
#'   enough (48 bytes) that Icechunk keeps them inline in the manifest.
#'
#' The values of the virtual arrays are `(0:3071) / 4`, little-endian
#' float32 in C order. Both source files live in a directory next to the
#' repository, declared as a virtual chunk container. Like any container,
#' it must be authorised before its chunks are read: pass `$authorize` to
#' [ic_repo_open()] or [DBI::dbConnect()]. The first commit is tagged `v1`.
#'
#' @param path directory to create (must not exist).
#' @return A list with `path` (the repository), `authorize` (the
#'   container's URL prefix, `file://...`), `source` (the directory holding
#'   `grid.bin` and `grid.nc`) and `values` (the virtual arrays' values).
#' @examples
#' ex <- ic_example_repo()
#' repo <- ic_repo_open(ex$path, authorize = ex$authorize)
#' repo
#' ic_nodes(ic_session_readonly(repo, branch = "main"))
#' @export
ic_example_repo <- function(path = tempfile("icechunk-example-")) {
  if (file.exists(path)) stop("'", path, "' already exists", call. = FALSE)
  dir.create(path, recursive = TRUE)
  src <- file.path(path, "source"); dir.create(src)
  vals <- (0:3071) / 4
  bytes <- writeBin(vals, raw(), size = 4, endian = "little")
  writeBin(bytes, file.path(src, "grid.bin"))
  writeBin(c(as.raw(rep(0x20, 2048)), bytes), file.path(src, "grid.nc"))
  src <- normalizePath(src, winslash = "/")
  prefix <- paste0("file://", src, "/")
  rp <- file.path(normalizePath(path, winslash = "/"), "repo")
  repo <- ic_repo_create(rp, containers = ic_container(prefix, "local", path = src), authorize = prefix)
  ic_repo_save_config(repo)
  s <- ic_session_writable(repo, "main")
  ic_set(s, "zarr.json", charToRaw('{"zarr_format":3,"node_type":"group","attributes":{"title":"icechunkr example"}}'))
  meta <- function(shape, chunks, dtype, fill) charToRaw(sprintf(
    '{"zarr_format":3,"node_type":"array","shape":[%s],"data_type":"%s",
      "chunk_grid":{"name":"regular","configuration":{"chunk_shape":[%s]}},
      "chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},
      "fill_value":%s,"codecs":[{"name":"bytes","configuration":{"endian":"little"}}],
      "attributes":{},"dimension_names":["y","x"]}',
    paste(shape, collapse = ","), dtype, paste(chunks, collapse = ","), fill))
  ic_set(s, "from_bin/zarr.json", meta(c(64, 48), c(16, 48), "float32", '"NaN"'))
  ic_set(s, "from_nc/zarr.json", meta(c(64, 48), c(64, 48), "float32", '"NaN"'))
  ic_set_virtual_refs(s, "/from_bin", data.frame(
    dim_0 = 0:3, dim_1 = 0L, location = paste0(prefix, "grid.bin"), offset = (0:3) * 3072, length = 3072))
  ic_set_virtual_refs(s, "/from_nc", data.frame(
    dim_0 = 0L, dim_1 = 0L, location = paste0(prefix, "grid.nc"), offset = 2048, length = 12288))
  v1 <- ic_commit(s, "two arrays of virtual references")
  ic_tag_create(repo, "v1", v1)
  s <- ic_session_writable(repo, "main")
  ic_set(s, "small/zarr.json", meta(c(4, 6), c(2, 3), "float64", "0.0"))
  m <- matrix(as.double(1:24), 4, 6, byrow = TRUE)
  for (i in 0:1) for (j in 0:1)
    ic_set(s, sprintf("small/c/%d/%d", i, j),
           writeBin(as.vector(t(m[i * 2 + 1:2, j * 3 + 1:3])), raw(), size = 8, endian = "little"))
  ic_commit(s, "a small array")
  list(path = rp, authorize = prefix, source = src, values = vals)
}
