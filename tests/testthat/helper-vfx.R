# A self-contained virtual-reference repository, built with icechunkr in a
# temporary directory, shaped like the Python-written `virtual_v2` fixture:
#
#   /from_bin  float32 64 x 48, chunks 16 x 48: four virtual refs into
#              grid.bin (12288 bytes, one per 16-row block)
#   /from_nc   float32 64 x 48, one chunk: a virtual ref into grid.nc at
#              offset 2048 (a 2048-byte header, then the same values)
#
# Values are (0:3071) / 4, C order, little-endian float32. The container is
# file://<dir>/, so nothing depends on where the package is checked.
local_vfx <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  src <- file.path(d, "src"); dir.create(src)
  vals <- (0:3071) / 4
  bytes <- writeBin(vals, raw(), size = 4, endian = "little")
  writeBin(bytes, file.path(src, "grid.bin"))
  writeBin(c(as.raw(rep(0x20, 2048)), bytes), file.path(src, "grid.nc"))
  prefix <- paste0("file://", normalizePath(src), "/")
  repo <- ic_repo_create(file.path(d, "repo"), containers = ic_container(prefix, "local", path = normalizePath(src)),
                         authorize = prefix)
  ic_repo_save_config(repo)
  s <- ic_session_writable(repo, "main")
  ic_set(s, "zarr.json", charToRaw('{"zarr_format":3,"node_type":"group","attributes":{"made_by":"local_vfx"}}'))
  meta <- function(chunk_rows) charToRaw(sprintf(
    '{"zarr_format":3,"node_type":"array","shape":[64,48],"data_type":"float32",
      "chunk_grid":{"name":"regular","configuration":{"chunk_shape":[%d,48]}},
      "chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},
      "fill_value":"NaN","codecs":[{"name":"bytes","configuration":{"endian":"little"}}],
      "attributes":{},"dimension_names":["y","x"]}', chunk_rows))
  ic_set(s, "from_bin/zarr.json", meta(16L))
  ic_set(s, "from_nc/zarr.json", meta(64L))
  ic_set_virtual_refs(s, "/from_bin", data.frame(
    dim_0 = 0:3, dim_1 = 0L, location = paste0(prefix, "grid.bin"),
    offset = (0:3) * 3072, length = 3072))
  ic_set_virtual_refs(s, "/from_nc", data.frame(
    dim_0 = 0L, dim_1 = 0L, location = paste0(prefix, "grid.nc"), offset = 2048, length = 12288))
  ic_commit(s, "local_vfx")
  list(path = file.path(d, "repo"), auth = prefix, src = src, values = vals)
}
