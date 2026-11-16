# byterefs <-> Icechunk.
#
# A byterefs table (https://github.com/byterefs/spec, byterefs/0.1) records,
# for every chunk of an array, which file holds it, at what byte range, and
# what a reader needs to decode it. An Icechunk repository holds the same
# facts plus versioning. These functions project one into the other.

# ---- reading a byterefs store -----------------------------------------------

#' Read a byterefs store
#'
#' Reads a byterefs table, either the SQLite working store (`store.sqlite`,
#' or an open DBI connection) or the Parquet "flat" export (a directory
#' holding `arrays.json`, `sources.parquet` and `refs_<array>.parquet`).
#' `byterefs/0.1` and its predecessor `mdim-refs/0.2` are accepted.
#'
#' @param x path to a `.sqlite` file, a flat-export directory, a DBI
#'   connection, or a `byterefs` object (returned as is).
#' @return a `byterefs` object: a list with `format_version`, `meta` (named
#'   list of strings), `sources`, `arrays` and `remap` data frames, and
#'   `refs(array)`, a function returning that array's global refs rows
#'   (`dim_0..`, `present`, `path`, `offset`, `size`, `info`, `data`) joined
#'   to the source's `etag`, `mtime` and `source_size`. Read from SQLite,
#'   `refs()` queries the file lazily, so the connection stays open: call
#'   `$close()` when done (functions given a file path close it themselves).
#' @example inst/examples/ic_byterefs_read.R
#' @export
ic_byterefs_read <- function(x) {
  if (inherits(x, "byterefs")) return(x)
  if (inherits(x, "DBIConnection")) return(.br_read_dbi(x))
  if (is.character(x) && length(x) == 1L && dir.exists(x)) return(.br_read_flat(x))
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    .need("DBI"); .need("RSQLite")
    con <- DBI::dbConnect(RSQLite::SQLite(), x, flags = RSQLite::SQLITE_RO, bigint = "numeric")
    br <- .br_read_dbi(con)
    br$close <- function() DBI::dbDisconnect(con)
    return(br)
  }
  stop("not a byterefs store: ", format(x), call. = FALSE)
}

.need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("package '", pkg, "' is required", call. = FALSE)
}

.BR_FORMATS <- c("byterefs/0.1", "mdim-refs/0.2")

.br_check_format <- function(v) {
  if (!isTRUE(v %in% .BR_FORMATS)) {
    stop("unsupported byterefs format_version: ", format(v), " (reads ",
         paste(.BR_FORMATS, collapse = ", "), ")", call. = FALSE)
  }
  v
}

.br_arrays_parse <- function(a) {
  js <- function(v) lapply(v, function(s) if (is.na(s) || !nzchar(s)) NULL else jsonlite::fromJSON(s, simplifyVector = TRUE))
  data.frame(
    array = a$array, kind = a$kind, storage = a$storage,
    dims = I(js(a$dims)), shape = I(js(a$shape)), chunks = I(js(a$chunks)),
    dtype = a$dtype, fill = as.character(a$fill),
    codec = I(lapply(a$codec, function(s) if (is.na(s) || !nzchar(s)) NULL else jsonlite::fromJSON(s, simplifyVector = FALSE))),
    attrs = I(lapply(a$attrs, function(s) if (is.na(s) || !nzchar(s)) list() else jsonlite::fromJSON(s, simplifyVector = FALSE))),
    affine = I(js(a$affine)),
    values = I(if (is.null(a$values)) vector("list", nrow(a)) else lapply(a$values, function(v) if (is.null(v) || all(is.na(v))) NULL else as.raw(v))),
    stringsAsFactors = FALSE
  )
}

# integer64 (RSQLite bigint default with bit64) -> double, safely
.dbl <- function(x) {
  if (inherits(x, "integer64")) { .need("bit64"); return(bit64::as.double.integer64(x)) }
  as.double(x)
}

.br_read_dbi <- function(con) {
  .need("DBI")
  tabs <- DBI::dbListTables(con)
  meta <- DBI::dbGetQuery(con, "SELECT key, value FROM meta")
  meta <- stats::setNames(as.list(meta$value), meta$key)
  sources <- DBI::dbGetQuery(con, "SELECT source_id, source, size, mtime, etag FROM sources")
  for (f in c("source_id", "size", "mtime")) sources[[f]] <- .dbl(sources[[f]])
  a <- DBI::dbGetQuery(con, 'SELECT array, kind, storage, dims, shape, chunks, dtype, fill, codec, attrs, affine, "values" FROM arrays')
  remap <- if ("remap" %in% tabs) DBI::dbGetQuery(con, "SELECT target, prefix, replacement FROM remap") else
    data.frame(target = character(), prefix = character(), replacement = character())
  arrays <- .br_arrays_parse(a)
  refs <- function(array) {
    v <- paste0("refs_", array)
    if (!v %in% tabs) stop("no refs view ", v, call. = FALSE)
    r <- DBI::dbGetQuery(con, sprintf(
      'SELECT r.*, s.etag AS etag, s.mtime AS mtime, s.size AS source_size
         FROM %s r LEFT JOIN sources s ON s.source = r.path',
      DBI::dbQuoteIdentifier(con, v)))
    for (f in c(grep("^dim_", names(r), value = TRUE), "offset", "size", "mtime", "source_size")) r[[f]] <- .dbl(r[[f]])
    # an all-NULL column comes back logical (e.g. path, for inline chunks)
    for (f in intersect(c("path", "info", "etag"), names(r))) r[[f]] <- as.character(r[[f]])
    r
  }
  structure(list(format_version = .br_check_format(meta$format_version), meta = meta,
                 sources = sources, arrays = arrays, remap = remap, refs = refs),
            class = "byterefs")
}

.br_read_flat <- function(dir) {
  .need("nanoparquet")
  doc <- jsonlite::read_json(file.path(dir, "arrays.json"), simplifyVector = FALSE)
  .br_check_format(doc$format_version)
  meta <- lapply(doc$meta, function(v) if (is.character(v)) v else as.character(jsonlite::toJSON(v, auto_unbox = TRUE)))
  meta$format_version <- doc$format_version
  if (!is.null(doc$dimensions) && is.null(meta$dimensions)) {
    meta$dimensions <- as.character(jsonlite::toJSON(doc$dimensions, auto_unbox = TRUE))
  }
  sources <- as.data.frame(nanoparquet::read_parquet(file.path(dir, "sources.parquet")))
  tojs <- function(v) if (is.null(v)) NA_character_ else .br_json(v)
  a <- data.frame(
    array = vapply(doc$arrays, `[[`, "", "array"),
    kind = vapply(doc$arrays, `[[`, "", "kind"),
    storage = vapply(doc$arrays, `[[`, "", "storage"),
    dims = vapply(doc$arrays, function(r) tojs(r$dims), ""),
    shape = vapply(doc$arrays, function(r) tojs(r$shape), ""),
    chunks = vapply(doc$arrays, function(r) tojs(r$chunks), ""),
    dtype = vapply(doc$arrays, function(r) r$dtype %||% NA_character_, ""),
    fill = vapply(doc$arrays, function(r) if (is.null(r$fill)) NA_character_ else as.character(r$fill), ""),
    codec = vapply(doc$arrays, function(r) tojs(r$codec), ""),
    attrs = vapply(doc$arrays, function(r) tojs(r$attrs), ""),
    affine = vapply(doc$arrays, function(r) tojs(r$affine), ""),
    stringsAsFactors = FALSE)
  # flat export stores inline coordinate values as a JSON list of numbers
  a$values <- lapply(doc$arrays, function(r) {
    if (is.null(r$values)) return(NULL)
    .br_pack(unlist(r$values), r$dtype)
  })
  arrays <- .br_arrays_parse(a)
  remap <- if (length(doc$remap)) do.call(rbind, lapply(doc$remap, as.data.frame)) else
    data.frame(target = character(), prefix = character(), replacement = character())
  refs <- function(array) {
    r <- as.data.frame(nanoparquet::read_parquet(file.path(dir, paste0("refs_", array, ".parquet"))))
    s <- sources[, c("source", "etag", "mtime", "size")]
    names(s) <- c("path", "etag", "mtime", "source_size")
    m <- match(r$path, s$path)
    r$etag <- s$etag[m]; r$mtime <- s$mtime[m]; r$source_size <- s$source_size[m]
    r
  }
  structure(list(format_version = doc$format_version, meta = meta, sources = sources,
                 arrays = arrays, remap = remap, refs = refs), class = "byterefs")
}

#' @export
print.byterefs <- function(x, ...) {
  cat("<byterefs> ", x$format_version, " ", x$meta$dataset %||% "", "\n", sep = "")
  cat("  sources: ", nrow(x$sources), "\n", sep = "")
  for (i in seq_len(nrow(x$arrays))) {
    a <- x$arrays[i, ]
    cat(sprintf("  %-20s %-5s %-10s %s %s\n", a$array, a$kind, a$storage, a$dtype,
                paste(unlist(a$shape), collapse = "x")))
  }
  if (nrow(x$remap)) cat("  remap targets: ", paste(unique(x$remap$target), collapse = ", "), "\n", sep = "")
  invisible(x)
}

# ---- JSON with exact doubles --------------------------------------------------

# Shortest decimal that round-trips a double (byterefs decision 0010).
.num <- function(x) {
  vapply(x, function(v) {
    if (is.na(v) && !is.nan(v)) return("null")
    if (is.nan(v)) return('"NaN"')
    if (is.infinite(v)) return(if (v > 0) '"Infinity"' else '"-Infinity"')
    if (v == round(v) && abs(v) < 2^53) return(format(v, scientific = FALSE, trim = TRUE))
    for (d in 15:17) {
      s <- formatC(v, digits = d, format = "g")
      if (as.numeric(s) == v) return(s)
    }
    formatC(v, digits = 17, format = "g")
  }, "")
}

# Mark doubles as verbatim JSON so toJSON keeps full precision.
.jsonify <- function(x) {
  if (is.list(x)) {
    out <- lapply(x, .jsonify)
    attributes(out) <- attributes(x)
    return(out)
  }
  if (is.double(x) && !inherits(x, "AsIs")) {
    j <- .num(x)
    if (length(x) == 1L && is.null(attr(x, "keep_array"))) return(structure(j, class = "json"))
    return(structure(paste0("[", paste(j, collapse = ","), "]"), class = "json"))
  }
  if (inherits(x, "AsIs")) {
    v <- unclass(x)
    if (is.double(v)) return(structure(paste0("[", paste(.num(v), collapse = ","), "]"), class = "json"))
    return(as.list(v))
  }
  x
}

.br_json <- function(x, pretty = FALSE) {
  as.character(jsonlite::toJSON(.jsonify(x), auto_unbox = TRUE, null = "null",
                                json_verbatim = TRUE, pretty = pretty, digits = NA))
}

# ---- dtypes, fill values, codecs ------------------------------------------------

.DT <- c(b1 = "bool", i1 = "int8", u1 = "uint8", i2 = "int16", u2 = "uint16",
         i4 = "int32", u4 = "uint32", i8 = "int64", u8 = "uint64",
         f2 = "float16", f4 = "float32", f8 = "float64")

.br_dtype <- function(dtype) {
  code <- substr(dtype, 2, nchar(dtype))
  v3 <- .DT[[code]]
  if (is.null(v3)) stop("unsupported dtype: ", dtype, call. = FALSE)
  list(data_type = v3, endian = if (substr(dtype, 1, 1) == ">") "big" else "little",
       size = as.integer(substr(code, 2, 9)), kind = substr(code, 1, 1))
}

.br_numpy_dtype <- function(data_type, endian = "little") {
  code <- names(.DT)[match(data_type, .DT)]
  if (is.na(code)) stop("unsupported data_type: ", data_type, call. = FALSE)
  size <- as.integer(substr(code, 2, 9))
  paste0(if (size == 1L) "|" else if (identical(endian, "big")) ">" else "<", code)
}

# byterefs fill token -> v3 fill_value (verbatim JSON)
.br_fill_json <- function(fill, dt) {
  if (is.null(fill) || is.na(fill) || !nzchar(fill)) {
    return(if (dt$kind == "b") "false" else if (dt$kind == "f") "0.0" else "0")
  }
  f <- tolower(fill)
  if (dt$kind == "f") {
    if (f %in% c("nan")) return('"NaN"')
    if (f %in% c("inf", "infinity", "+inf")) return('"Infinity"')
    if (f %in% c("-inf", "-infinity")) return('"-Infinity"')
    return(.num(as.numeric(fill)))
  }
  if (dt$kind == "b") return(if (f %in% c("1", "true")) "true" else "false")
  format(as.numeric(fill), scientific = FALSE, trim = TRUE)
}

# v3 fill_value -> byterefs fill token
.br_fill_token <- function(fv) {
  if (is.null(fv)) return(NA_character_)
  if (is.character(fv)) return(switch(fv, `NaN` = "NaN", Infinity = "Inf", `-Infinity` = "-Inf", fv))
  if (is.logical(fv)) return(if (fv) "1" else "0")
  .num(fv)
}

.br_fill_value <- function(fill) {
  if (is.null(fill) || is.na(fill)) return(NULL)
  f <- tolower(fill)
  if (f == "nan") return(NaN)
  if (f %in% c("inf", "infinity")) return(Inf)
  if (f %in% c("-inf", "-infinity")) return(-Inf)
  as.numeric(fill)
}

# byterefs codec (v2 {compressor, filters}, or a v3 chain) -> v3 codecs list
.br_v3_codecs <- function(codec, dt, array = "") {
  bytes <- if (dt$size == 1L) list(name = "bytes") else
    list(name = "bytes", configuration = list(endian = dt$endian))
  if (is.null(codec)) return(list(bytes))
  if (is.null(names(codec))) return(codec)  # already a v3 chain (decision 0007)
  pre <- list(); post <- list()
  for (f in codec$filters) {
    id <- f$id
    if (id == "shuffle") {
      post[[length(post) + 1L]] <- list(name = "numcodecs.shuffle",
        configuration = list(elementsize = as.integer(f$elementsize %||% dt$size)))
    } else if (id == "imagecodecs_delta") {
      pre[[length(pre) + 1L]] <- list(name = "imagecodecs_delta",
        configuration = list(axis = as.integer(f$axis %||% -1L), dist = as.integer(f$dist %||% 1L)))
    } else {
      stop(array, ": unsupported filter '", id, "'", call. = FALSE)
    }
  }
  c <- codec$compressor
  if (!is.null(c)) {
    lvl <- as.integer(c$level %||% 1L)
    post[[length(post) + 1L]] <- switch(c$id,
      zstd = list(name = "zstd", configuration = list(level = lvl, checksum = FALSE)),
      zlib = list(name = "numcodecs.zlib", configuration = list(level = lvl)),
      gzip = list(name = "gzip", configuration = list(level = lvl)),
      stop(array, ": unsupported compressor '", c$id, "'", call. = FALSE))
  }
  c(pre, list(bytes), post)
}

# v3 codecs -> byterefs v2 codec {compressor, filters} (NULL if bytes only)
.br_v2_codec <- function(codecs) {
  filters <- list(); compressor <- NULL; endian <- "little"; seen <- FALSE
  for (cd in codecs) {
    cfg <- cd$configuration
    nm <- sub("^numcodecs[.]", "", cd$name)
    if (nm == "bytes") { endian <- cfg$endian %||% "little"; seen <- TRUE; next }
    if (nm == "shuffle") filters[[length(filters) + 1L]] <- list(id = "shuffle", elementsize = cfg$elementsize)
    else if (nm == "imagecodecs_delta") filters[[length(filters) + 1L]] <- list(id = "imagecodecs_delta", axis = cfg$axis, dist = cfg$dist)
    else if (nm %in% c("zstd", "zlib", "gzip")) compressor <- list(id = nm, level = cfg$level)
    else stop("codec '", cd$name, "' has no byterefs v2 form", call. = FALSE)
  }
  list(codec = if (is.null(compressor) && !length(filters)) NULL else
         list(compressor = compressor, filters = if (length(filters)) filters else NULL),
       endian = endian)
}

.br_pack <- function(v, dtype) {
  dt <- .br_dtype(dtype)
  if (dt$kind == "f") writeBin(as.double(v), raw(), size = dt$size, endian = dt$endian)
  else writeBin(as.integer(v), raw(), size = dt$size, endian = dt$endian)
}

.br_unpack <- function(b, dtype, n) {
  dt <- .br_dtype(dtype)
  if (dt$kind == "f") readBin(b, "double", n, size = dt$size, endian = dt$endian)
  else readBin(b, "integer", n, size = dt$size, signed = dt$kind == "i" || dt$size >= 4, endian = dt$endian)
}

# ---- remap and containers --------------------------------------------------------

.br_rules <- function(br, target) {
  r <- br$remap[br$remap$target == target, c("prefix", "replacement"), drop = FALSE]
  r[order(-nchar(r$prefix)), , drop = FALSE]
}

.br_target <- function(br, target) {
  if (!is.null(target)) {
    if (!target %in% br$remap$target && nrow(br$remap)) {
      stop("no remap rules for target '", target, "'", call. = FALSE)
    }
    return(target)
  }
  br$meta$default_target %||% (if ("public" %in% br$remap$target) "public" else NA_character_)
}

.br_resolve <- function(path, rules) {
  out <- path
  done <- rep(FALSE, length(path))
  for (i in seq_len(nrow(rules))) {
    hit <- !done & !is.na(out) & startsWith(out, rules$prefix[i])
    out[hit] <- paste0(rules$replacement[i], substring(out[hit], nchar(rules$prefix[i]) + 1L))
    done <- done | hit
  }
  bad <- !is.na(out) & startsWith(out, "/vsi")
  if (any(bad)) stop(out[bad][1], ": still a GDAL /vsi path after remap; choose a target that gives a URL", call. = FALSE)
  loc <- !is.na(out) & startsWith(out, "/")
  out[loc] <- paste0("file://", out[loc])
  .file_url(out)
}

.br_unresolve <- function(loc, rules) {
  out <- loc
  done <- rep(FALSE, length(loc))
  rules <- rules[order(-nchar(rules$replacement)), , drop = FALSE]
  for (i in seq_len(nrow(rules))) {
    hit <- !done & !is.na(out) & startsWith(out, rules$replacement[i])
    out[hit] <- paste0(rules$prefix[i], substring(out[hit], nchar(rules$replacement[i]) + 1L))
    done <- done | hit
  }
  f <- !done & !is.na(out) & startsWith(out, "file:///")
  out[f] <- substring(out[f], 8L)
  out
}

.br_container_prefix <- function(url) {
  scheme <- sub(":.*$", "", url)
  switch(scheme,
    http = , https = sub("^(https?://[^/]+/).*$", "\\1", url),
    s3 = , gs = , gcs = sub("^([a-z0-9]+://[^/]+/).*$", "\\1", url),
    file = paste0(dirname(.file_url(url)), "/"),
    stop(url, ": no virtual chunk container for scheme '", scheme, "'", call. = FALSE))
}

#' Virtual chunk containers needed by a byterefs store
#'
#' One container per scheme and host (http/https), bucket (s3, gcs), or
#' directory (file), as the locations resolve under `target`.
#'
#' @param br a byterefs store (see [ic_byterefs_read()]).
#' @param target remap target; default `meta.default_target`, else
#'   `"public"`, else no remapping.
#' @param region S3 region for s3 containers.
#' @return a list of [ic_container()] objects; the `prefixes` attribute
#'   holds the URL prefixes, suitable for `authorize`.
#' @example inst/examples/ic_byterefs_containers.R
#' @export
ic_byterefs_containers <- function(br, target = NULL, region = Sys.getenv("AWS_REGION", "us-east-1")) {
  opened <- !inherits(br, "byterefs")
  br <- ic_byterefs_read(br)
  if (opened && !is.null(br$close)) on.exit(br$close(), add = TRUE)
  target <- .br_target(br, target)
  rules <- .br_rules(br, target)
  paths <- unique(br$sources$source)
  locs <- unique(.br_resolve(paths, rules))
  pre <- sort(unique(vapply(locs, .br_container_prefix, "")))
  out <- lapply(pre, function(p) {
    switch(sub(":.*$", "", p),
      http = , https = ic_container(p, "http"),
      s3 = ic_container(p, "s3", region = region),
      gs = , gcs = ic_container(p, "gcs"),
      file = ic_container(p, "local", path = substring(p, 8L)))
  })
  attr(out, "prefixes") <- pre
  out
}

# ---- byterefs -> Icechunk ---------------------------------------------------------

.br_array_meta <- function(shape, chunks, dtype, fill, codecs, attrs, dims) {
  dt <- .br_dtype(dtype)
  m <- list(
    zarr_format = 3L, node_type = "array",
    shape = I(as.integer(shape)), data_type = dt$data_type,
    chunk_grid = list(name = "regular", configuration = list(chunk_shape = I(as.integer(chunks)))),
    chunk_key_encoding = list(name = "default", configuration = list(separator = "/")),
    fill_value = structure(.br_fill_json(fill, dt), class = "json"),
    codecs = codecs,
    attributes = if (length(attrs)) attrs else structure(list(), names = character()),
    dimension_names = I(as.character(unlist(dims))),
    storage_transformers = I(list())
  )
  charToRaw(.br_json(m, pretty = TRUE))
}

# xarray masks on _FillValue; a non-finite one is left out (it needs no mask
# and some writers reject it), as py/render_icechunk.py does.
# Only an existing _FillValue is touched: a finite one is written as the
# array's fill, a non-finite one is dropped (and recorded, see
# .br_fill_dropped, so the projection can restore it). None is invented.
.br_attrs_out <- function(attrs, fill) {
  if (is.null(attrs$`_FillValue`)) return(attrs)
  attrs$`_FillValue` <- NULL
  f <- .br_fill_value(fill)
  if (!is.null(f) && is.finite(f)) attrs$`_FillValue` <- f
  attrs
}

# arrays whose non-finite _FillValue .br_attrs_out drops
.br_fill_dropped <- function(arrays) {
  keep <- vapply(seq_len(nrow(arrays)), function(i) {
    a <- arrays[i, ]
    f <- .br_fill_value(a$fill)
    a$storage == "referenced" && !is.null(a$attrs[[1]]$`_FillValue`) && !is.null(f) && !is.finite(f)
  }, logical(1))
  as.character(arrays$array[keep])
}

#' Write a byterefs store into an Icechunk session
#'
#' Every present chunk becomes a virtual reference (location, offset,
#' length) to the source bytes, with the source's `etag` (or `mtime`) as
#' the checksum, so Icechunk refuses chunks whose source has changed.
#' (Icechunk checks "unmodified since"; for `file://` locations the
#' recorded time is `mtime + 1`, because local filesystems keep sub-second
#' mtimes and byterefs records whole seconds.)
#' Chunks with an inline `data` blob are written as native chunks, absent
#' chunks are left unwritten (they read as the fill value), and coordinate
#' arrays (`affine`, `inline`) are materialised as single native chunks.
#' The root group's attributes hold `meta`, plus a `byterefs` entry with
#' the remap rules and target, so [ic_to_byterefs()] can recover the
#' verbatim source paths.
#'
#' Arrays become Zarr v3: dtype and byte order from `dtype`, `bytes` then
#' `numcodecs.shuffle` then `zstd` / `numcodecs.zlib` / `gzip` from `codec`,
#' with `imagecodecs_delta` (TIFF predictor 2) ahead of `bytes`.
#'
#' @param session a writable `icechunk_session`.
#' @param br a byterefs store (see [ic_byterefs_read()]).
#' @param target remap target (see [ic_byterefs_containers()]).
#' @param checksums record `etag` / `mtime` checksums on virtual refs.
#' @param validate check every location falls within a declared container.
#' @return a data frame, one row per array: `array`, `refs` (virtual),
#'   `native` (inline or coordinate chunks), `absent`.
#' @example inst/examples/ic_write_byterefs.R
#' @export
ic_write_byterefs <- function(session, br, target = NULL, checksums = TRUE, validate = TRUE) {
  opened <- !inherits(br, "byterefs")
  br <- ic_byterefs_read(br)
  if (opened && !is.null(br$close)) on.exit(br$close(), add = TRUE)
  target <- .br_target(br, target)
  rules <- .br_rules(br, target)
  root <- c(br$meta, list(byterefs = list(
    target = target %||% NA_character_,
    file_mtime_offset = if (checksums) 1L else 0L,
    dropped_fill_value = I(.br_fill_dropped(br$arrays)),
    remap = if (nrow(br$remap)) unname(split(br$remap, seq_len(nrow(br$remap)))) |>
      lapply(as.list) else list())))
  .br_write_root(session, root)
  out <- list()
  for (i in seq_len(nrow(br$arrays))) {
    a <- br$arrays[i, ]
    .br_write_array_meta(session, a)
    out[[a$array]] <- if (a$storage == "referenced") {
      .br_write_refs(session, a$array, br$refs(a$array), rules, checksums, validate)
    } else data.frame(array = a$array, refs = 0L, native = 1L, absent = 0L)
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

.br_write_root <- function(session, attrs) {
  ic_set(session, "zarr.json", charToRaw(.br_json(list(
    zarr_format = 3L, node_type = "group", attributes = attrs), pretty = TRUE)))
}

# one parsed arrays row -> zarr.json (and the coordinate chunk for affine/inline)
.br_write_array_meta <- function(session, a) {
  nm <- a$array
  dims <- a$dims[[1]]; shape <- a$shape[[1]]
  if (a$storage %in% c("affine", "inline")) {
    b <- if (a$storage == "affine") {
      af <- a$affine[[1]]
      .br_pack(af$start + af$step * (seq_len(shape[1]) - 1), a$dtype)
    } else a$values[[1]]
    ic_set(session, paste0(nm, "/zarr.json"),
           .br_array_meta(shape, shape, a$dtype, NA, .br_v3_codecs(NULL, .br_dtype(a$dtype)),
                          a$attrs[[1]], dims))
    ic_set(session, paste0(nm, "/c/", paste(rep(0L, length(shape)), collapse = "/")), b)
    return(invisible(NULL))
  }
  if (a$storage != "referenced") stop(nm, ": unknown storage '", a$storage, "'", call. = FALSE)
  ic_set(session, paste0(nm, "/zarr.json"),
         .br_array_meta(shape, a$chunks[[1]], a$dtype, a$fill,
                        .br_v3_codecs(a$codec[[1]], .br_dtype(a$dtype), nm),
                        .br_attrs_out(a$attrs[[1]], a$fill), dims))
  invisible(NULL)
}

# refs rows (dim_*, present, path, offset, size, data, [etag, mtime, source_size])
# -> virtual refs and native chunks
.br_write_refs <- function(session, nm, r, rules, checksums = TRUE, validate = TRUE) {
  if (is.null(r$present)) r$present <- 1L
  if (is.null(r$data)) r$data <- vector("list", nrow(r))
  present <- r$present == 1L
  inl <- present & vapply(r$data, function(d) length(d) > 0L, logical(1))
  virt <- present & !inl & !is.na(r$path)
  v <- r[virt, , drop = FALSE]
  # decision 0008: size NULL with offset 0 is the whole object
  whole <- is.na(v$size)
  if (any(whole)) {
    if (is.null(v$source_size) || any(is.na(v$source_size[whole]))) stop(nm, ": whole-object refs without a source size", call. = FALSE)
    v$size[whole] <- v$source_size[whole]
  }
  refs <- v[, grep("^dim_", names(v)), drop = FALSE]
  refs$location <- .br_resolve(v$path, rules)
  refs$offset <- as.double(v$offset)
  refs$length <- as.double(v$size)
  if (checksums) {
    refs$etag <- v$etag %||% rep(NA_character_, nrow(v))
    # Icechunk checks "unmodified since <seconds>"; for local files
    # object_store compares against the full sub-second mtime, so a
    # truncated-seconds mtime always fails. Record the ceiling instead.
    lm <- as.double(v$mtime %||% rep(NA_real_, nrow(v)))
    f <- startsWith(refs$location, "file://")
    lm[f] <- lm[f] + 1
    refs$last_modified <- lm
  }
  nfail <- if (nrow(refs)) ic_set_virtual_refs(session, paste0("/", nm), refs, validate = validate) else 0L
  if (nfail) stop(nm, ": ", nfail, " refs rejected (location outside declared containers?)", call. = FALSE)
  dcols <- grep("^dim_", names(r), value = TRUE)
  for (j in which(inl)) {
    idx <- vapply(dcols, function(d) r[[d]][j], 0)
    ic_set(session, paste0(nm, "/c/", paste(as.integer(idx), collapse = "/")), r$data[[j]])
  }
  data.frame(array = nm, refs = nrow(refs), native = sum(inl), absent = sum(!present))
}

#' Create an Icechunk repository from a byterefs store
#'
#' Creates the repository with one virtual chunk container per source
#' prefix ([ic_byterefs_containers()]), authorises them, writes the store
#' with [ic_write_byterefs()] and commits.
#'
#' @param br a byterefs store (see [ic_byterefs_read()]).
#' @param storage an [ic_storage] object, or a local path, for the new repository.
#' @param target remap target.
#' @param message commit message.
#' @param authorize credentials for the containers; default inferred from
#'   each prefix's scheme (see [ic_repo_open()]).
#' @param manifest_split manifest splitting, see [ic_repo_create()]; e.g.
#'   `c(time = 1)` for a daily series, so a day's refs are one manifest.
#' @param ... passed to [ic_write_byterefs()].
#' @return the `icechunk_repo` (invisibly), with attributes `snapshot`
#'   and `summary`.
#' @example inst/examples/ic_repo_from_byterefs.R
#' @export
ic_repo_from_byterefs <- function(br, storage, target = NULL, message = NULL, authorize = NULL,
                                  manifest_split = NULL, ...) {
  opened <- !inherits(br, "byterefs")
  br <- ic_byterefs_read(br)
  if (opened && !is.null(br$close)) on.exit(br$close(), add = TRUE)
  target <- .br_target(br, target)
  cont <- ic_byterefs_containers(br, target)
  auth <- authorize %||% attr(cont, "prefixes")
  repo <- ic_repo_create(storage, containers = cont, authorize = auth, manifest_split = manifest_split)
  ic_repo_save_config(repo)
  s <- ic_session_writable(repo, "main")
  summ <- ic_write_byterefs(s, br, target = target, ...)
  msg <- message %||% sprintf("byterefs %s %s, target %s", br$format_version,
                              br$meta$dataset %||% "", target %||% "none")
  snap <- ic_commit(s, msg, metadata = list(byterefs = list(
    format_version = br$format_version, dataset = br$meta$dataset %||% NA,
    target = target %||% NA, refs = sum(summ$refs))))
  attr(repo, "snapshot") <- snap
  attr(repo, "summary") <- summ
  invisible(repo)
}

# ---- Icechunk -> byterefs ---------------------------------------------------------

# root group attributes -> meta, remap rules and the byterefs bookkeeping
.br_root <- function(session) {
  zj <- ic_get(session, "zarr.json")                 # absent in a new, empty repository
  root <- if (is.null(zj)) list() else jsonlite::fromJSON(rawToChar(zj), simplifyVector = FALSE)
  ra <- root$attributes %||% list()
  brx <- ra$byterefs
  ra$byterefs <- NULL
  meta <- lapply(ra, function(v) if (is.character(v) && length(v) == 1L) v else .br_json(v))
  meta$format_version <- "byterefs/0.1"
  meta$icechunk_snapshot <- ic_session_info(session)$snapshot_id
  remap <- if (length(brx$remap)) do.call(rbind, lapply(brx$remap, as.data.frame)) else
    data.frame(target = character(), prefix = character(), replacement = character())
  target <- brx$target
  rules <- if (!is.null(target) && !is.na(target)) .br_rules(list(remap = remap), target) else remap[0, c("prefix", "replacement")]

  list(meta = meta, remap = remap, rules = rules, brx = brx)
}

# one array -> list(arow = arrays row (list), refs = refs data frame or NULL)
.br_array <- function(session, p, root, native = TRUE, ranges = NULL) {
  rules <- root$rules; brx <- root$brx
  arrs <- list(); refs <- list()
    nm <- sub("^/", "", p)
    zj <- jsonlite::fromJSON(rawToChar(ic_get(session, paste0(nm, "/zarr.json"))), simplifyVector = FALSE)
    v2 <- .br_v2_codec(zj$codecs)
    dtype <- .br_numpy_dtype(zj$data_type, v2$endian)
    shape <- as.integer(unlist(zj$shape))
    chunks <- as.integer(unlist(zj$chunk_grid$configuration$chunk_shape))
    dims <- unlist(zj$dimension_names) %||% paste0("dim_", seq_along(shape) - 1L)
    fill <- .br_fill_token(zj$fill_value)
    attrs <- zj$attributes %||% list()
    cr <- .ic(ic_refs_columns_(session$ptr, p, ranges))
    ncr <- length(cr$kind)
    one_chunk <- identical(shape, chunks) && length(shape) == 1L
    coordlike <- one_chunk && ncr == 1L && cr$kind != "virtual" && nm %in% dims
    # a compressed coordinate chunk is not decoded here: it is kept as a
    # referenced coordinate array whose one chunk travels as a data blob
    # with its codec (lossless; written back as a native chunk)
    if (coordlike && !is.null(v2$codec)) coordlike <- FALSE
    is_coord <- one_chunk && nm %in% dims
    if (coordlike) {
      b <- if (cr$kind == "inline") cr$data[[1]] else ic_get(session, cr$key)
      vals <- .br_unpack(b, dtype, shape[1])
      d <- diff(vals)
      aff <- length(vals) > 1L && all(d == d[1]) &&
        isTRUE(all(vals[1] + d[1] * (seq_along(vals) - 1) == vals))
      arrs[[nm]] <- list(array = nm, kind = "coord",
                         storage = if (aff) "affine" else "inline",
                         dims = .br_json(I(dims)), shape = .br_json(I(shape)), chunks = .br_json(I(chunks)),
                         dtype = dtype, fill = NA_character_, codec = NA_character_,
                         attrs = .br_json(if (length(attrs)) attrs else structure(list(), names = character())),
                         affine = if (aff) .br_json(list(start = vals[1], step = d[1])) else NA_character_,
                         values = if (aff) list(NULL) else list(b))
      return(list(arow = arrs[[nm]], refs = NULL))
    }
    # restore a non-finite _FillValue that ic_write_byterefs left out
    fv <- .br_fill_value(fill)
    if (is.null(attrs$`_FillValue`) && !is.null(fv) && !is.finite(fv) &&
        nm %in% unlist(brx$dropped_fill_value)) {
      attrs$`_FillValue` <- if (is.nan(fv)) "NaN" else if (fv > 0) "Infinity" else "-Infinity"
    }
    arrs[[nm]] <- list(array = nm, kind = if (is_coord) "coord" else "data", storage = "referenced",
                       dims = .br_json(I(dims)), shape = .br_json(I(shape)), chunks = .br_json(I(chunks)),
                       dtype = dtype, fill = fill,
                       codec = if (is.null(v2$codec)) NA_character_ else .br_json(v2$codec),
                       attrs = .br_json(if (length(attrs)) attrs else structure(list(), names = character())),
                       affine = NA_character_, values = list(NULL))
    if (!ncr) return(list(arow = arrs[[nm]], refs = NULL))
    rr <- stats::setNames(cr$dims, paste0("dim_", seq_along(cr$dims) - 1L))
    isv <- cr$kind == "virtual"
    path <- rep(NA_character_, ncr)
    ul <- unique(cr$location[isv])
    path[isv] <- .br_unresolve(ul, rules)[match(cr$location[isv], ul)]
    rr$present <- rep(1L, ncr)
    rr$path <- path
    rr$offset <- ifelse(isv, cr$offset, NA_real_)
    rr$size <- ifelse(isv, cr$length, NA_real_)
    rr$info <- rep(NA_character_, ncr)
    data <- cr$data
    nat <- which(cr$kind == "native")
    if (native && length(nat)) data[nat] <- ic_get_many(session, cr$key[nat])
    rr$data <- data
    rr$etag <- cr$etag
    rr$mtime <- cr$last_modified
    rr$mtime[is.nan(rr$mtime)] <- NA_real_
    off <- brx$file_mtime_offset %||% 0L
    if (off) {
      f <- isv & startsWith(cr$location, "file://")
      rr$mtime[f] <- rr$mtime[f] - off
    }
    rr <- structure(rr, class = "data.frame", row.names = .set_row_names(ncr))
    refs[[nm]] <- rr
  list(arow = arrs[[nm]], refs = refs[[nm]])
}

#' Project an Icechunk session to a byterefs store
#'
#' The inverse of [ic_write_byterefs()], and the "chunk references as a
#' table" view of any Icechunk snapshot. Each array becomes an `arrays` row
#' (dtype, fill, codec and attributes from its `zarr.json`); virtual chunks
#' become refs rows with their source path, offset and size; inline and
#' native chunks become rows with a `data` blob (native chunk bytes are
#' fetched only if `native = TRUE`). Distinct locations become `sources`,
#' with `etag` / `mtime` from the chunk checksums.
#'
#' What Icechunk does not hold is reconstructed or left empty: source
#' `size` is `NA`, and `placements` are synthesised so the global refs view
#' is the identity (one placement per source, covering its chunks). If the
#' repository was written by [ic_write_byterefs()], the root group's
#' `byterefs` entry restores the `meta`, the remap rules and the verbatim
#' source paths.
#'
#' @param session an `icechunk_session`.
#' @param dsn optional path of a SQLite file to write (requires DBI and
#'   RSQLite); it must not exist.
#' @param native fetch native chunk bytes into `data` (else those rows have
#'   `present = 1` and no location; a byterefs reader cannot use them).
#' @return a `byterefs` object (invisibly if `dsn` is given).
#' @example inst/examples/ic_to_byterefs.R
#' @export
ic_to_byterefs <- function(session, dsn = NULL, native = TRUE) {
  root <- .br_root(session)
  meta <- root$meta; remap <- root$remap
  nodes <- ic_nodes(session)
  nodes <- nodes[nodes$type == "array", , drop = FALSE]
  arrs <- list(); refs <- list()
  for (p in nodes$path) {
    x <- .br_array(session, p, root, native = native)
    nm <- x$arow$array
    arrs[[nm]] <- x$arow
    if (!is.null(x$refs)) refs[[nm]] <- x$refs
  }
  arrays <- do.call(rbind, lapply(arrs, function(x) {
    v <- x$values; x$values <- NULL
    d <- as.data.frame(x, stringsAsFactors = FALSE); d$values <- v; d
  }))
  rownames(arrays) <- NULL

  # sources: distinct paths across arrays
  allr <- do.call(rbind, lapply(refs, function(r) r[!is.na(r$path), c("path", "etag", "mtime")]))
  src <- if (is.null(allr) || !nrow(allr)) data.frame(source = character(), etag = character(), mtime = numeric()) else {
    u <- allr[!duplicated(allr$path), ]
    data.frame(source = u$path, etag = u$etag, mtime = u$mtime, stringsAsFactors = FALSE)
  }
  src <- src[order(src$source), , drop = FALSE]
  sources <- data.frame(source_id = seq_len(nrow(src)), source = src$source,
                        size = rep(NA_real_, nrow(src)), mtime = src$mtime, etag = src$etag, stringsAsFactors = FALSE)

  parsed <- .br_arrays_parse(arrays)
  br <- structure(list(format_version = "byterefs/0.1", meta = meta, sources = sources,
                       arrays = parsed, remap = remap,
                       refs = function(array) {
                         r <- refs[[array]]
                         if (is.null(r)) {
                           a <- parsed[parsed$array == array, ]
                           if (!nrow(a)) stop("no array ", array, call. = FALSE)
                           nd <- length(unlist(a$shape[[1]]))
                           r <- as.data.frame(stats::setNames(rep(list(integer()), nd), paste0("dim_", seq_len(nd) - 1L)))
                           r$present <- integer(); r$path <- character(); r$offset <- numeric(); r$size <- numeric()
                           r$info <- character(); r$data <- list(); r$etag <- character(); r$mtime <- numeric()
                         }
                         m <- match(r$path, sources$source)
                         r$source_size <- sources$size[m]
                         r
                       }),
                  class = "byterefs")
  br$.raw <- list(arrays = arrays, refs = refs)
  if (!is.null(dsn)) {
    .br_write_sqlite(br, dsn)
    return(invisible(br))
  }
  br
}

# ---- SQLite writer (byterefs/0.1 working-store schema) ---------------------------

.br_write_sqlite <- function(br, dsn) {
  .need("DBI"); .need("RSQLite")
  if (file.exists(dsn)) stop(dsn, " exists", call. = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), dsn)
  on.exit(DBI::dbDisconnect(con))
  q <- function(sql) DBI::dbExecute(con, sql)
  DBI::dbBegin(con)
  q("CREATE TABLE meta(key TEXT PRIMARY KEY, value TEXT) WITHOUT ROWID")
  q("CREATE TABLE sources(source_id INTEGER PRIMARY KEY, source TEXT NOT NULL UNIQUE, size BIGINT, mtime BIGINT, etag TEXT)")
  q('CREATE TABLE arrays(array TEXT PRIMARY KEY, kind TEXT NOT NULL, storage TEXT NOT NULL, dims TEXT, shape TEXT,
     chunks TEXT, dtype TEXT, fill TEXT, codec TEXT, attrs TEXT, affine TEXT, "values" BLOB) WITHOUT ROWID')
  q("CREATE TABLE placements(array TEXT NOT NULL REFERENCES arrays(array), source_id INTEGER NOT NULL REFERENCES sources(source_id),
     source_array TEXT NOT NULL, src_offset TEXT NOT NULL, count TEXT NOT NULL, dest TEXT NOT NULL,
     PRIMARY KEY (array, dest)) WITHOUT ROWID")
  q("CREATE TABLE remap(target TEXT NOT NULL, prefix TEXT NOT NULL, replacement TEXT NOT NULL, PRIMARY KEY (target, prefix)) WITHOUT ROWID")
  DBI::dbAppendTable(con, "meta", data.frame(key = names(br$meta), value = unlist(br$meta), stringsAsFactors = FALSE))
  DBI::dbAppendTable(con, "sources", br$sources)
  a <- br$.raw$arrays
  a$values <- blob_list(a$values)
  DBI::dbAppendTable(con, "arrays", a)
  if (nrow(br$remap)) DBI::dbAppendTable(con, "remap", br$remap)
  for (nm in names(br$.raw$refs)) {
    r <- br$.raw$refs[[nm]]
    arow <- br$arrays[br$arrays$array == nm, ]
    ch <- as.integer(arow$chunks[[1]]); shape <- as.integer(arow$shape[[1]])
    nd <- length(ch)
    dcols <- paste0("dim_", seq_len(nd) - 1L)
    sid <- match(r$path, br$sources$source)
    # rows without a source (native / inline chunks) go under a synthetic source 0
    sid[is.na(sid)] <- 0L
    tq <- DBI::dbQuoteIdentifier(con, nm)
    q(sprintf("CREATE TABLE %s (source_id INTEGER NOT NULL, %s, present INTEGER NOT NULL DEFAULT 1,
               \"offset\" BIGINT, size BIGINT, info TEXT, data BLOB, PRIMARY KEY (source_id, %s)) WITHOUT ROWID",
              tq, paste(dcols, "INTEGER NOT NULL", collapse = ", "), paste(dcols, collapse = ", ")))
    t <- r[, c(dcols, "present", "offset", "size", "info")]
    t <- cbind(source_id = sid, t)
    t$data <- blob_list(r$data)
    DBI::dbAppendTable(con, nm, t)
    # identity placements: one per source, its chunk bounding box (elements)
    pl <- lapply(split(seq_len(nrow(r)), sid), function(ix) {
      lo <- vapply(dcols, function(d) min(r[[d]][ix]), 0) * ch
      hi <- pmin((vapply(dcols, function(d) max(r[[d]][ix]), 0) + 1) * ch, shape)
      data.frame(array = nm, source_id = sid[ix[1]], source_array = "icechunk:",
                 src_offset = .br_json(I(as.integer(lo))), count = .br_json(I(as.integer(hi - lo))),
                 dest = .br_json(I(as.integer(lo))), stringsAsFactors = FALSE)
    })
    pl <- do.call(rbind, pl)
    if (anyDuplicated(pl$dest)) stop(nm, ": two sources share a placement origin; cannot synthesise placements", call. = FALSE)
    DBI::dbAppendTable(con, "placements", pl)
    # the refs_<array> view, same form as raad.virtualize writes
    dimx <- vapply(seq_len(nd) - 1L, function(i) sprintf(
      "c.dim_%d - CAST(json_extract(p.src_offset, '$[%d]') AS INTEGER) / %d + CAST(json_extract(p.dest, '$[%d]') AS INTEGER) / %d AS dim_%d",
      i, i, ch[i + 1], i, ch[i + 1], i), "")
    wh <- vapply(seq_len(nd) - 1L, function(i) sprintf(
      "c.dim_%d >= CAST(json_extract(p.src_offset, '$[%d]') AS INTEGER) / %d AND c.dim_%d < (CAST(json_extract(p.src_offset, '$[%d]') AS INTEGER) + CAST(json_extract(p.count, '$[%d]') AS INTEGER) + %d - 1) / %d",
      i, i, ch[i + 1], i, i, i, ch[i + 1], ch[i + 1]), "")
    q(sprintf("CREATE VIEW %s AS SELECT %s, c.present, CASE WHEN c.\"offset\" IS NULL THEN NULL ELSE s.source END AS path,
       c.\"offset\", c.size, c.info, c.data
       FROM %s c JOIN placements p ON p.array = '%s' AND p.source_id = c.source_id
            LEFT JOIN sources s ON s.source_id = c.source_id
       WHERE %s ORDER BY %s",
      DBI::dbQuoteIdentifier(con, paste0("refs_", nm)), paste(dimx, collapse = ",\n       "),
      tq, nm, paste(wh, collapse = "\n    AND "), paste(dcols, collapse = ", ")))
  }
  DBI::dbCommit(con)
  invisible(dsn)
}

blob_list <- function(x, check = TRUE) {
  # check = FALSE: elements are already raw or NULL (built in Rust)
  if (check) x <- lapply(x, function(v) if (is.null(v)) NULL else as.raw(v))
  structure(unclass(x), class = c("blob", "vctrs_list_of", "vctrs_vctr", "list"), ptype = raw())
}

# ---- comparison ---------------------------------------------------------------------

#' Compare two byterefs stores
#'
#' The conformance check for a round trip: array descriptions must agree
#' (dims, shape, chunks, dtype, fill, codec, attributes, coordinate values)
#' and every array's global refs rows must agree on index, presence,
#' resolved location, offset, size and inline bytes. Source paths are
#' compared after each side's own remap for `target`.
#'
#' @param x,y byterefs stores (see [ic_byterefs_read()]).
#' @param target remap target used to resolve paths on both sides.
#' @return a data frame of checks: `array`, `check`, `ok`, `detail`.
#' @example inst/examples/ic_byterefs_compare.R
#' @export
ic_byterefs_compare <- function(x, y, target = NULL) {
  ox <- !inherits(x, "byterefs"); oy <- !inherits(y, "byterefs")
  x <- ic_byterefs_read(x); y <- ic_byterefs_read(y)
  if (ox && !is.null(x$close)) on.exit(x$close(), add = TRUE)
  if (oy && !is.null(y$close)) on.exit(y$close(), add = TRUE)
  tx <- .br_target(x, target); ty <- .br_target(y, target %||% tx)
  rx <- .br_rules(x, tx); ry <- if (nrow(y$remap)) .br_rules(y, ty) else rx
  out <- list()
  add <- function(a, check, ok, detail = "") out[[length(out) + 1L]] <<- data.frame(array = a, check = check, ok = isTRUE(ok), detail = detail)
  na <- sort(x$arrays$array); nb <- sort(y$arrays$array)
  add("*", "array names", identical(na, nb), paste(setdiff(union(na, nb), intersect(na, nb)), collapse = ","))
  same <- function(u, v) isTRUE(all.equal(u, v, check.attributes = FALSE))
  for (nm in intersect(na, nb)) {
    a <- x$arrays[x$arrays$array == nm, ]; b <- y$arrays[y$arrays$array == nm, ]
    for (f in c("kind", "dtype")) add(nm, f, identical(a[[f]], b[[f]]), paste(a[[f]], b[[f]]))
    for (f in c("dims", "shape", "chunks")) add(nm, f, same(unlist(a[[f]][[1]]), unlist(b[[f]][[1]])))
    fa <- .br_fill_value(a$fill); fb <- .br_fill_value(b$fill)
    add(nm, "fill", identical(fa, fb) || (is.null(fa) && is.null(fb)) || isTRUE(all.equal(fa, fb)), paste(a$fill, b$fill))
    if (a$storage == "referenced") {
      add(nm, "codec", same(a$codec[[1]], b$codec[[1]]), paste(.br_json(a$codec[[1]]), "|", .br_json(b$codec[[1]])))
    }
    ka <- a$attrs[[1]]; kb <- b$attrs[[1]]
    srt <- function(k) if (length(k)) k[order(names(k))] else list()
    add(nm, "attrs", same(srt(ka), srt(kb)),
        paste(setdiff(union(names(ka), names(kb)), intersect(names(ka), names(kb))), collapse = ","))
    if (a$kind == "coord" && a$storage != "referenced") {
      va <- .br_coord_values(a); vb <- .br_coord_values(b)
      add(nm, "coord values", identical(va, vb), sprintf("%s vs %s", a$storage, b$storage))
      next
    }
    ra <- x$refs(nm); rb <- y$refs(nm)
    key <- function(r) do.call(paste, c(r[grep("^dim_", names(r))], sep = ","))
    pa <- ra[ra$present == 1L, ]; pb <- rb[rb$present == 1L, ]
    pa <- pa[order(key(pa)), ]; pb <- pb[order(key(pb)), ]
    add(nm, "present chunks", identical(key(pa), key(pb)), sprintf("%d vs %d", nrow(pa), nrow(pb)))
    if (identical(key(pa), key(pb))) {
      la <- .br_resolve(pa$path, rx); lb <- .br_resolve(pb$path, ry)
      add(nm, "locations", identical(la, lb), sprintf("%d differ", sum(la != lb, na.rm = TRUE)))
      add(nm, "offsets", same(as.double(pa$offset), as.double(pb$offset)))
      add(nm, "sizes", same(as.double(pa$size), as.double(pb$size)))
      dz <- function(d) vapply(d, function(v) if (length(v)) paste(as.character(v), collapse = "") else "", "")
      add(nm, "inline data", identical(dz(pa$data), dz(pb$data)))
    }
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

.br_coord_values <- function(a) {
  shape <- unlist(a$shape[[1]])
  if (a$storage == "affine") {
    af <- a$affine[[1]]
    return(.br_unpack(.br_pack(af$start + af$step * (seq_len(shape[1]) - 1), a$dtype), a$dtype, shape[1]))
  }
  .br_unpack(a$values[[1]], a$dtype, prod(shape))
}

#' Chunk references of one array as an Arrow stream
#'
#' The byterefs refs view of an array (`dim_0..`, `present`, `path`,
#' `offset`, `size`, `info`, `data`), built in Rust as Arrow record batches
#' and handed over as a `nanoarrow_array_stream`, without R vectors in
#' between. `path` is dictionary-encoded (one entry per source), `offset`
#' and `size` are int64, as the byterefs spec has them. Paths have the
#' repository's remap rules undone (see [ic_write_byterefs()]); native
#' chunks (bytes held by the repository) have no path, and their bytes in
#' `data` when `native = TRUE`, as [ic_to_byterefs()] writes them.
#'
#' The stream is lazy: it reads the session as it is when called (later
#' writes to the session are not seen), and builds each batch as the
#' consumer asks for it, with one batch of read-ahead, so a large array's
#' refs never sit in memory whole. Errors met while streaming (a manifest
#' that cannot be fetched) are raised by the consumer.
#'
#' Consume with `nanoarrow::convert_array_stream()`, `arrow::as_arrow_table()`,
#' `arrow::write_dataset()` or `duckdb::duckdb_register_arrow()`.
#'
#' @param session an `icechunk_session`.
#' @param path array path.
#' @param chunks optional slab, `list(dim_0 = c(lo, hi), ...)` of chunk
#'   indices (hi exclusive); only the manifests covering it are read.
#' @param batch_rows rows per record batch.
#' @param native fetch native chunks' bytes into `data` (concurrently, see
#'   [ic_manifest_concurrency()]); `FALSE` leaves them null.
#' @return a `nanoarrow_array_stream`.
#' @example inst/examples/ic_refs_arrow.R
#' @export
ic_refs_arrow <- function(session, path, chunks = NULL, batch_rows = 65536L, native = TRUE) {
  .need("nanoarrow")
  p <- if (startsWith(path, "/")) path else paste0("/", path)
  rules <- .br_root(session)$rules
  ranges <- NULL
  if (!is.null(chunks)) {
    n <- ic_nodes(session)
    i <- match(p, n$path)
    if (is.na(i)) stop("no array '", path, "'", call. = FALSE)
    ranges <- .dbi_chunks_ranges(chunks, length(strsplit(n$shape[i], ",", fixed = TRUE)[[1]]))
  }
  .refs_arrow(session, p, ranges, rules, batch_rows, native)
}

# -> lazy nanoarrow stream
.refs_arrow <- function(session, p, ranges, rules, batch_rows = 65536L, native = TRUE) {
  stream <- nanoarrow::nanoarrow_allocate_array_stream()
  .ic(ic_refs_arrow_(session$ptr, p, ranges, as.character(rules$prefix), as.character(rules$replacement),
                             as.integer(batch_rows), isTRUE(native), nanoarrow::nanoarrow_pointer_addr_chr(stream)))
  stream
}
