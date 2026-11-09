#' Icechunk storage
#'
#' Where the repository lives. Storage objects hold no open connections and
#' are cheap; credentials given here are never printed.
#'
#' @param path directory of the repository (local).
#' @param base_url URL of the repository root (read-only HTTP).
#' @param bucket,prefix bucket and key prefix of the repository.
#' @param region,endpoint_url S3 region and endpoint (for S3-compatible
#'   services such as Pawsey Acacia or data.source.coop).
#' @param anonymous unsigned requests.
#' @param allow_http allow `http://` endpoints.
#' @param force_path_style path-style addressing (most S3-compatible
#'   services need this).
#' @param requester_pays requester-pays bucket.
#' @param credentials an [ic_cred_s3()] spec, or `NULL` for icechunk's default
#'   (from the environment).
#' @param backend `"object_store"` (default) or `"aws"` (the aws-sdk backend,
#'   only in the `full` build).
#' @param account,container Azure account and container.
#' @return an `icechunk_storage` object.
#' @name ic_storage
#' @export
ic_storage_local <- function(path) {
  path <- normalizePath(path.expand(path), mustWork = FALSE)
  .storage(.ic(ic_storage_local_(path)))
}

#' @rdname ic_storage
#' @export
ic_storage_memory <- function() .storage(.ic(ic_storage_memory_()))

#' @rdname ic_storage
#' @export
ic_storage_http <- function(base_url) .storage(.ic(ic_storage_http_(base_url)))

#' @rdname ic_storage
#' @export
ic_storage_s3 <- function(bucket, prefix = NULL, region = NULL, endpoint_url = NULL,
                          anonymous = FALSE, allow_http = FALSE,
                          force_path_style = !is.null(endpoint_url),
                          requester_pays = FALSE, credentials = NULL,
                          backend = c("object_store", "aws")) {
  backend <- match.arg(backend)
  if (anonymous && is.null(credentials)) credentials <- ic_cred_s3("anonymous")
  opts <- list(region = region, endpoint_url = endpoint_url, anonymous = anonymous,
               allow_http = allow_http, force_path_style = force_path_style,
               requester_pays = requester_pays,
               credentials = if (is.null(credentials)) NULL else unclass(credentials))
  opts <- opts[!vapply(opts, is.null, logical(1))]
  .storage(.ic(ic_storage_s3_(bucket, .chr(prefix), .json(opts), backend)))
}

#' @rdname ic_storage
#' @export
ic_storage_gcs <- function(bucket, prefix = NULL, anonymous = FALSE) {
  .storage(.ic(ic_storage_gcs_(bucket, .chr(prefix), anonymous)))
}

#' @rdname ic_storage
#' @export
ic_storage_azure <- function(account, container, prefix = NULL) {
  .storage(.ic(ic_storage_azure_(account, container, .chr(prefix))))
}

.storage <- function(ptr) structure(list(ptr = ptr), class = "icechunk_storage")

#' @export
print.icechunk_storage <- function(x, ...) {
  cat("<icechunk_storage>", ic_storage_desc_(x$ptr), "\n")
  invisible(x)
}

.json <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}
