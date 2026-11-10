#' Credentials and virtual chunk containers
#'
#' Virtual chunk references point outside the repository. A repository
#' declares *containers* (URL prefixes plus how to reach them), and the
#' reader must *authorise* each prefix it is willing to read, with
#' credentials. Nothing is authorised by default: this is the "where"
#' control.
#'
#' @param kind for S3: `"from_env"`, `"anonymous"` or `"static"`; for GCS
#'   and Azure: `"from_env"` or `"anonymous"`.
#' @param access_key_id,secret_access_key,session_token static S3 keys.
#' @return a credential spec (a list), for `authorize` in [ic_repo_open()].
#' @name ic_credentials
#' @example inst/examples/ic_credentials.R
#' @export
ic_cred_s3 <- function(kind = c("from_env", "anonymous", "static"),
                       access_key_id = NULL, secret_access_key = NULL,
                       session_token = NULL) {
  kind <- match.arg(kind)
  x <- list(type = "s3", kind = kind, access_key_id = access_key_id,
            secret_access_key = secret_access_key, session_token = session_token)
  structure(x[!vapply(x, is.null, logical(1))], class = "icechunk_cred")
}

#' @rdname ic_credentials
#' @export
ic_cred_http <- function() structure(list(type = "http"), class = "icechunk_cred")

#' @rdname ic_credentials
#' @export
ic_cred_local <- function() structure(list(type = "local"), class = "icechunk_cred")

#' @rdname ic_credentials
#' @export
ic_cred_gcs <- function(kind = c("from_env", "anonymous")) {
  structure(list(type = "gcs", kind = match.arg(kind)), class = "icechunk_cred")
}

#' @rdname ic_credentials
#' @export
ic_cred_azure <- function(kind = c("from_env", "anonymous")) {
  structure(list(type = "azure", kind = match.arg(kind)), class = "icechunk_cred")
}

#' @export
print.icechunk_cred <- function(x, ...) {
  shown <- x[setdiff(names(x), c("access_key_id", "secret_access_key", "session_token"))]
  cat("<icechunk_cred>", paste(names(shown), unlist(shown), sep = "=", collapse = " "))
  if (!is.null(x$secret_access_key)) cat(" [static keys hidden]")
  cat("\n")
  invisible(x)
}

#' @param url_prefix the prefix chunk locations must start with, e.g.
#'   `"s3://bucket/path/"`, `"https://host/path/"`, `"file:///data/"`.
#' @param type store type for the container.
#' @param region,endpoint_url,anonymous,allow_http,force_path_style S3
#'   options for `type = "s3"` or `"s3_compatible"`.
#' @param path for `type = "local"`.
#' @rdname ic_credentials
#' @export
ic_container <- function(url_prefix,
                         type = c("s3", "s3_compatible", "http", "local", "gcs", "in_memory"),
                         region = NULL, endpoint_url = NULL, anonymous = FALSE,
                         allow_http = FALSE, force_path_style = FALSE, path = NULL) {
  type <- match.arg(type)
  st <- list(type = type, region = region, endpoint_url = endpoint_url,
             anonymous = anonymous, allow_http = allow_http,
             force_path_style = force_path_style, path = path)
  st <- st[!vapply(st, is.null, logical(1))]
  structure(list(url_prefix = url_prefix, store = st), class = "icechunk_container")
}

# authorize: NULL, a character vector of prefixes (credential inferred from
# scheme), or a named list prefix -> cred spec (NULL = icechunk default).
.authorize_json <- function(authorize) {
  if (is.null(authorize) || length(authorize) == 0L) return("")
  if (is.character(authorize)) {
    authorize <- stats::setNames(lapply(authorize, .infer_cred), authorize)
  }
  if (is.null(names(authorize)) || any(!nzchar(names(authorize)))) {
    stop("authorize must be named by URL prefix", call. = FALSE)
  }
  out <- lapply(authorize, function(x) if (is.null(x)) NULL else unclass(x))
  as.character(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null"))
}

.infer_cred <- function(prefix) {
  scheme <- tolower(sub(":.*$", "", prefix))
  switch(scheme,
    s3 = ic_cred_s3("from_env"),
    http = , https = ic_cred_http(),
    file = ic_cred_local(),
    gs = , gcs = ic_cred_gcs("from_env"),
    az = , abfs = , azure = ic_cred_azure("from_env"),
    stop("cannot infer credentials for prefix: ", prefix, call. = FALSE)
  )
}

.containers_json <- function(containers) {
  if (is.null(containers) || length(containers) == 0L) return("")
  if (inherits(containers, "icechunk_container")) containers <- list(containers)
  as.character(jsonlite::toJSON(lapply(containers, unclass), auto_unbox = TRUE))
}
