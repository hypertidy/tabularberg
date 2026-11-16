# Rust errors arrive as "[icechunk:<class>] message"; re-signal them as
# conditions of class c("icechunk_<class>", "icechunk_error", "error").
.ic <- function(expr) {
  tryCatch(expr, error = function(e) {
    msg <- conditionMessage(e)
    m <- regmatches(msg, regexec("^\\[icechunk:([a-z_]+)\\] (.*)$", msg))[[1]]
    if (length(m) != 3L) stop(e)
    cls <- m[2]
    # The metadata service's "404 Not Found" is not a missing object.
    if (.imds(m[3])) cls <- "error"
    m[3] <- .hint(m[3])
    sub <- if (cls == "error") character() else paste0("icechunk_", cls)
    stop(structure(
      class = c(sub, "icechunk_error", "error", "condition"),
      list(message = m[3], call = sys.call(-4))
    ))
  })
}

# An S3 client with no credentials looks them up from the EC2 instance
# metadata service (169.254.169.254). Off EC2 that fails, so a failure there
# almost always means a virtual chunk container on a public bucket is being
# read with signed requests: it needs anonymous access, not a network fix.
.imds <- function(msg) grepl("169.254.169.254", msg, fixed = TRUE)

.hint <- function(msg) {
  if (!.imds(msg)) return(msg)
  msg <- sub(":?\\s*<html>.*$", "", msg)
  paste0(trimws(msg), "\n",
         "Hint: the S3 client was looking for credentials in instance metadata, so a virtual ",
         "chunk container is being read with signed requests. For a public bucket, authorise ",
         "it anonymously: authorize = list(<prefix> = ic_cred_s3(\"anonymous\")). ",
         "ic_repo_containers() lists the prefixes.")
}

.chr <- function(x) if (is.null(x) || (length(x) == 1L && is.na(x))) "" else as.character(x)

.na <- function(x) { x[!nzchar(x)] <- NA_character_; x }

.df <- function(x) {
  x <- lapply(x, function(col) if (is.character(col)) .na(col) else col)
  structure(x, class = "data.frame", row.names = .set_row_names(length(x[[1L]])))
}

# base R has %||% only from 4.4.0
`%||%` <- function(x, y) if (is.null(x)) y else x
