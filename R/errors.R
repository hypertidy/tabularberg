# Rust errors arrive as "[icechunk:<class>] message"; re-signal them as
# conditions of class c("icechunk_<class>", "icechunk_error", "error").
.ic <- function(expr) {
  tryCatch(expr, error = function(e) {
    msg <- conditionMessage(e)
    m <- regmatches(msg, regexec("^\\[icechunk:([a-z_]+)\\] (.*)$", msg))[[1]]
    if (length(m) != 3L) stop(e)
    cls <- m[2]
    sub <- if (cls == "error") character() else paste0("icechunk_", cls)
    stop(structure(
      class = c(sub, "icechunk_error", "error", "condition"),
      list(message = m[3], call = sys.call(-4))
    ))
  })
}

.chr <- function(x) if (is.null(x) || (length(x) == 1L && is.na(x))) "" else as.character(x)

.na <- function(x) { x[!nzchar(x)] <- NA_character_; x }

.df <- function(x) {
  x <- lapply(x, function(col) if (is.character(col)) .na(col) else col)
  structure(x, class = "data.frame", row.names = .set_row_names(length(x[[1L]])))
}

# base R has %||% only from 4.4.0
`%||%` <- function(x, y) if (is.null(x)) y else x
