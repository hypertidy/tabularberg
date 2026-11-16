# dbplyr integration (Suggests): tbl(con, "refs_<array>") works on an
# Icechunk connection, translated in the dialect of its SQL engine.
# The tokio runtime is shut down when R exits (and when the package is
# unloaded): on Windows, exiting with it live, after any HTTP/TLS use, crashed
# R in ExitProcess (CI: tests and --run-donttest examples). The finalizer is
# on an environment of our own so it runs once, at exit, with onexit = TRUE.
.runtime_guard <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  reg.finalizer(.runtime_guard, function(e) try(ic_runtime_shutdown_(), silent = TRUE), onexit = TRUE)
  s3_register("dbplyr::dbplyr_edition", "IcechunkConnection", function(con) 2L)
  s3_register("dbplyr::sql_dialect", "IcechunkConnection", function(con) {
    state <- .dbi_check(con)
    dbplyr::sql_dialect(.dbi_sql(state, "")$con)
  })
  invisible()
}

# Register an S3 method for a generic in a suggested package, now if it is
# loaded and again whenever it loads (after vctrs::s3_register).
s3_register <- function(generic, class, method) {
  parts <- strsplit(generic, "::", fixed = TRUE)[[1]]
  pkg <- parts[1]; gen <- parts[2]
  reg <- function(...) {
    ns <- asNamespace(pkg)
    if (exists(gen, envir = ns)) registerS3method(gen, class, method, envir = ns)
  }
  setHook(packageEvent(pkg, "onLoad"), reg)
  if (isNamespaceLoaded(pkg)) reg()
  invisible()
}

.onUnload <- function(libpath) {
  try(ic_runtime_shutdown_(), silent = TRUE)
  invisible()
}
