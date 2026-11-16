test_that("the runtime shuts down, and starts again on next use", {
  # R's exit finalizer and .onUnload() call this
  ex <- ic_example_repo()
  ic_runtime_shutdown_()
  expect_false(ic_runtime_shutdown_())  # nothing running now
  s <- ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main")
  expect_length(ic_get(s, "from_bin/c/0/0"), 3072L)
  expect_true(ic_runtime_shutdown_())
  expect_length(ic_get(s, "from_bin/c/1/0"), 3072L)  # a session made before shutdown still reads
})
