test_that("an instance-metadata failure gets a missing-region hint", {
  msg <- paste0("error fetching virtual reference: Generic S3 error: Error performing PUT ",
                "http://169.254.169.254/latest/api/token in 2.6s - 404 Not Found: <html><body>x</body></html>")
  out <- .hint(msg)
  expect_match(out, "no region is set", fixed = TRUE)
  expect_false(grepl("<html>", out, fixed = TRUE))
  expect_match(out, "error fetching virtual reference", fixed = TRUE)
})

test_that("other messages are left alone", {
  expect_identical(.hint("object not found"), "object not found")
})

test_that("an instance-metadata 404 is not classed as not_found", {
  msg <- paste0("[icechunk:not_found] error fetching virtual reference: Generic S3 error: ",
                "Error performing PUT http://169.254.169.254/latest/api/token - 404 Not Found: <html></html>")
  e <- tryCatch(.ic(stop(msg)), error = identity)
  expect_s3_class(e, "icechunk_error")
  expect_false(inherits(e, "icechunk_not_found"))
  expect_false(grepl("Not Found:\n", conditionMessage(e), fixed = TRUE))
})

test_that("a failed virtual fetch reports its cause", {
  ex <- ic_example_repo()
  unlink(file.path(ex$source, "grid.bin"))
  s <- ic_session_readonly(ic_repo_open(ex$path, authorize = ex$authorize), branch = "main")
  e <- tryCatch(ic_get(s, "from_bin/c/0/0"), error = identity)
  expect_s3_class(e, "icechunk_not_found")
  # the top-level message is only "error fetching virtual reference"
  expect_match(conditionMessage(e), "grid.bin", fixed = TRUE)
  e <- tryCatch(ic_get_many(s, "from_bin/c/0/0"), error = identity)
  expect_match(conditionMessage(e), "grid.bin", fixed = TRUE)
})
