test_that("compiled feature tier is what the build asked for", {
  f <- ic_compiled_features()
  expect_true(all(c("minimal", "fs", "http") %in% f))
  expect_identical(ic_icechunk_version(), "2.3.1")
  # CI sets ICECHUNK_EXPECT_TIER so a build that silently dropped the tier fails
  tier <- Sys.getenv("ICECHUNK_EXPECT_TIER")
  skip_if(!nzchar(tier), "ICECHUNK_EXPECT_TIER not set")
  expect_true(tier %in% f, info = paste(f, collapse = ","))
  higher <- switch(tier, minimal = c("cloud", "full"), cloud = "full", full = character())
  expect_false(any(higher %in% f), info = paste(f, collapse = ","))
})
