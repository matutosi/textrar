test_that("textra_env() returns the value of the variable", {
  withr::local_envvar(TEXTRA_TEST_VAR = "a-value")
  expect_equal(textra_env("TEXTRA_TEST_VAR"), "a-value")
})

test_that("textra_env() fails when the variable is unset", {
  withr::local_envvar(TEXTRA_TEST_VAR = NA)
  expect_error(textra_env("TEXTRA_TEST_VAR"), "is not set")
})

test_that("textra_env() fails when the variable is empty", {
  withr::local_envvar(TEXTRA_TEST_VAR = "")
  expect_error(textra_env("TEXTRA_TEST_VAR"), "is not set")
})

test_that("the error names the variable and says how to fix it", {
  withr::local_envvar(TEXTRA_TEST_VAR = NA)
  expect_error(textra_env("TEXTRA_TEST_VAR"), "TEXTRA_TEST_VAR")
  expect_error(textra_env("TEXTRA_TEST_VAR"), ".Renviron", fixed = TRUE)
})
