test_that("gen_params() assembles the fields the API expects", {
  local_mocked_bindings(get_token = function(key, secret) "a-token")
  params <- gen_params(key = "a-key", secret = "a-secret", name = "a-login")
  expect_named(params,
               c("access_token", "key", "name", "api_name", "type"))
  expect_equal(params$access_token, "a-token")
  expect_equal(params$key, "a-key")
  expect_equal(params$name, "a-login")
  expect_equal(params$api_name, "mt")
  expect_equal(params$type, "json")
})

test_that("gen_params() falls back to the environment variables", {
  local_mocked_bindings(get_token = function(key, secret) paste0("t:", key))
  withr::local_envvar(TEXTRA_API_KEY = "env-key",
                      TEXTRA_API_SECRET = "env-secret",
                      TEXTRA_NAME = "env-login")
  params <- gen_params()
  expect_equal(params$key, "env-key")
  expect_equal(params$name, "env-login")
  expect_equal(params$access_token, "t:env-key")
})

test_that("gen_params() fails when no credential is available", {
  local_mocked_bindings(get_token = function(key, secret) "a-token")
  withr::local_envvar(TEXTRA_API_KEY = NA,
                      TEXTRA_API_SECRET = NA,
                      TEXTRA_NAME = NA)
  expect_error(gen_params(), "TEXTRA_API_KEY")
})

test_that("api_name can be overridden", {
  local_mocked_bindings(get_token = function(key, secret) "a-token")
  params <- gen_params(key = "k", secret = "s", name = "n",
                       api_name = "mt_standard")
  expect_equal(params$api_name, "mt_standard")
})
