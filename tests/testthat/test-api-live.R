# A real round trip through the API. It is skipped unless the credentials are
# available AND TEXTRA_TEST_LIVE is set, so that it does not consume the API
# quota on every check.
skip_unless_live <- function(){
  skip_on_cran()
  if(!nzchar(Sys.getenv("TEXTRA_TEST_LIVE"))){
    skip("set TEXTRA_TEST_LIVE to run the tests that call the API")
  }
  for(var in c("TEXTRA_API_KEY", "TEXTRA_API_SECRET", "TEXTRA_NAME")){
    if(!nzchar(Sys.getenv(var))) skip(paste(var, "is not set"))
  }
  skip_if_offline()
}

test_that("get_token() returns a token", {
  skip_unless_live()
  token <- get_token()
  expect_type(token, "character")
  expect_true(nzchar(token))
})

test_that("a text can be translated from English into Japanese", {
  skip_unless_live()
  translated <- textra("Hello world", gen_params(),
                       model = "transLM", from = "en", to = "ja")
  expect_type(translated, "character")
  expect_true(nzchar(translated))
})
