# get_token() must fail loudly when the credentials are rejected, instead of
# handing an empty token to the next call.
test_that("get_token() reports rejected credentials", {
  local_mocked_bindings(
    POST = function(...) fake_response(
      paste0('{"error":"invalid_client","error_description":',
             '"The client credentials are invalid"}'),
      status = 400L),
    .package = "httr")
  expect_error(get_token("a-key", "a-secret"), "invalid_client")
  expect_error(get_token("a-key", "a-secret"), "credentials are invalid")
})

test_that("get_token() reports an answer without a token", {
  local_mocked_bindings(
    POST = function(...) fake_response('{"token_type":"Bearer"}'),
    .package = "httr")
  expect_error(get_token("a-key", "a-secret"), "no access token")
})

test_that("get_token() returns the token when the answer carries one", {
  local_mocked_bindings(
    POST = function(...) fake_response(
      '{"access_token":"a-token","token_type":"Bearer"}'),
    .package = "httr")
  expect_equal(get_token("a-key", "a-secret"), "a-token")
})
