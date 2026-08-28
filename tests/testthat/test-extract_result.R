test_that("extract_result() picks the translated text out of the answer", {
  json <- paste0('{"resultset":{"code":0,"message":"",',
                 '"result":{"text":"\\u3053\\u3093\\u306b\\u3061\\u306f"}}}')
  res <- fake_response(json)
  expect_equal(extract_result(res), "\u3053\u3093\u306b\u3061\u306f")
})

test_that("extract_result() handles plain ASCII text", {
  res <- fake_response('{"resultset":{"result":{"text":"Ciao mondo"}}}')
  expect_equal(extract_result(res), "Ciao mondo")
})

# The API answers with HTTP 200 even when it fails, and reports the failure
# in resultset$code, so the status code alone cannot be relied upon.
# Observed on 2026-08-28: a wrong token gives code 510 with an empty message,
# and an unknown model gives code 530 with a message.
test_that("a non-zero code raises an error naming the code and the message", {
  res <- fake_response('{"resultset":{"code":530,"message":"an error"}}')
  expect_error(extract_result(res), "530")
  expect_error(extract_result(res), "an error")
})

test_that("a non-zero code with no message still names the code", {
  res <- fake_response('{"resultset":{"code":510,"message":""}}')
  expect_error(extract_result(res), "510")
  expect_error(extract_result(res), "no message")
})

test_that("an OAuth style error is reported", {
  res <- fake_response('{"error":"invalid_client"}', status = 400L)
  expect_error(extract_result(res), "invalid_client")
})

test_that("a success code with no text raises an error", {
  res <- fake_response('{"resultset":{"code":0,"message":""}}')
  expect_error(extract_result(res), "no translated text")
})

test_that("an HTTP error with no resultset is reported", {
  res <- fake_response('{}', status = 500L)
  expect_error(extract_result(res), "500")
})
