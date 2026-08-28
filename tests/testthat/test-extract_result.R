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

# The current implementation returns NULL when the answer carries no text.
# These tests pin that behaviour down, so that a later change to raise an
# error instead becomes a deliberate and visible one.
test_that("extract_result() returns NULL when the answer carries no result", {
  res <- fake_response('{"resultset":{"code":500,"message":"an error"}}')
  expect_null(extract_result(res))
})

test_that("extract_result() returns NULL when there is no resultset at all", {
  res <- fake_response('{"error":"invalid_client"}')
  expect_null(extract_result(res))
})
