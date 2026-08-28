# The three low level functions stay exported for one release, but warn.
# They must keep working, and must not warn when textra() uses them
# internally.
test_that("base_url() warns but still returns the endpoint", {
  expect_warning(out <- base_url(), "deprecated")
  expect_equal(out, api_base_url())
})

test_that("extract_result() warns but still parses the answer", {
  res <- fake_response('{"resultset":{"result":{"text":"Ciao mondo"}}}')
  expect_warning(out <- extract_result(res), "deprecated")
  expect_equal(out, "Ciao mondo")
})

test_that("the warning says what to use instead and where to report", {
  expect_warning(base_url(), "textra()", fixed = TRUE)
  expect_warning(base_url(), "github.com/matutosi/textrar/issues",
                 fixed = TRUE)
})

test_that("textra() does not warn when it uses the low level functions", {
  local_mocked_bindings(
    api_post = function(params, text) "a-response",
    api_extract = function(res) "translated"
  )
  expect_no_warning(textra("Hello world", list()))
})
