test_that("api_base_url() returns the 'TexTra' endpoint", {
  expect_type(api_base_url(), "character")
  expect_length(api_base_url(), 1L)
  expect_equal(api_base_url(), "https://mt-auto-minhon-mlt.ucri.jgn-x.jp")
})

test_that("api_base_url() is https, so that credentials are not sent in clear", {
  expect_match(api_base_url(), "^https://")
})
