test_that("base_url() returns the 'TexTra' endpoint", {
  expect_type(base_url(), "character")
  expect_length(base_url(), 1L)
  expect_equal(base_url(), "https://mt-auto-minhon-mlt.ucri.jgn-x.jp")
})

test_that("base_url() is https, so that credentials are not sent in clear", {
  expect_match(base_url(), "^https://")
})
