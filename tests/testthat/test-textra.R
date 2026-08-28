test_that("textra() builds api_param from model, from and to", {
  captured <- NULL
  local_mocked_bindings(
    api_post = function(params, text) {
      captured <<- list(params = params, text = text)
      "a-response"
    },
    api_extract = function(res) "translated"
  )
  out <- textra("Hello world", list(key = "k"),
                model = "transLM", from = "en", to = "ja")
  expect_equal(out, "translated")
  expect_equal(captured$text, "Hello world")
  expect_equal(captured$params$api_param, "transLM_en_ja")
})

test_that("textra() keeps the parameters it was given", {
  captured <- NULL
  local_mocked_bindings(
    api_post = function(params, text) {
      captured <<- params
      "a-response"
    },
    api_extract = function(res) "translated"
  )
  textra("text", list(access_token = "t", key = "k"))
  expect_equal(captured$access_token, "t")
  expect_equal(captured$key, "k")
})

test_that("textra() defaults to transLM from English to Japanese", {
  captured <- NULL
  local_mocked_bindings(
    api_post = function(params, text) {
      captured <<- params
      "a-response"
    },
    api_extract = function(res) "translated"
  )
  textra("text", list())
  expect_equal(captured$api_param, "transLM_en_ja")
})
