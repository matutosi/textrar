#' Translate text using Transliteration Model
#'
#' This function translates text using a transliteration model.
#'
#' @param text The text to be translated.
#' @param params A list of parameters to be passed to the API.
#' @param model The model to be used for translation.
#' @param from The source language.
#' @param to The target language.
#'
#' @return The translated text.
#'
#' @examples
#' \dontrun{
#' text <- "Hello world"
#' key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
#' secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
#' name <- "login_ID"                                 # login_ID
#' params <- gen_params(key = key, secret = secret, name = name)
#' translated <- 
#'   textra(text, params, model = "transLM", from = "en", to = "ja")
#' translated
#' }
#'
#' @export
textra <- function(text, params, model = "transLM", from = "en", to = "ja"){
  api_param <- paste0(model, "_", from, "_", to)
  params <- c(params, list(api_param = api_param))
  res <- post_request(params, text)
  translated <- extract_result(res)
  return(translated)
}

#' Get Access Token
#'
#' This function retrieves an access token 
#' from the API using the provided key and secret.
#' 
#' @inherit gen_params
#'
#' @return A character string containing the access token.
#'
#' @examples
#' \dontrun{
#' key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
#' secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
#' token <- get_token(key = key, secret = secret)
#' }
#' 
#' @export
get_token <- function(key, secret){
  token_url <- paste0(base_url(), "/oauth2/token.php")
  token_req <- httr::POST(
    url = token_url,
    body = list(
      client_id = key,
      client_secret = secret,
      grant_type = "client_credentials"
    ),
    encode = "form",
    config = httr::config(ssl_verifypeer = FALSE)
  )
  token <- 
    token_req |>
    httr::content("text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    `$`(_, "access_token")
  return(token)
}

#' Generate parameters for API call
#'
#' This function generates a list of parameters 
#' that can be used to make an API call.
#'
#' @param key The API key.
#' @param secret The API secret.
#' @param name The name of the API.
#' @param api_name The name of the API to use. Defaults to "mt".
#'
#' @return A list of parameters.
#'
#' @examples
#' \dontrun{
#' key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
#' secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
#' name <- "login_ID"                                 # login_ID
#' params <- gen_params(key = key, secret = secret, name = name)
#' }
#'
#' @export
gen_params <- function(key, secret, name, api_name = "mt"){
  token <- get_token(key, secret)
  params <- 
    list(
      access_token = token,
      key = key,
      name = name,
      api_name = api_name,
      type = "json")
   return(params)
}

#' Send a POST request to the API
#'
#' This function sends a POST request to the API 
#' with the specified parameters and text.
#'
#' @inherit textra
#'
#' @return The response from the API.
#'
#' @examples
#' \dontrun{
#' post_request(params = , text = "Hello, world!")
#' }
#'
#' @export
post_request <- function(params, text){
  body <- c(params, list(text = text))
  res <- 
    httr::POST(
      url = paste0(base_url(), "/api/?"),
      body = body,
      encode = "form",
      config = httr::config(ssl_verifypeer = FALSE))
  return(res)
}

#' Extract Translated Text from Response
#'
#' This function extracts the translated text from the response.
#' returned by the `post_request()` function.
#'
#' @param res The response object returned by the `post_request()`.
#'
#' @return A character string containing the translated text.
#'
#' @examples
#' \dontrun{
#' res <- post_request(paramas, "Hello world!")
#' translated <- extract_result(res)
#' }
#'
#' @export
extract_result <- function(res){
  res_list <- 
    res |>
    httr::content("text", encoding = "UTF-8") |>
    jsonlite::fromJSON()
  translated <- 
    res_list |>
    `$`(_, "resultset") |>
    `$`(_, "result") |>
    `$`(_, "text")
  return(translated)
}

#' Return the base URL for the API
#'
#' @return A string containing the base URL.
#'
#' @examples
#' base_url()
#' 
#' @export
base_url <- function(){
  return("https://mt-auto-minhon-mlt.ucri.jgn-x.jp")
}
