#' Translate text with 'TexTra'
#'
#' This function translates text with the 'TexTra' machine translation API.
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
#' @inheritParams gen_params
#'
#' @return A character string containing the access token.
#'
#' @examples
#' \dontrun{
#' # Using the credentials stored in .Renviron (recommended).
#' token <- get_token()
#'
#' # Or passing them explicitly.
#' key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
#' secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
#' token <- get_token(key = key, secret = secret)
#' }
#' 
#' @export
get_token <- function(key = textra_env("TEXTRA_API_KEY"),
                      secret = textra_env("TEXTRA_API_SECRET")){
  token_url <- paste0(base_url(), "/oauth2/token.php")
  token_req <- httr::POST(
    url = token_url,
    body = list(
      client_id = key,
      client_secret = secret,
      grant_type = "client_credentials"
    ),
    encode = "form"
  )
  parsed <- jsonlite::fromJSON(
    httr::content(token_req, "text", encoding = "UTF-8"))
  token <- parsed$access_token
  return(token)
}

#' Generate parameters for API call
#'
#' This function generates a list of parameters 
#' that can be used to make an API call.
#'
#' @param key The API key. Defaults to the environment variable
#'   `TEXTRA_API_KEY`.
#' @param secret The API secret. Defaults to the environment variable
#'   `TEXTRA_API_SECRET`.
#' @param name The login ID of the 'TexTra' account. Defaults to the
#'   environment variable `TEXTRA_NAME`.
#' @param api_name The name of the API to use. Defaults to "mt".
#'
#' @details
#' Storing the credentials in the environment rather than in your scripts
#' keeps them out of version control and out of shared files. Add the
#' following lines to your user `.Renviron` file, which
#' `usethis::edit_r_environ()` opens, and restart R.
#'
#' ```
#' TEXTRA_API_KEY=your_api_key
#' TEXTRA_API_SECRET=your_api_secret
#' TEXTRA_NAME=your_login_id
#' ```
#'
#' @return A list of parameters.
#'
#' @examples
#' \dontrun{
#' # Using the credentials stored in .Renviron (recommended).
#' params <- gen_params()
#'
#' # Or passing them explicitly.
#' key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
#' secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
#' name <- "login_ID"                                 # login_ID
#' params <- gen_params(key = key, secret = secret, name = name)
#' }
#'
#' @export
gen_params <- function(key = textra_env("TEXTRA_API_KEY"),
                       secret = textra_env("TEXTRA_API_SECRET"),
                       name = textra_env("TEXTRA_NAME"),
                       api_name = "mt"){
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
#' post_request(params, text = "Hello, world!")
#' }
#'
#' @export
post_request <- function(params, text){
  body <- c(params, list(text = text))
  res <- 
    httr::POST(
      url = paste0(base_url(), "/api/?"),
      body = body,
      encode = "form")
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
#' res <- post_request(params, "Hello world!")
#' translated <- extract_result(res)
#' }
#'
#' @export
extract_result <- function(res){
  res_list <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8"))
  translated <- res_list$resultset$result$text
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

#' Read a credential from an environment variable
#'
#' Internal helper. Fails with an actionable message when the variable is
#' unset, instead of letting an empty string reach the API.
#'
#' @param var The name of the environment variable.
#'
#' @return A character string containing the value of the variable.
#'
#' @noRd
textra_env <- function(var){
  value <- Sys.getenv(var)
  if(!nzchar(value)){
    stop("Environment variable '", var, "' is not set.
",
         "  Add it to your .Renviron file ",
         "(`usethis::edit_r_environ()` opens it) and restart R,
",
         "  or pass the value directly.",
         call. = FALSE)
  }
  return(value)
}
