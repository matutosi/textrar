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
  res <- api_post(params, text)
  translated <- api_extract(res)
  return(translated)
}

#' Get Access Token
#'
#' This function retrieves an access token 
#' from the API using the provided key and secret.
#' 
#' @inheritParams gen_params
#'
#' @return A character string containing the access token. An error is
#'   raised when 'TexTra' rejects the credentials.
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
  token_url <- paste0(api_base_url(), "/oauth2/token.php")
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
  if(!is.null(parsed$error)){
    stop("'TexTra' rejected the credentials: ", parsed$error,
         if(is.null(parsed$error_description)) ""
         else paste0(" (", parsed$error_description, ")"),
         call. = FALSE)
  }
  token <- parsed$access_token
  if(is.null(token) || !nzchar(token)){
    stop("'TexTra' returned no access token (HTTP ",
         httr::status_code(token_req), ").", call. = FALSE)
  }
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
#' @section Deprecated:
#' `post_request()` is a low level building block that was never meant to be
#' part of the public interface. It is deprecated as of textrar 0.9.0 and
#' will be removed in a future release. Use [textra()] instead. If you rely
#' on it to reach an endpoint that [textra()] does not cover, please open an
#' issue at <https://github.com/matutosi/textrar/issues> so that a supported
#' way of doing so can be provided.
#'
#' @examples
#' \dontrun{
#' post_request(params, text = "Hello, world!")
#' }
#'
#' @export
post_request <- function(params, text){
  deprecate_low_level("post_request()")
  return(api_post(params, text))
}

#' Send a POST request to the API
#'
#' Internal workhorse behind [post_request()].
#'
#' @inheritParams textra
#'
#' @return The response from the API.
#'
#' @noRd
api_post <- function(params, text){
  body <- c(params, list(text = text))
  res <-
    httr::POST(
      url = paste0(api_base_url(), "/api/?"),
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
#' @return A character string containing the translated text. An error is
#'   raised when the API reports a failure. Note that the API answers with
#'   HTTP 200 even when it fails, and reports the failure in
#'   `resultset$code`, so the status code alone cannot be relied upon.
#'
#' @section Deprecated:
#' `extract_result()` is a low level building block that was never meant to
#' be part of the public interface. It is deprecated as of textrar 0.9.0 and
#' will be removed in a future release. Use [textra()] instead. If you rely
#' on it, please open an issue at
#' <https://github.com/matutosi/textrar/issues>.
#'
#' @examples
#' \dontrun{
#' res <- post_request(params, "Hello world!")
#' translated <- extract_result(res)
#' }
#'
#' @export
extract_result <- function(res){
  deprecate_low_level("extract_result()")
  return(api_extract(res))
}

#' Extract Translated Text from Response
#'
#' Internal workhorse behind [extract_result()].
#'
#' @param res The response object returned by [api_post()].
#'
#' @return A character string containing the translated text.
#'
#' @noRd
api_extract <- function(res){
  res_list <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8"))
  check_api_code(res_list, httr::status_code(res))
  translated <- res_list$resultset$result$text
  if(is.null(translated)){
    stop("'TexTra' returned no translated text.", call. = FALSE)
  }
  return(translated)
}

#' Return the base URL for the API
#'
#' @return A string containing the base URL.
#'
#' @section Deprecated:
#' `base_url()` is a low level building block that was never meant to be part
#' of the public interface. It is deprecated as of textrar 0.9.0 and will be
#' removed in a future release. If you rely on it, please open an issue at
#' <https://github.com/matutosi/textrar/issues>.
#'
#' @examples
#' \dontrun{
#' base_url()
#' }
#' 
#' @export
base_url <- function(){
  deprecate_low_level("base_url()")
  return(api_base_url())
}

#' Return the base URL for the API
#'
#' Internal workhorse behind [base_url()].
#'
#' @return A string containing the base URL.
#'
#' @noRd
api_base_url <- function(){
  return("https://mt-auto-minhon-mlt.ucri.jgn-x.jp")
}

#' Warn that a low level function is deprecated
#'
#' Internal helper, so that the three deprecated functions say the same
#' thing.
#'
#' @param what The name of the deprecated function.
#'
#' @return Nothing, called for its warning.
#'
#' @noRd
deprecate_low_level <- function(what){
  .Deprecated(msg = paste0(
    what, " is deprecated as of textrar 0.9.0 and will be removed in a ",
    "future release.
",
    "  Use textra() instead. If you rely on ", what, ", please open an ",
    "issue at
  https://github.com/matutosi/textrar/issues"))
  return(invisible(NULL))
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

#' Stop when the API reports a failure
#'
#' Internal helper. The API answers with HTTP 200 even when it fails, and
#' reports the failure in `resultset$code` instead, so the status code alone
#' is not enough to tell success from failure.
#'
#' @param res_list The parsed body of the answer.
#' @param status The HTTP status code of the answer.
#'
#' @return `res_list`, invisibly.
#'
#' @noRd
check_api_code <- function(res_list, status = 200L){
  if(!is.null(res_list$error)){
    stop("'TexTra' returned an error: ", res_list$error,
         if(is.null(res_list$error_description)) ""
         else paste0(" (", res_list$error_description, ")"),
         call. = FALSE)
  }
  code <- res_list$resultset$code
  if(is.null(code)){
    if(status >= 400L){
      stop("'TexTra' answered with HTTP ", status, ".", call. = FALSE)
    }
    return(invisible(res_list))
  }
  if(!identical(as.integer(code), 0L)){
    api_message <- res_list$resultset$message
    stop("'TexTra' API error ", code,
         if(is.null(api_message) || !nzchar(api_message))
           " (the API sent no message)."
         else paste0(": ", api_message),
         call. = FALSE)
  }
  return(invisible(res_list))
}
