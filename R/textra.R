  ## TexTra
  # https://mt-auto-minhon-mlt.ucri.jgn-x.jp/
  # https://mt-auto-minhon-mlt.ucri.jgn-x.jp/api/mt/transLM_ja_en/


## TexTra

#' @examples
#' 
#' key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
#' secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
#' name <- "login_ID"                                 # login_ID
#' params <- gen_params(key = key, secret = secret, name = name)
#' 
#' text <- "text to be translated"
#' textra(text = text, params = params, 
#'   model = "transLM", from = "en", to = "ja")
#' 
#' @export
textra <- function(text, params, model = "transLM", from = "en", to = "ja"){
  api_param <- paste0(model, "_", from, "_", to)
  params <- c(params, list(api_param = api_param))
  res <- post_request(params, text)
  translated <- extract_result(res)
  return(translated)
}


#' Return baseURL character.
#' @examples
#' base_url()
#' 
#' @export
base_url <- function(){
  return("https://mt-auto-minhon-mlt.ucri.jgn-x.jp")
}


#' get token
#' 
#' @examples
#' 
#' key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
#' secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
#' token <- get_token(key = key, secret = secret)
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


#' Generate parameters for TexTra
#' 
#' @examples
#' 
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
