# Build a minimal httr response, so that the parsing of the API answer can be
# tested without touching the network.
fake_response <- function(json, status = 200L){
  structure(
    list(url = "https://example.org/api/?",
         status_code = as.integer(status),
         headers = httr::insensitive(list(`content-type` = "application/json")),
         all_headers = list(),
         cookies = data.frame(),
         content = charToRaw(json),
         date = Sys.time(),
         times = numeric(),
         request = NULL,
         handle = NULL),
    class = "response")
}
