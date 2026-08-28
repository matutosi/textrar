# Get Access Token

This function retrieves an access token from the API using the provided
key and secret.

## Usage

``` r
get_token(
  key = textra_env("TEXTRA_API_KEY"),
  secret = textra_env("TEXTRA_API_SECRET")
)
```

## Arguments

- key:

  The API key. Defaults to the environment variable `TEXTRA_API_KEY`.

- secret:

  The API secret. Defaults to the environment variable
  `TEXTRA_API_SECRET`.

## Value

A character string containing the access token. An error is raised when
'TexTra' rejects the credentials.

## Examples

``` r
if (FALSE) { # \dontrun{
# Using the credentials stored in .Renviron (recommended).
token <- get_token()

# Or passing them explicitly.
key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
token <- get_token(key = key, secret = secret)
} # }
```
