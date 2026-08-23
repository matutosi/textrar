# Get Access Token

This function retrieves an access token from the API using the provided
key and secret.

## Usage

``` r
get_token(key, secret)
```

## Arguments

- key:

  The API key.

- secret:

  The API secret.

## Value

A character string containing the access token.

## Examples

``` r
if (FALSE) { # \dontrun{
key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
token <- get_token(key = key, secret = secret)
} # }
```
