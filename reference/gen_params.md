# Generate parameters for API call

This function generates a list of parameters that can be used to make an
API call.

## Usage

``` r
gen_params(key, secret, name, api_name = "mt")
```

## Arguments

- key:

  The API key.

- secret:

  The API secret.

- name:

  The name of the API.

- api_name:

  The name of the API to use. Defaults to "mt".

## Value

A list of parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
name <- "login_ID"                                 # login_ID
params <- gen_params(key = key, secret = secret, name = name)
} # }
```
