# Generate parameters for API call

This function generates a list of parameters that can be used to make an
API call.

## Usage

``` r
gen_params(
  key = textra_env("TEXTRA_API_KEY"),
  secret = textra_env("TEXTRA_API_SECRET"),
  name = textra_env("TEXTRA_NAME"),
  api_name = "mt"
)
```

## Arguments

- key:

  The API key. Defaults to the environment variable `TEXTRA_API_KEY`.

- secret:

  The API secret. Defaults to the environment variable
  `TEXTRA_API_SECRET`.

- name:

  The login ID of the 'TexTra' account. Defaults to the environment
  variable `TEXTRA_NAME`.

- api_name:

  The name of the API to use. Defaults to "mt".

## Value

A list of parameters.

## Details

Storing the credentials in the environment rather than in your scripts
keeps them out of version control and out of shared files. Add the
following lines to your user `.Renviron` file, which
`usethis::edit_r_environ()` opens, and restart R.

    TEXTRA_API_KEY=your_api_key
    TEXTRA_API_SECRET=your_api_secret
    TEXTRA_NAME=your_login_id

## Examples

``` r
if (FALSE) { # \dontrun{
# Using the credentials stored in .Renviron (recommended).
params <- gen_params()

# Or passing them explicitly.
key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
name <- "login_ID"                                 # login_ID
params <- gen_params(key = key, secret = secret, name = name)
} # }
```
