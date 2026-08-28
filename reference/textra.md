# Translate text with 'TexTra'

This function translates text with the 'TexTra' machine translation API.

## Usage

``` r
textra(text, params, model = "transLM", from = "en", to = "ja")
```

## Arguments

- text:

  The text to be translated.

- params:

  A list of parameters to be passed to the API.

- model:

  The model to be used for translation.

- from:

  The source language.

- to:

  The target language.

## Value

The translated text.

## Examples

``` r
if (FALSE) { # \dontrun{
text <- "Hello world"
key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
name <- "login_ID"                                 # login_ID
params <- gen_params(key = key, secret = secret, name = name)
translated <- 
  textra(text, params, model = "transLM", from = "en", to = "ja")
translated
} # }
```
