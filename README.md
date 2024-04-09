
<!-- README.md is generated from README.Rmd. Please edit that file -->

# textrar

The goal of textrar is to translate texts with TexTra easily.

<https://mt-auto-minhon-mlt.ucri.jgn-x.jp/>

## Installation

You can install from [GitHub](https://github.com/) with:

``` r
  # install.packages("remotes")
remotes::install_github("matutosi/textrar")
```

You can also install from CRAN:

``` r
install.packages("textrar")
```

## Example

You can use `textra()` to translate texts with textra easily.

``` r
library(textrar)

key <- "abcdefghijklmnopqrstuvw01234567890abcdef1" # API key
secret <- "xyzabcdefghijklmnopqrstuvw012345"       # API secret
name <- "login_ID"                                 # login_ID
params <- gen_params(key = key, secret = secret, name = name)

text <- "Hello world"
translated <- textra(text, params, model = "transLM", from = "en", to = "it")
translated
 ## [1] "Ciao mondo"
```

## Citation

Toshikazu Matsumura (2024) textrar. Interface to ‘TexTra’ from R.
<https://github.com/matutosi/textrar/>
