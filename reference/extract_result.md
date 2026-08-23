# Extract Translated Text from Response

This function extracts the translated text from the response. returned
by the
[`post_request()`](https://matutosi.github.io/textrar/reference/post_request.md)
function.

## Usage

``` r
extract_result(res)
```

## Arguments

- res:

  The response object returned by the
  [`post_request()`](https://matutosi.github.io/textrar/reference/post_request.md).

## Value

A character string containing the translated text.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- post_request(paramas, "Hello world!")
translated <- extract_result(res)
} # }
```
