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

A character string containing the translated text. An error is raised
when the API reports a failure. Note that the API answers with HTTP 200
even when it fails, and reports the failure in `resultset$code`, so the
status code alone cannot be relied upon.

## Deprecated

`extract_result()` is a low level building block that was never meant to
be part of the public interface. It is deprecated as of textrar 0.9.0
and will be removed in a future release. Use
[`textra()`](https://matutosi.github.io/textrar/reference/textra.md)
instead. If you rely on it, please open an issue at
<https://github.com/matutosi/textrar/issues>.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- post_request(params, "Hello world!")
translated <- extract_result(res)
} # }
```
