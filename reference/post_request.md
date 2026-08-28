# Send a POST request to the API

This function sends a POST request to the API with the specified
parameters and text.

## Usage

``` r
post_request(params, text)
```

## Arguments

- params:

  A list of parameters to be passed to the API.

- text:

  The text to be translated.

## Value

The response from the API.

## Deprecated

`post_request()` is a low level building block that was never meant to
be part of the public interface. It is deprecated as of textrar 0.9.0
and will be removed in a future release. Use
[`textra()`](https://matutosi.github.io/textrar/reference/textra.md)
instead. If you rely on it to reach an endpoint that
[`textra()`](https://matutosi.github.io/textrar/reference/textra.md)
does not cover, please open an issue at
<https://github.com/matutosi/textrar/issues> so that a supported way of
doing so can be provided.

## Examples

``` r
if (FALSE) { # \dontrun{
post_request(params, text = "Hello, world!")
} # }
```
