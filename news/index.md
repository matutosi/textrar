# Changelog

## textrar 0.9.0

- Fixed `.Rbuildignore` so that the `tools/` directory is no longer
  shipped in the source package.
- Enabled TLS certificate verification:
  [`get_token()`](https://matutosi.github.io/textrar/reference/get_token.md)
  and
  [`post_request()`](https://matutosi.github.io/textrar/reference/post_request.md)
  no longer set `ssl_verifypeer = FALSE`.
- The responses are now parsed with plain `$` extraction instead of the
  `` `$`(_, "x") `` pipe placeholder. The behavior is unchanged, but the
  package no longer needs R 4.2: `DESCRIPTION` declares R \>= 3.6, which
  is what ‘httr’ asks for. The unused `LazyData` field was removed.
- [`gen_params()`](https://matutosi.github.io/textrar/reference/gen_params.md)
  and
  [`get_token()`](https://matutosi.github.io/textrar/reference/get_token.md)
  now read the credentials from the environment variables
  `TEXTRA_API_KEY`, `TEXTRA_API_SECRET` and `TEXTRA_NAME` by default, so
  that keys need not be written in scripts. Passing them explicitly
  still works. An unset variable now raises an informative error instead
  of sending an empty credential.
- **Breaking**: failures now raise an error instead of returning `NULL`.
  [`get_token()`](https://matutosi.github.io/textrar/reference/get_token.md)
  stops when ‘TexTra’ rejects the credentials, and
  [`extract_result()`](https://matutosi.github.io/textrar/reference/extract_result.md)
  stops when the API reports a failure. The API answers with HTTP 200
  even when it fails and reports the failure in `resultset$code`, so the
  status code alone was never enough to detect it. Code that tested the
  result with [`is.null()`](https://rdrr.io/r/base/NULL.html) needs to
  use [`tryCatch()`](https://rdrr.io/r/base/conditions.html).
- [`post_request()`](https://matutosi.github.io/textrar/reference/post_request.md),
  [`extract_result()`](https://matutosi.github.io/textrar/reference/extract_result.md)
  and
  [`base_url()`](https://matutosi.github.io/textrar/reference/base_url.md)
  are deprecated. They are low level building blocks that were never
  meant to be part of the public interface. They still work, but warn,
  and will be removed in a future release. Use
  [`textra()`](https://matutosi.github.io/textrar/reference/textra.md)
  instead; if you rely on them to reach an endpoint that
  [`textra()`](https://matutosi.github.io/textrar/reference/textra.md)
  does not cover, please open an issue at
  <https://github.com/matutosi/textrar/issues>.
- Added a `testthat` (edition 3) suite. Most tests run without touching
  the network; the ones that call the API are skipped unless
  `TEXTRA_TEST_LIVE` is set, so that they do not consume the API quota
  on every check.
- Documentation fixes:
  [`textra()`](https://matutosi.github.io/textrar/reference/textra.md)
  is a translation model, not a transliteration one; `name` in
  [`gen_params()`](https://matutosi.github.io/textrar/reference/gen_params.md)
  is the login ID; broken examples in
  [`post_request()`](https://matutosi.github.io/textrar/reference/post_request.md)
  and
  [`extract_result()`](https://matutosi.github.io/textrar/reference/extract_result.md)
  were corrected.

## textrar 0.8.0

CRAN release: 2024-04-23

- First release
- [`textra()`](https://matutosi.github.io/textrar/reference/textra.md) :
  main function to translate with TexTra
- [`gen_params()`](https://matutosi.github.io/textrar/reference/gen_params.md):
  function to get token parameters
