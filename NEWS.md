# textrar release news

# textrar 0.9.0

* Fixed `.Rbuildignore` so that the `tools/` directory is no longer shipped in
  the source package.
* Enabled TLS certificate verification: `get_token()` and `post_request()` no
  longer set `ssl_verifypeer = FALSE`.
* The responses are now parsed with plain `$` extraction instead of the
  `` `$`(_, "x") `` pipe placeholder. The behavior is unchanged, but the
  package no longer needs R 4.2: `DESCRIPTION` declares R >= 3.6, which is
  what 'httr' asks for. The unused `LazyData` field was removed.
* `gen_params()` and `get_token()` now read the credentials from the
  environment variables `TEXTRA_API_KEY`, `TEXTRA_API_SECRET` and
  `TEXTRA_NAME` by default, so that keys need not be written in scripts.
  Passing them explicitly still works. An unset variable now raises an
  informative error instead of sending an empty credential.
* **Breaking**: failures now raise an error instead of returning `NULL`.
  `get_token()` stops when 'TexTra' rejects the credentials, and
  `extract_result()` stops when the API reports a failure. The API answers
  with HTTP 200 even when it fails and reports the failure in
  `resultset$code`, so the status code alone was never enough to detect it.
  Code that tested the result with `is.null()` needs to use `tryCatch()`.
* `post_request()`, `extract_result()` and `base_url()` are deprecated. They
  are low level building blocks that were never meant to be part of the
  public interface. They still work, but warn, and will be removed in a
  future release. Use `textra()` instead; if you rely on them to reach an
  endpoint that `textra()` does not cover, please open an issue at
  <https://github.com/matutosi/textrar/issues>.
* Added a `testthat` (edition 3) suite. Most tests run without touching the
  network; the ones that call the API are skipped unless `TEXTRA_TEST_LIVE`
  is set, so that they do not consume the API quota on every check.
* Documentation fixes: `textra()` is a translation model, not a
  transliteration one; `name` in `gen_params()` is the login ID; broken
  examples in `post_request()` and `extract_result()` were corrected.

# textrar 0.8.0

* First release
* `textra()` : main function to translate with TexTra
* `gen_params()`: function to get token parameters
