# textrar release news

# textrar 0.8.0.9000 (development)

* Fixed `.Rbuildignore` so that the `tools/` directory is no longer shipped in
  the source package.
* Enabled TLS certificate verification: `get_token()` and `post_request()` no
  longer set `ssl_verifypeer = FALSE`.
* `DESCRIPTION` now declares the R version the code actually needs
  (R >= 4.2.0), and the unused `LazyData` field was removed.
* Documentation fixes: `textra()` is a translation model, not a
  transliteration one; `name` in `gen_params()` is the login ID; broken
  examples in `post_request()` and `extract_result()` were corrected.

# textrar 0.8.0

* First release
* `textra()` : main function to translate with TexTra
* `gen_params()`: function to get token parameters
