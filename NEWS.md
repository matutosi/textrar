# textrar release news

# textrar 0.8.0.9000 (development)

* Fixed `.Rbuildignore` so that the `tools/` directory is no longer shipped in
  the source package.
* Enabled TLS certificate verification: `get_token()` and `post_request()` no
  longer set `ssl_verifypeer = FALSE`.

# textrar 0.8.0

* First release
* `textra()` : main function to translate with TexTra
* `gen_params()`: function to get token parameters
