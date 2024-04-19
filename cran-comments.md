# Test environments

* local
    * Windows 11, R 4.3.3
    * Mac OS 11 Big Sur, R 4.3.3
* devtools::check_win_devel()
* rhub::rc_submit()
    * ubuntu-latest on GitHub
    * macos-latest on GitHub
    * windows-latest on GitHub

# R CMD check results

## On local check and rhub::check_rhub()

There were 0 errors  | 0 warnings  | 0 notes

## devtools::check_win_devel()

There were 0 errors  | 0 warnings  | 1 notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Toshikazu Matsumura <matutosi@gmail.com>'
  New submission
  Possibly misspelled words in DESCRIPTION:
    TexTra (10:5)

# Downstream dependencies

There are currently no downstream dependencies for this package.
