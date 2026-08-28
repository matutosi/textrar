# Submission

This is a package update (0.8.0 -> 0.9.0).

This version fixes a security issue: due to a broken `.Rbuildignore`
entry, the `tools/` directory (containing scripts with hard-coded API
credentials, used only for local development) was included in the
0.8.0 source package. This has been corrected, and the affected
credentials have been revoked and reissued.

This version also contains a breaking change: functions now raise an
error on API failure instead of returning `NULL` (the API can report a
failure with an HTTP 200 status, so the status code alone was not
sufficient to detect it). This is documented in NEWS.md. There are no
reverse dependencies on CRAN, so no other packages are affected.

# Test environments

* local
    * Windows 11, R 4.4.x
* devtools::check_win_devel()

# R CMD check results

0 errors | 0 warnings | 0 notes

# Downstream dependencies

There are currently no downstream dependencies for this package.
