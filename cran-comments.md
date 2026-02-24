## Test environments

* Local Windows 11 x64 (build 26200), R 4.5.2 (ucrt)
  - `R CMD build`
  - `R CMD check --as-cran`
  - `devtools::check()`
  - `devtools::check(cran = TRUE)`

* GitHub Actions CI (R-CMD-check)
  - macOS (release)
  - Windows (release)
  - Ubuntu (devel)
  - Ubuntu (release)
  - Ubuntu (oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

Local `R CMD check --as-cran` completed successfully on Windows.
GitHub Actions CI also completed successfully across the matrix listed above.

WinBuilder
- release - ok
- dev - ok
- old - produced Note about Authors and Authors@R fields not matching despite 
other checks being ok.  Checked format and DESCRIPTION using correct format.

## Additional notes

* Unit tests pass locally (14 tests, 0 failures).
* Vignettes rebuild successfully locally (Pandoc available).
* On local Windows runs, a `devtools`/`quarto` wrapper message may appear after check completion:
  `Unknown command "TMPDIR=..."` from `quarto -V`.
  This occurs after `R CMD check` has already completed and reported clean results.

## Downstream dependencies

There are currently no downstream CRAN dependencies for this package.

`tools::dependsOnPkgs("baytrends")` identifies one locally installed downstream package (`baycluster`), which is currently GitHub-based (not on CRAN).

# Comment

This is a resubmission of baytrends with version incremented to 2.0.14 to
address CRAN feedback on an invalid URL. The previously flagged pkgdown site URL
(https://tetratech.github.io/baytrends/) is now live and publicly resolves. The
URL was removed out of an abundance of caution.

Local R CMD check --as-cran completed cleanly (0 errors, 0 warnings, 0 notes),
and GitHub Actions CI passed across the existing matrix (macOS release, Windows
release, Ubuntu devel/release/oldrel-1). 

baytrends 2.0.14 is a maintenance/update release focused on package robustness
and release-readiness, including updates related to USGS flow
handling/retrieval, documentation/vignette build behavior, and CRAN compliance
cleanup (metadata/checks).

