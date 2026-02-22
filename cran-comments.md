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

## Additional notes

* Unit tests pass locally (14 tests, 0 failures).
* Vignettes rebuild successfully locally (Pandoc available).
* Ghostscript is installed and detected locally.
* On local Windows runs, a `devtools`/`quarto` wrapper message may appear after check completion:
  `Unknown command "TMPDIR=..."` from `quarto -V`.
  This occurs after `R CMD check` has already completed and reported clean results.

* This submission is being made by a co-author/collaborator on behalf of the designated maintainer, Erik W. Leppo, who remains the package maintainer in `DESCRIPTION` and will confirm the CRAN submission email.

## Downstream dependencies

There are currently no downstream CRAN dependencies for this package.

`tools::dependsOnPkgs("baytrends")` identifies one locally installed downstream package (`baycluster`), which is currently GitHub-based (not on CRAN).




# Comment

This submission updates baytrends (version 2.0.13). Local checks were run on
Windows 11 x64 with R 4.5.2 (ucrt), including R CMD check --as-cran, and
completed with 0 errors, 0 warnings, and 0 notes. GitHub Actions CI also passed
on macOS (release), Windows (release), Ubuntu (devel), Ubuntu (release), and
Ubuntu (oldrel-1). This submission is being made by a co-author/collaborator on
behalf of the designated maintainer, Erik W. Leppo, who remains the package
maintainer in DESCRIPTION and will confirm the CRAN submission email.

baytrends 2.0.13 is a maintenance/update release focused on package robustness
and release-readiness, including updates related to USGS flow
handling/retrieval, documentation/vignette build behavior, and CRAN compliance
cleanup (metadata/checks).

