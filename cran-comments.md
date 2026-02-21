jon.harcum@tetratech.com
2026-02-21

# Test environments
* local Windows 11 x64 (build 26200), R 4.5.2 (ucrt)
* devtools::check()
* devtools::check(cran = TRUE)
* GitHub Actions CI:
  - macOS (release)
  - Windows (release)
  - Ubuntu (devel)
  - Ubuntu (release)
  - Ubuntu (oldrel-1)

# R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded locally and in GitHub Actions CI.

## Error
zero

## Warnings
zero

## NOTE
zero

# Additional notes
* Unit tests pass locally (14 tests, 0 failures).
* Vignettes rebuild successfully locally (Pandoc available).
* Ghostscript is installed and detected locally.
* On local Windows runs, a `devtools`/`quarto` wrapper message may appear **after** check completion:
  `Unknown command "TMPDIR=..."` from `quarto -V`.
  This appears after `R CMD check` has already completed and reported clean results
  (0 errors / 0 warnings / 0 notes).

# Downstream dependencies

devtools::revdep("baytrends")
character(0)

tools::dependsOnPkgs("baytrends")
[1] "baycluster"

There are currently no downstream CRAN dependencies for this package.

`tools::dependsOnPkgs()` identifies one locally installed downstream package (`baycluster`),
which is currently a GitHub-based package (not on CRAN).