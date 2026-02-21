jon.harcum@tetratech.com
2026-02-21

# Test environments
* local Windows 11 x64 (build 26200), R 4.5.2 (ucrt)
* devtools::check()
* devtools::check(cran = TRUE)

# R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded locally under both:
* `devtools::check()`
* `devtools::check(cran = TRUE)`

## Error
zero

## Warnings
zero

## NOTE
zero

# Additional notes
* Vignettes rebuild successfully locally (Pandoc available).
* Ghostscript is installed and detected locally.
* A local `devtools`/`quarto` wrapper message may appear after checks complete on Windows
  (`Unknown command "TMPDIR=..."` from `quarto -V`), but `R CMD check` results are
  clean (0 errors / 0 warnings / 0 notes) and the package checks successfully.

# Downstream dependencies

devtools::revdep("baytrends")
character(0)

tools::dependsOnPkgs("baytrends")
[1] "baycluster"

There are currently no downstream CRAN dependencies for this package.

`tools::dependsOnPkgs()` identifies one locally installed downstream package (`baycluster`),
which is currently a GitHub-based package (not on CRAN).

