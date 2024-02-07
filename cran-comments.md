Erik.Leppo@tetratech.com
2024-02-07

# Test environments
* local Win 10, R 4.3.2
* win-builder (release)
* win-builder (devel)
* r-hub

# R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖
R CMD check succeeded

## Error
zero

## Warnings
zero

## NOTE
❯ checking sizes of PDF files under 'inst/doc' ... NOTE
  Unable to find GhostScript executable to run checks on size reduction

# Downstream dependencies

devtools::revdep("baytrends")
character(0)

tools::dependsOnPkgs("baytrends")
character(0)

There are currently no downstream CRAN dependencies for this package.
