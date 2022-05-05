Erik.Leppo@tetratech.com
2022-05-05

# Test environments
* local Win 10, R 4.2.0
* win-builder (release)
* win-builder (devel)

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
tools::dependsOnPkgs("baytrends")

There are currently no downstream dependencies for this package.
