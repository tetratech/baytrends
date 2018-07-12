Erik.Leppo@tetratech.com
2018-07-12

# Test environments
* local Win 7, R 3.4.4
* win-builder (release)

# R CMD check results

0 errors | 0 warnings | 1 note 

Don't need to fix.

## Error
zero

## Warnings
zero

## NOTE
One via RStudio Check

> check_failures(dir.check)
[1] "checking sizes of PDF files under 'inst/doc' ... 
NOTE\n Unable to find GhostScript executable to run checks on size reduction"

ignore.  qpdf not installing properly but not a real issue.

One via win-builder

Possibly mis-spelled words in DESCRIPTION:
  hydrologic (7:22)
  
  ignore.  spelled and used properly.

# Downstream dependencies
Run R CMD check on downstream dependencies of httr 
(https://github.com/wch/checkresults/blob/master/httr/r-release). 

There are currently no downstream dependencies for this package.