
<!-- README.md is generated from README.Rmd. Please edit that file -->

# baytrends

<!-- badges: start -->
<!--
CRAN check ALL
[![cran checks](https://badges.cranchecks.info/summary/baytrends.svg)](https://cran.r-project.org/web/checks/check_results_baytrends.html) All flavors

[![R-CMD-check](https://github.com/tetratech/baytrends/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tetratech/baytrends/actions/workflows/R-CMD-check.yaml)
-->

[![CRAN
status](https://www.r-pkg.org/badges/version/baytrends)](https://cran.r-project.org/package=baytrends)
Windows [![cran
checks](https://badges.cranchecks.info/flavor/windows/baytrends.svg)](https://cran.r-project.org/web/checks/check_results_baytrends.html)
Linux [![cran
checks](https://badges.cranchecks.info/flavor/linux/baytrends.svg)](https://cran.r-project.org/web/checks/check_results_baytrends.html)
OSX [![cran
checks](https://badges.cranchecks.info/flavor/macos/baytrends.svg)](https://cran.r-project.org/web/checks/check_results_baytrends.html)

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/tetratech/baytrends/graphs/commit-activity)
<!-- badges: end --> <!-- CRAN version and downloads have errors -->

The baytrends package was developed to enable users to evaluate
long-term trends in the Chesapeake Bay using a Generalized Additive
Modeling (GAM) approach. The model development includes selecting a GAM
structure to describe nonlinear seasonally-varying changes over time,
incorporation of hydrologic variability via either a river flow or
salinity, the use of an intervention to deal with method or laboratory
changes suspected to impact data values, and representation of left- and
interval-censored data. This approach, which is fully transferable to
other systems, allows for Chesapeake Bay water quality data to be
evaluated in a statistically rigorous, yet flexible way to provide
insights to a range of management- and research-focused questions.

## Installation

The CRAN version of baytrends from [CRAN](https://CRAN.R-project.org)
can be installed with the code below.

``` r
install.packages("baytrends")
```

In some cases not all dependent packages are available on the user’s
system. In these cases installing all dependencies is necessary.

``` r
install.packages("baytrends", dependencies = TRUE)
```

The development version (with vignettes) from
[GitHub](https://github.com/) can be installed with the code example
below using the `remotes` package.

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
install_github("tetratech/baytrends", force = TRUE, build_vignettes = TRUE)
```
