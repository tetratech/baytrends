#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2020-11-09
# Erik.Leppo@tetratech.com
#
# CRAN message (2020-09-10) about archiving gdata package and thus baytrends.
# https://github.com/tetratech/baytrends/issues/56
# 
# Copied function code from gdata::nobs GitHub page and gdata::nobs help.
# https://github.com/cran/gdata/blob/master/R/nobs.R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# $Id: nobs.R 1799 2014-04-05 18:38:23Z warnes $
## Redefine here, so that the locally defined methods (particularly
## nobs.default) take precidence over the ones now defined in the
## stats package
# nobs <- function(object, ...)
#   UseMethod("nobs")
#   
#   nobs.default <- function(object, ...)
#   {
#     if(is.numeric(object) || is.logical(object))
#       sum( !is.na(object) )
#     else
#       stats::nobs(object, ...)
#   }
#   
#   
#   nobs.data.frame <- function(object, ...)
#     sapply(object, nobs.default)
#   
#   ## Now provided by 'stats' package, so provide alias to satisfy
#   ## dependencies
#   nobs.lm <- stats:::nobs.lm
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' 
#' @title Compute the Number of Non-Missing Observations
#' 
#' @description Compute the number of non-missing observations. 
#' Provides a 'default' method to handle vectors, and a method for data frames.
#' 
#' @details Calculate the number of observations in `object`.
#' 
#' * For numeric vectors, this is simply the number of non-NA elements, as 
#' computed by `sum(!is.na(object))`.
#' 
#' * For dataframe objects, the result is a vector containing the number of 
#' non-NA elementes of each column.
#' 
#' The `nobs` and `nobs.lm` functions defined in gtools are simply aliases for 
#' the functions in the base R `stats` package, provided for backwards 
#' compatibility.
#' 
#' `baytrends` borrowed `gdata::nobs` 'as is' to avoid being archived in 2020.
#' https://github.com/tetratech/baytrends/issues/56
#' 
#' @note 
#' The base R package `stats` now provides a S3 dispatch function for nobs, and
#' methods for for objects of classes "lm", "glm", "nls" and "logLik", as well
#' as a default method.
#' 
#' Since they provided a subset of the the functionality, the base method 
#' dispatch (nobs) function and method for "lm" objects (`nobs.lm`) are, as of 
#' gdata version 2.10.1, simply aliases for the equivalent functions in the base
#'  R `stats` package.
#' 
#' Since `gdata`'s default method (`nobs.default`) processes vectors and hands 
#' any other data/object types to `stats:::nobs.default`.
#' 
#' @author Gregory R. Warnes greg@warnes.net
#' 
#' @param object Target Object
#' @param ... Optional parameters (currently ignored)
#' 
#' @examples 
#' x <- c(1,2,3,5,NA,6,7,1,NA )
#' length(x)
#' nobs(x)
#' 
#' df <- data.frame(x=rnorm(100), y=rnorm(100))
#' df[1,1] <- NA
#' df[1,2] <- NA
#' df[2,1] <- NA
#' 
#' nobs(df)
#' 
#' fit <- lm(y ~ x, data=df)
#' nobs(fit)
#' 
#' @export
#nobs.default <- function(object, ...)
nobs <- function(object, ...){
  UseMethod("nobs")
}

#' @export
nobs.default <- function(object, ...){
  if(is.numeric(object) || is.logical(object))
    sum( !is.na(object) )
  else
    stats::nobs(object, ...)
}

#' @export
nobs.data.frame <- function(object, ...){
  sapply(object, nobs.default)
}

## Now provided by 'stats' package, so provide alias to satisfy
## dependencies
#' @export
nobs.lm <- stats:::nobs.lm

