#'Partial Value Matching
#'
#'Matches partial values, such as substrings.
#'
#' Added from smwrBase.
#' 
#' @rdname zcn
#' @usage x \%cn\% pattern
#' @param x the character vector to be matched. Missing values are permitted.
#' @param pattern the pattern to be matched against, may be a regular
#'expression.
#' @return A vector the same length as \code{x} of logical values indicating
#'whether \code{pattern} is found in the element of \code{x} or not.
# baytrends, comment out line below
# @seealso 
#Flip for production/manual
#\code{\link[base]{\%in\%}}, \code{\link[base]{regexpr}}
#\code{\%in\%}, \code{regexpr} (both in base package)
#' @keywords internal manip
#' @examples
#'
#'## A simple example
#'c("abc", "def") %cn% "c"
#' @export
"%cn%" <- function(x, pattern)
  ## Coding history:
  ##    2009Feb13 DLLorenz Original Coding
  ##    2011Apr26 DLLorenz Conversion to R
  ##    2013Feb02 DLLorenz Prep for gitHub
  sapply(x, function(X, pattern)
         regexpr(pattern, X), pattern = pattern) > 0
