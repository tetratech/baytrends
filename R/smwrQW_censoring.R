#' Describe Censoring
#'
#' Returns the type of censoring ("none," "left," "multiple") for an object.
#' 
#' Added from smwrQW.
#'
#' @aliases censoring censoring.default censoring.lcens censoring.mcens
#'censoring.qw
#' @param x the object to get the type of censoring.
#' @return A character string "none," "left," or "multiple" describing the type
#'of censoring present in the object.
#' @note This function is mostly used within other functions to determine the
#''best' technique to use for analysis.
# @seealso \code{\link{censorLevels}}
#' @keywords internal censored attribute
#' @examples
#'
#'censoring(2.3) # a simple number
#'censoring(as.lcens(1, 2)) # left-censored 
#'
#' @export
censoring <- function(x) {
  ## Coding history:
  ##    2012Sep17 DLLorenz Original coding
  ##    2013Jan01 DLLorenz Roxygenized
  ##    2013Jan01          This version
  ##
  UseMethod("censoring")
}

#' @rdname censoring
#' @export
#' @method censoring default
censoring.default <- function(x)
  "none"

#' @rdname censoring
#' @export
#' @method censoring lcens
censoring.lcens <- function(x) {
  if(any(x@censor.codes, na.rm=TRUE))
    "left"
  else
    "none"
}

#' @rdname censoring
#' @export
#' @method censoring mcens
censoring.mcens <- function(x) {
  if(any(x@censor.codes > 0, na.rm=TRUE) || any(x@interval, na.rm=TRUE))
    "multiple"
  else if(all(x@censor.codes == 0, na.rm=TRUE))
    "none"
  else
    "left"
}

#' @rdname censoring
#' @export
#' @method censoring qw
censoring.qw <- function(x) {
  if(any(x@remark.codes %in% c(">", "I"), na.rm=TRUE))
    "multiple"
  else if(any(x@remark.codes == "<", na.rm=TRUE))
    "left"
  else
    "none"
}
