#'Describe Censoring
#'
#'Returns all censoring levels for an object.
#'
#'
#' @aliases censorLevels censorLevels.default censorLevels.lcens
#'censorLevels.mcens censorLevels.qw
#' @param x the object to get the levels of censoring.
#' @param type a character string indicating the type of censoring levels, must
#'be either "left" or "right."
#' @return A numeric vector of the sorted censoring levels. If all data are
#'uncensored, then -Inf is returned for \code{type} = "left" and Inf is
#'returned for \code{type} = "right."
#' @note Interval censoring is not characterized by a single value, so it is not
#'an option for \code{type}.
#' @seealso \code{\link{censoring}}
#' @keywords censored attribute
#' @examples
#'
#'censorLevels(2.3) # a simple number
#'censorLevels(as.lcens(1, 2)) # left-censored 
#'
#' @rdname censorLevels
#' @export
censorLevels <- function(x, type="left") {
  ## Coding history:
  ##    2012Sep17 DLLorenz Original coding
  ##    2013Jan01 DLLorenz Roxygenized
  ##    2013Jan01          This version
  ##
  UseMethod("censorLevels")
}

#' @rdname censorLevels
#' @export
#' @method censorLevels default
censorLevels.default <- function(x, type="left") {
  type <- match.arg(type, c("left", "right"))
  if(type == "left")
    -Inf
  else
    Inf
}

#' @rdname censorLevels
#' @export
#' @method censorLevels lcens
censorLevels.lcens <- function(x, type="left") {
  type <- match.arg(type, c("left", "right"))
  if(type == "right")
    retval <- Inf
  else {
    retval <- sort(unique(x@.Data[x@censor.codes,2]))
    if(length(retval) == 0)
      retval <- -Inf
  }
  return(retval)
}

#' @rdname censorLevels
#' @export
#' @method censorLevels mcens
censorLevels.mcens <- function(x, type="left") {
  type <- match.arg(type, c("left", "right"))
  if(type == "right") {
    retval <- sort(unique(x@.Data[x@censor.codes == 1, 1]), ascending=FALSE)
    if(length(retval) == 0)
      retval <- Inf
  }
  else {
    retval <- sort(unique(x@.Data[x@censor.codes == -1, 2]))
    if(length(retval) == 0)
      retval <- -Inf
  }
  return(retval)  
}

#' @rdname censorLevels
#' @export
#' @method censorLevels qw
censorLevels.qw <- function(x, type="left") {
  type <- match.arg(type, c("left", "right"))
  censorLevels(as.mcens(x), type=type)
}
