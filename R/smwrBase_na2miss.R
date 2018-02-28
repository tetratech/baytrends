#'Recode Data
#'
#'Converts missing values (\code{NA}s) to or from a user specified value.
#'From smwrBase package.
#'
#'
#' @aliases na2miss miss2na 
#' @param x a vector. Missing values (\code{NA}s) are allowed.
#' @param to the replacement value for \code{NA}.
#' @param from the target value to match and replace with \code{NA}.
#' @return An object like \code{x} with each target value replaced by the
#'specified value.
#' @note The function \code{na2miss} converts missing values (\code{NA}) to the
#'value \code{to} and is useful to prepare a vector for export and subsequent
#'use by software external to R that does not handle NAs.\cr The function
#'\code{miss2na} converts the value \code{from} to \code{NA} and can be used to
#'recode data imported from external software that uses a special value to
#'indicate missing values.\cr
# @seealso 
#Flip for production/manual
#\code{\link[base]{is.na}}, \code{\link[base]{sub}}
#\code{is.na}, \code{sub} (both in base package)
#' @keywords internal manip
#' @export
#' @examples
#'
#'## Construct simple substitutions
#'na2miss(c(1, 2, 3, NA, 5, 6))
na2miss <- function (x, to = -99999) {
  ## Coding history:
  ##    2000Oct26 JRSlack  Original coding.
  ##    2000Mar07 JRSlack  Renamed to na2miss from na2mv.
  ##    2011Jul01 DLLorenz Conversion to R (possiblility of character NAs)
  ##    2012Feb17 DLLorenz Added miss2na
  ##    2012Aug20 DLLorenz renamed args to match recode
  ##    2013Feb03 DLLoren Prep for gitHub.
  ##
  ## Special instructions for a factor:
  if(inherits(x, 'factor')) {
    levs <- c(levels(x), as.character(to))
    x <- as.vector(x)
    x[is.na(x)] <- to
    return(factor(x, levels=levs))
  } # Otherwise everything else is OK
  x[is.na(x)] <- to
  return(x)
}

#' @rdname na2miss
#' @export
miss2na <- function (x, from = -99999) {
  ## Special instructions for a factor:
  if(inherits(x, 'factor')) {
    levs <- levels(x)
	from <- as.character(from)
    levs <- levs[which(levs != from)]
    return(factor(x, levels=levs))
  } # Otherwise everything else is OK
  x[x == from] <- NA
  return(x)
}
