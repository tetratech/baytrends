#' Test whether an object can be treated in a particular way
#'
#' Tests if an object can be treated as a character, to name something; as a
#'date; as a grouping variable, has distinct values; or as a number.
#'
#'The function \code{isCharLike} tests whether \code{x} is of class "character"
#'or "factor." The function \code{isDateLike} tests whether \code{x} is of
#'class "Date" or "POSIXt." The function \code{isGroupLike} tests whether
#'\code{x} is of class "character" or "factor" or if \code{x} is of type
#'"integer" or "logical." The function \code{isNumberLike} tests whether
#'\code{x} is of type "numeric" or of class "Date."
#'
#' Added from smwrBase.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# document @rdname with multiple.  Didn't work the same as smwrBase.
# added @name NULL and removed first @rdname
# https://r-pkgs.org/man.html#multiple-man
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' 
# @rdname isLike
#' @aliases isCharLike isDateLike isGroupLike isNumberLike
#' @param x any object.
#' @name isLike
NULL
#' @return A logical value \code{TRUE} if \code{x} meets the criteria, or
#'\code{FALSE} if it does not.
#' @note This function is most useful within other functions to control how that
#'function handles a particular argument.
# @seealso 
#Flip for production/manual
# baytrends, comment out lines below
#\code{\link[base]{class}}, \code{\link[base]{is.numeric}},
#\code{\link[base]{is.factor}}, \code{\link[base]{is.character}},
#\code{\link[base]{is.integer}}, \code{\link[base]{is.logical}}
#\code{class}, \code{is.numeric}, \code{is.factor}, \code{is.character},
#\code{is.integer}, \code{is.logical} (all in base package)


#' @rdname isLike
#' @keywords internal
#' @examples
#'# The first should be FALSE and the second TRUE
#'isCharLike(as.Date("2004-12-31"))
#'isCharLike("32")
#' @export
isCharLike <- function(x)
  is.factor(x) || is.character(x)

#' @rdname isLike
#' @keywords internal manip
#' @examples
#'# The first should be FALSE and the second TRUE
#'isDateLike(32)
#'isDateLike(as.Date("2004-12-31"))
#' @export
isDateLike <- function(x) {
  ## some objects may have multiple classes, so any is needed
  any(class(x) %in% c("Date", "POSIXt"))
}

#' @rdname isLike
#' @keywords internal
#' @examples
#'# The first should be FALSE and the second TRUE
#'isGroupLike(as.Date("2004-12-31"))
#'isGroupLike(32)
#' @export
isGroupLike <- function(x) {
  is.factor(x) || is.character(x) || is.integer(x) ||  is.logical(x)
}

#' @rdname isLike
#' @examples
#'# The first should be FALSE and the second TRUE
#'isNumberLike(as.Date("2004-12-31"))
#'isNumberLike(32)
#' @export
isNumberLike <- function(x) {
  class(x)[1L] %in% c("integer", "numeric")
}
