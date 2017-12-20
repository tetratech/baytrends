#'Length of an Object
#'
#'Gets the length of a vector: methods for "lcens," "mcens," and "qw" data.
#'
#' @aliases length.lcens length.mcens length.qw
#' @usage \method{length}{lcens}(x)
#'
#'\method{length}{mcens}(x)
#'
#'\method{length}{qw}(x)
#' @param x a censored-data or water-quality object.
#' @return An integer of length 1 indicating the number of elements in \code{x}.
#' @keywords attribute
#' @examples
#'
#'length(as.lcens(c(1,3, NA), 2))
#'
#' @rdname length
#' @export
#' @method length lcens
length.lcens <- function(x)
  nrow(x@.Data)

#' @rdname length
#' @export
#' @method length mcens
length.mcens <- function(x)
  nrow(x@.Data)

#' @rdname length
#' @export
#' @method length qw
length.qw <- function(x)
  nrow(x@.Data)
