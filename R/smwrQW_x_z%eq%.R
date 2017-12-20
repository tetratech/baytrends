#' Possible Equality
#' 
#' Checks for the possibility that two values are equal. If there is any overlap
#'in range, they are considered possibly equal and \code{TRUE} is returned, otherwise
#'\code{FALSE} is returned.
#'
#' Missing values are permitted in either \code{e1} or \code{e2}. wherever either 
#'are missing the returned element is set to \code{NA}.
#'
#' @rdname zeq
#' @usage e1 \%~=\% e2
#' @param e1 any vector that can be converted to class "mcens."
#' @param e2 any vector that can be converted to class "mcens."
#' @return A logical vector that matches the length of the longer of \code{e1} or 
#'\code{e2}, set to \code{TRUE} is there is overlap in the values, and \code{FALSE}
#'otherwise.
#' @seealso \code{\link{Compare,qw,numeric-method}}
#' @keywords manip
#' @examples
#' as.mcens(1, 2) %~=% 0.5
#' as.mcens(1, 2) %~=% 1.5
#' as.mcens(1, 2) %~=% 2.5
#' @export
"%~=%" <- function(e1, e2) {
	if(inherits(e1, "qw")) {
		e1 <- qw2mcens(e1)
	} else
		e1 <- as.mcens(e1)
	if(inherits(e2, "qw")) {
		e2 <- qw2mcens(e2)
	} else
		e2 <- as.mcens(e2)
	return(as.vector((e1@.Data[, 1L] <= e2@.Data[, 2L]) & (e1@.Data[, 2L] >= e2@.Data[, 1L])))
}
