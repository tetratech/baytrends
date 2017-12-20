#' Invoke a Data Viewer
#' 
#' Invoke a spreadsheet-style data viewer on a matrix-like \code{R} object.
#' 
#' @param x an \code{R} object that can be forecd to a character representation
#'of a data frame.
#' @param title the title for the viewer window. Defaults to the name of \code{x}.
#' @return Invisibly returns \code{NULL}. The functions opens a widnow showing the
#' formatted contents of \code{x} and returns immediately.
#' @export
View <- function(x, title) {
	if(missing(title)) {
		title <- deparse(substitute(x))
	}
  # Start looking for View beyond where this one came from
  SP <- search()
  # Look for RStudio, else utils
  if("tools:rstudio" %in% SP) {
  	pos <- "tools:rstudio"
  } else {
  	pos <- "package:utils"
  }
  vf <- get("View", pos=pos, mode="function")
  invisible(vf(format(x), title=title))
}