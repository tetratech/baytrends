#' Display Structure
#'
#' Displays the basic information about an object: methods for "lcens," "mcens," 
#'and "qw" data.
#'
#' @aliases str.lcens str.mcens str.qw
#' @param object an object of class "lcens,", "mcens," or "qw."
#' @param \dots any additional valid arguments ot the default method for \code{str} and
#'give.censoring, a logical value that includes the type of censoring in the output if TRUE.
#' @return Nothing is returned, the side effect is to print a short summary of the object.
#' @keywords internal
#' @seealso \code{\link[utils]{str}}
#' @examples
#'
#'str(as.lcens(c(1,3), 2))
#'
#' @importFrom utils str
#' @rdname str
#' @export
#' @method str lcens
str.lcens <- function (object, ...) {
  str.qw(object, ...)
}

#' @rdname str
#' @export
#' @method str mcens
str.mcens <- function (object, ...) {
  str.qw(object, ...)
}

#' @rdname str
#' @importFrom utils getS3method
#' @importFrom utils strOptions
#' @export
#' @method str qw
str.qw <- function (object, ...) {
	## Stolen from str.Date, with modifications for censoring
	cl <- oldClass(object)
	n <- length(object)
	if (n == 0L) {
		def <- getS3method("str", "default")
		return(def(object))
	}
	if (n > 1000L) 
		object <- object[seq_len(1000L)]
	give.length <- TRUE
	give.censoring <- TRUE
	if (length(larg <- list(...))) {
		nl <- names(larg)
		if(any(GC <- nl == "give.censoring"))
			give.censoring <- larg[[GC]]
		iGiveHead <- which(nl == "give.head")
		if (any(Bgl <- nl == "give.length")) 
			give.length <- larg[[which(Bgl)]]
		else if (length(iGiveHead)) 
			give.length <- larg[[iGiveHead]]
		if (length(iGiveHead)) 
			larg <- larg[-iGiveHead]
		if (is.numeric(larg[["nest.lev"]]) && is.numeric(v.len <- larg[["vec.len"]])) 
			larg[["vec.len"]] <- min(larg[["vec.len"]], (larg[["width"]] - 
																									 	nchar(larg[["indent.str"]]) - 31)%/%19)
	}
	le.str <- if (give.length) 
		paste0("[1:", as.character(n), "]")
	if(give.censoring) {
		cen <- censoring(object)
		if(cen == "none")
			cen <- "no"
		cat(" ", cl[1L], le.str, ", ", cen, " censoring: ", sep = "")
	} else
		cat(" ", cl[1L], le.str, sep = "")
	if(cl == "qw") {
		formObj <- format(object)
	} else {
		strO <- getOption("str")
		if (!is.list(strO)) 
			strO <- strOptions()
		digits <- strO$digits.d
		if(is.null(digits))
			digits <- 3
		formObj <- format(object, digits=digits)
	}
	do.call(str, c(list(formObj, give.head = FALSE), larg))
}
