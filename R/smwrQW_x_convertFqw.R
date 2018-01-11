#' Water-Quality Data
#'
#' Converts information from data of class "qw" to separate columns representing the data in
#'the "qw" columns. This can facilitate transfer to other systems or editing the meta data
#'in a "qw" column. Uses \code{as.data.frame(object of class qw, expand=TRUE)} to convert each
#'"qw" column.
#'
#' @param data a data frame that contains limited sample information.
#' @param columns a character vector listing the names of the columns to convert. If
#'a single empty character string, the default, then convert all "qw" columns.
#' @return A data frame of the converted water-quality data and all other columns in the
#'original dataset.
#' @note This function can be used in conjunction with \code{convert2qw} to update or
#'modify reporting levels or inspect the details of the meta data.
#' @seealso \code{\link{readNWISqw}}, \code{\link{convert2qw}}
#' @references Lorenz, D.L., 2014, USGSwsQW OFR.\cr See information about discrete
#'samples at \url{https://nwis.waterdata.usgs.gov/usa/nwis/qw}.
#' @keywords internal datasets IO
#' @examples
#'# Make a dummy partial dataset
#'tmp.dum <- data.frame(sta=c("A", "B"),
#'  Val=c(1, 1.2),
#'  Val.rmk=c("<", ""),
#'  Val.dl=c(1, 1), stringsAsFactors=FALSE)
#'tmp.qw <- convert2qw(tmp.dum, scheme="partial")
#'# Expand the result
#'convertFqw(tmp.qw)
#'@export
convertFqw <- function(data, columns="") {
  ## Coding history:
  ##    2014Jul18 DLLorenz Original Coding
  ##
  # Get the columns to convert
	if(length(columns) == 1L && columns == "")
		columns <- names(data)[sapply(data, function(d) inherits(d, "qw"))]
	tmp <- list()
	for(i in names(data)) {
		tmp[[i]] <- if(i %in% columns) {
			as.data.frame(data[[i]], nm="J_U_N_K", expand=TRUE) # Need to remove later
		} else
			data[, i, drop=FALSE]
	}
	# now pack together is a dataset
	retval <- do.call(cbind, tmp)
	# Remove the junk in the name
	names(retval) <- sub(".J_U_N_K.", ".", names(retval), fixed=TRUE)
  return(retval)
}
