#'Water-Quality Data
#'
#'Converts information from discrete water-quality samples to data of class "qw."
#'Attempts to construct the best-fit estimates for data of class "qw" from more limited 
#'data sources like just remark codes and values. But will also convert data created by 
#'\code{as.data.frame(object of class qw, expand=TRUE)}.
#'
#'The options for \code{scheme} are "booker," "qw," and "partial." The "booker" naming scheme
#'assumes that remark columns begin with "R" or "r" and the value columns begin with "P" or "p."
#'That scheme is the most limited form because it forces missing values or blanks for almost all other
#'of the meta data. The "qw" scheme assumes that the columns are named according to 
#'\code{as.data.frame(object of class qw, expand=TRUE)}. The "partial" scheme assumes that the basic 
#'scheme from \code{as.data.frame(object of class qw, expand=TRUE)} is retained, but not all 
#'columns are required. Variations include no suffix for the value and .dl
#'in lieu of .rlv for the detection limit. When .dl is used, the method detection limit is assumed 
#'to be "MDL" if not supplied. If the scheme is "partial," then at least the value and remark or 
#'detetion limit or reporting limit must be supplied.
#'
#'@param data a data frame that contains limited sample information.
#'@param scheme a character string that describes the naming style of the variables in
#'\code{data} that are to be conterted to class "qw." See \bold{Details}.
#'@return A data frame of the water-quality data of class "qw" organized by
#'sample. Column names for the water-quality constituents are generated 
#'automatically. Other columns are retained unchanged.
#'@seealso \code{\link{readNWISqw}}, \code{\link{makeColNames}}
#'@references Lorenz, D.L., 2014, smwrQW OFR.\cr See information about discrete
#'samples at \url{https://nwis.waterdata.usgs.gov/usa/nwis/qw}.
#'@keywords internal datasets IO
#'@examples
#'# Make a dummy partial dataset
#'tmp.dum <- data.frame(sta=c("A", "B"),
#'  Val=c(1, 1.2),
#'  Val.rmk=c("<", ""),
#'  Val.dl=c(1, 1), stringsAsFactors=FALSE)
#'convert2qw(tmp.dum, scheme="partial")
#'
#'@export
convert2qw <- function(data, scheme="booker") {
  ## Coding history:
  ##    2013May03 DLLorenz Original Coding
  ##    2013Jun05 DLLorenz Removed Stop if no scheme "qw" found
  ##
  scheme <- match.arg(scheme, c("booker", "qw", "partial"))
  dnames <- names(data)
  ## Note each section returns the data rather than returning at end.
  if(scheme == "booker") { # look for Ps and Rs
    Pnames <- substring(grep("^P", dnames, ignore.case=TRUE, value=TRUE), 2)
    Rnames <- substring(grep("^R", dnames, ignore.case=TRUE, value=TRUE), 2)
    Cnames <- intersect(Pnames, Rnames)
    if(length(Cnames) == 0)
      stop("No valid \"booker\" scheme column names")
    ## Extract the non qw data
    Keep <- dnames
    for(i in Cnames)
      Keep <- grep(paste(i, "$", sep=""), Keep, value=TRUE, invert=TRUE)
    retval <- data[, Keep, drop=FALSE]
    ## Now convert each name and add the column
    for(i in Cnames) {
    	Pname <- grep(paste("^P", i, sep=""), dnames, ignore.case=TRUE, value=TRUE)
    	Rname <- grep(paste("^R", i, sep=""), dnames, ignore.case=TRUE, value=TRUE)
    	qw <- as.qw(as.numeric(data[[Pname]]), as.character(data[[Rname]]), value.codes="", reporting.level=NA_real_,
    							reporting.method="", reporting.units="", analyte.method="", analyte.name="",
    							unique.code=i)
    	retval[[Pname]] <- qw
    }
    return(retval)
  } # End of booker
  if(scheme == "qw") {
  	Vnames <- grep("\\.va1$", dnames, value=TRUE)
  	Vnames <- substring(Vnames, 1L, nchar(Vnames) - 4L)
  	## Extract the non qw data
  	Keep <- dnames
  	for(i in Vnames)
      Keep <- grep(paste("^", i, sep=""), Keep, value=TRUE, invert=TRUE)
    retval <- data[, Keep, drop=FALSE]
    ## Now convert each name and add the column
    for(i in Vnames) {
    	mat <- cbind(values=as.numeric(data[[paste(i, "va1", sep=".")]]), 
    							 value2=as.numeric(data[[paste(i, "va2", sep=".")]]))
    	## as.characer protects against factors
    	qw <- new("qw", mat, 
    						remark.codes=as.character(data[[paste(i, "rmk", sep=".")]]),
    						value.codes=as.character(data[[paste(i, "vqc", sep=".")]]),
    						reporting.level=as.numeric(data[[paste(i, "rlv", sep=".")]]),
    						reporting.method=as.character(data[[paste(i, "rmt", sep=".")]]),
    						reporting.units=as.character(data[[paste(i, "unt", sep=".")]]),
    						analyte.method=as.character(data[[paste(i, "mth", sep=".")]]), 
    						analyte.name=as.character(data[[paste(i, "nam", sep=".")]]),
    						unique.code=as.character(data[[paste(i, "pcd", sep=".")]]), 
    						rounding=c(2L,3L),
    						names=as.character(seq(ncol(mat))))
    	retval[[i]] <- qw
    }
    return(retval)
  } # End of qw
  ## Only way to get here is for scheme = "partial"
  Vnames <- grep("\\.(rmk|rlv|dl)$", dnames, value=TRUE)
  if(length(Vnames) == 0)
  	stop("No valid \"partial\" scheme column names (ending in .rmk, .rlv, or .dl)")
  ## strip off suffixes and keep other columns
  Vnames <- unique(sub("\\.(rmk|rlv|dl)$", "", Vnames))
  Keep <- dnames
  for(i in Vnames)
    Keep <- grep(paste("^", i, sep=""), Keep, value=TRUE, invert=TRUE)
  retval <- data[, Keep, drop=FALSE]
  ## Now convert each name and add the column
  for(i in Vnames) {
    Val <- as.numeric(data[[i]])
    ## Detection or reporting limit (and set up method code)
    RMT <- ""
    if(!is.null(data[[paste(i, "dl", sep=".")]])) {
      RL <- as.numeric(data[[paste(i, "dl", sep=".")]])
      RMT <- "MDL"
    }
    else if(!is.null(data[[paste(i, "rlv", sep=".")]]))
      RL <- as.numeric(data[[paste(i, "rlv", sep=".")]])
    else
      RL <- NA_real_
    ## Remark codes
    if(!is.null(data[[paste(i, "rmk", sep=".")]]))
      RMK <- as.character(data[[paste(i, "rmk", sep=".")]])
    else
      RMK <- ifelse(Val < na2miss(RL, FALSE), "<", "")
    ## The rest
    if(!is.null(data[[paste(i, "vqc", sep=".")]]))
      VQC <- as.character(data[[paste(i, "vqc", sep=".")]])
    else
      VQC <- ""
    if(!is.null(data[[paste(i, "rmt", sep=".")]]))
      RMT <- as.character(data[[paste(i, "rmt", sep=".")]])
    if(!is.null(data[[paste(i, "unt", sep=".")]]))
      UNT <- as.character(data[[paste(i, "unt", sep=".")]])
    else
      UNT <- ""
    if(!is.null(data[[paste(i, "mth", sep=".")]]))
      MTH <- as.character(data[[paste(i, "mth", sep=".")]])
    else
      MTH <- ""
    if(!is.null(data[[paste(i, "nam", sep=".")]]))
      NAM <- as.character(data[[paste(i, "name", sep="")]])
    else
      NAM <- ""
    if(!is.null(data[[paste(i, "pcd", sep=".")]]))
      PCD <- as.character(data[[paste(i, "pcd", sep=".")]])
    else
      PCD <- ""
    qw <- as.qw(Val, RMK, 
      value.codes=VQC, 
      reporting.level=RL,
      reporting.method=RMT, 
      reporting.units=UNT, 
      analyte.method=MTH, 
      analyte.name=NAM,
      unique.code=PCD)
    retval[[i]] <- qw
  }
  return(retval)
}
