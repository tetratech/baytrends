#'Column Names
#'
#'Creates valid, unique column names for water-quality constituents.
#'
#'The arguments \code{name} and \code{short} are expected to be columns
#'retreived by those arguments in the function \code{pcodeNWISqw}.
#'
#'@param params the parameter codes.
#'@param name the long-name of the parameter code.
#'@param short the short name of the parameter code.
#'@return A data frame containing the columns \code{parm_cd} and
#'\code{col_name}.
#'@note This function is a support function designed to be called within other
#'functions.
#@seealso \code{\link{pcodeNWISqw}}, \code{\link{pcodeColData}}
#'@keywords internal utilities
#'@export
makeColNames <- function(params, name, short) {
  ## Coding history:
  ##    2012Sep11 DLLorenz original Coding
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2012Dec28          This version
  ##
	## Get the predefined names 
	pcode.ColNames <- get("pcodeColData")
	## Force names to be valid and unique
	pcode.ColNames <- pcode.ColNames[pcode.ColNames$data_type %in% c("qw", "numeric"),  c(1L,2L)]
	pcode.ColNames$col_name <- make.names(pcode.ColNames$col_name, unique=TRUE)
  ## Are we done?
  done <- params %in% pcode.ColNames$parm_cd
  retval <- pcode.ColNames[pcode.ColNames$parm_cd %in% params,]
  if(all(done))
    return(retval)
  ## Generate new names
  todo <- !done
  name <- name[todo]
  short <- short[todo]
  params <- params[todo]
  col_name <- character(sum(todo))
  ## If short is blank, then generate name from the first 2 entries in name
  toshort <- short == ""
  if(any(toshort)) {
    ## make name from an abbreviation of the first 2 parts of name
    col_name[toshort] <- sapply(as.list(name[toshort]), function(x)
                                abbreviate(paste(strsplit(x, split=",")[[1]][c(1,2)], collapse=','), 20))
    col_name[toshort] <- paste(col_name[toshort], params[toshort], sep='.')
  }
  todo <- !toshort
  col_name[todo] <- abbreviate(short[todo], 20)
  ## append special tags
  bed <- name[todo] %cn% "bed sediment"
  col_name[todo][bed] <- paste(col_name[todo][bed], "_bed", sep='')
  whowat <-  name[todo] %cn% "water, unfiltered"
  col_name[todo][whowat] <- paste(col_name[todo][whowat], "_WW", sep='')
  susp <- name[todo] %cn% "suspended sediment"
  col_name[todo][susp] <- paste(col_name[todo][susp], "_susp", sep='')
  soil <- name[todo] %cn% "soil"
  col_name[todo][soil] <- paste(col_name[todo][soil], "_soil", sep='')
  tiss <- name[todo] %cn% "tissue"
  col_name[todo][tiss] <- paste(col_name[todo][tiss], "_tiss", sep='')
  ## Any duplicated?
  dups <- duplicated(col_name) | duplicated(col_name, fromLast=TRUE)
  col_name[dups] <- paste(col_name[dups], params[dups], sep='.')
  retval <- rbind(retval, data.frame(parm_cd=params, col_name=col_name))
  retval$col_name <- make.names(retval$col_name)
  return(retval)
}
