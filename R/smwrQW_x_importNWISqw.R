#'Water-Quality Data
#'
#'Imports discrete water-quality sample data from NWISweb.
#'
#' @details Valid parameter code groups are "All," or group codes:
#'\tabular{ll}{
#'Code \tab Description\cr
#'INF \tab Information \cr
#'PHY \tab Physical \cr
#'INM \tab Inorganics, Major, Metals (major cations) \cr
#'INN \tab Inorganics, Major, Non-metals (major anions) \cr
#'NUT \tab Nutrient \cr
#'MBI \tab Microbiological \cr
#'BIO \tab Biological \cr
#'IMN \tab Inorganics, Minor, Non-metals \cr
#'IMM \tab Inorganics, Minor, Metals \cr
#'TOX \tab Toxicity \cr
#'OPE \tab Organics, pesticide \cr
#'OPC \tab Organics, PCBs \cr
#'OOT \tab Organics, other \cr
#'RAD \tab Radiochemical \cr
#'SED \tab Sediment \cr
#'POP \tab Population/community \cr
#'}
#'
#' @param sites a vector of the USGS station identifiers.
#' @param params A character string contains the name of a group of parameter
#'codes, or a vector of parameter codes. See \bold{Details}.
#' @param begin.date the earliest date for data, must be a character with the
#'format "YYYY-mm-dd."
#' @param end.date the latest date for data, must be a character with the format
#'"YYYY-mm-dd."
#' @param keep a character vector for any additional columns to retain from the
#'retrieved data.
#' @param use.pnames create colummn names based on pcode (like P00000)? Default
#'is to generate more English-like names.
#' @return A data frame of the water-quality data of class "qw" organized by
#'sample. Column names for the water-quality constituents are generated 
#'automatically, but can be set by the user,
#'see \code{\link{makeColNames}}.
#' @note The sample information columns that are automatically retained are the 
#'USGS station identifier, sample date, sample time, time zone code, and medium
#'code. If composite samples are found in the data, then sample end date and 
#'sample end time are also included. Use the \code{keep} argument to retain more 
#'if needed.\cr
#'
#'NWIS has several remark codes that have special meaning in addition to "<," which
#'indicates a left-censored value and ">," which indicates a right censored value.
#'Most other codes are ignored in processing, but passed through and retained as the
#'remark code in the data; a warning is printed when special remark codes are retained.
#'NWIS also uses the remark code "M" to indicate whan a result value is rounded to 0. In
#'that case, \code{importNWISqw} tries to recode the data as less than the reportiong 
#'level, but is the reporting level is the missing value \code{NA}, the remark code "M"
#'is retained. Warnings are generated for both conversion and retention of the "M"
#'remark codes.
#'
#' @seealso \code{\link{readNWISqw}}
#' @references Lorenz, D.L., 2014, smwrQW OFR.\cr See information about discrete
#'samples at \url{https://nwis.waterdata.usgs.gov/usa/nwis/qw}.
#' @keywords datasets IO
#' @examples
#'
#'\dontrun{
#'importNWISqw("05330000", "00608") # Ammonia samples from the Minnesota River at Jordan.
#'Empty <- importNWISqw('05330000',c('00400','00403'),begin.date='2008-01-01')
#'}
#' @import dataRetrieval
#' @export
importNWISqw <- function(sites, params="All", begin.date="", end.date="",
                         keep=NULL, use.pnames=FALSE) {
  ## Coding history:
  ##    2012Sep17 DLLorenz original Coding
  ##    2013Jan26 DLLorenz Added option to use Pcode names
  ##    2014Oct07 DLLorenz Added pcodes to convert to numeric
	##
	## Numeric pcodes: These are almost all INFO and many PHYSICAL groups
	NPCd <- get("pcodeColData")
	NPCd <- NPCd$parm_cd[NPCd$data_type == "numeric"]
	# Continue
  ByResult <- readNWISqw(sites, params, begin.date, end.date, 
  											 expanded=TRUE, reshape=FALSE)
  if(use.pnames) {
    Extra <- pcodeNWISqw(params, group=FALSE, name=FALSE, CASRN=FALSE,
                       short=TRUE, units=TRUE, col.name=FALSE)
    Extra$col_name <- paste("P", Extra$parameter_cd, sep="")
  } else {
    Extra <- pcodeNWISqw(params, group=FALSE, name=FALSE, CASRN=FALSE,
                         short=TRUE, units=TRUE, col.name=TRUE)    
  }
  
  if(nrow(ByResult) > 0){
    ByResult <- merge(ByResult, Extra, by.x="parm_cd", by.y="parameter_cd")
    
    ## Create the qw column and the by sample dataset
    ByResult$qw <- as.qw(as.numeric(ByResult$result_va), ByResult$remark_cd,
                         ByResult$val_qual_tx,
                         as.numeric(ByResult$rpt_lev_va), ByResult$rpt_lev_cd,
                         ByResult$parameter_units, ByResult$meth_cd,
                         ByResult$srsname, ByResult$parm_cd)
    
    ## The function group2row cannot handle complicated data structures like qw
    ##  work around by creating index to values and then extract the actual data
    ByResult$Seq <- seq(nrow(ByResult))
    ## If composite samples are found in the mix, retain the end dates, times and tz
    if(any(!is.na(ByResult[["sample_end_dt"]]))) 
      keep <- c("sample_end_dt", "sample_end_tm", keep)
    Carry <- c("site_no", "sample_dt", "sample_tm", "sample_start_time_datum_cd", 
               "medium_cd", keep)
    retval <- group2row(ByResult, Carry, "col_name", "Seq")
    names(retval)[4L] <- "tzone_cd" # Make it simple
    for(i in grep(".Seq", names(retval), value=TRUE, fixed=TRUE)) {
      val <-ByResult$qw[retval[[i]]]
      ## Work around some issues, need more robust criteria etc.
      uniq.val <- val@unique.code[!is.na(val@unique.code)][1L]
      if((uniq.val %in% NPCd) && censoring(val) == "none") {
        retval[[i]] <- as.numeric(val)
      } else
        retval[[i]] <- val
    }
    names(retval) <- gsub(".Seq", "", names(retval), fixed=TRUE)
    ## Sort by date
    Seq <- order(retval$sample_dt, retval$sample_tm, na.last=TRUE)
    retval <- retval[Seq,]
    return(retval)
    
  } else {
    message("No data returned")
    
    return(ByResult)
  }
  

}
