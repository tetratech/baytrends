#'Parameter Code Information
#'
#'Returns information about parameter codes. Support function for \code{importNWISqw}.
#'
#'Valid groups for \code{param} are "All," "INF," "PHY," "INM," "INN," 
#'"NUT," "MBI," "BIO," "IMN," "IMM," "TOX," "OPE," "OPC," "OOT," "RAD,"
#'"SED," or "POP."
#'
#'Added from smwrQW.
#'
#' @param params A character string contains the name of a group of parameter
#'codes, or a vector of pamater codes. See \bold{Details}.
#' @param group include the parameter group in the output?
#' @param name include the parameter name in the output?
#' @param CASRN include the parameter Chemical Abstracts Service Registry Number
#'(CASRN) in the output?
#' @param short include the parameter short name in the output?
#' @param units include the parameter units in the output?
#' @param col.name include a column name for the parameter in the output?
#' @return A data frame with the parameter codes and the selected columns.
#' @seealso \code{\link[dataRetrieval]{readNWISpCode}}
#' @references Lorenz, D.L., 2014, smwrQW OFR.\cr See information about discrete
#'samples at \url{https://nwis.waterdata.usgs.gov/usa/nwis/qw}.
#' @keywords internal datasets IO
#' @examples
#'
#'\dontrun{
#'pcodeNWISqw("00925")
#'}
#'
#'@export
pcodeNWISqw <- function(params="all", group=TRUE, name=TRUE, CASRN=FALSE, short=TRUE,
                        units=TRUE, col.name=FALSE) {
  ## Coding history:
  ##    2012Sep07 DLLorenz original Coding
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2015Jan29 DLLorenz Tweaks for smwr series
  ##
  ## The index of short names to NWIS group names
  grpcd <- c("INF"="information",
             "PHY"="physical",
             "INM"="cations",
             "INN"="anions",
             "NUT"="nutrients",
             "MBI"="microbiological",
             "BIO"="biological",
             "IMM"="metals",
             "IMN"="nonmetals",
             "TOX"="toxicity",
             "OPE"="pesticides",
             "OPC"="pcbs",
             "OOT" ="other organics",
             "RAD"="radio chemicals",
             "SED"="sediment",
             "POP"="population/community")
  ## Continue, ckprm = 0 if pcode list
  if(length(params) > 1L) # Must be multiple pcodes, get em all
    ckprm <- -1L
  else
    ckprm <- pmatch(params, c(grpcd, "all"), nomatch=0L)
  if(ckprm == 0L) { # Get a single pcode
    myurl <- url(paste("https://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=pm_search",
                       "&pm_search=", params,
                       "&format=rdb", "&show=parameter_group_nm"[group],
                       "&show=parameter_nm", "&show=casrn"[CASRN],
                       "&show=srsname", "&show=parameter_units"[units],
                       sep=""))
    retval <- importRDB(myurl)
    close(myurl)
  }
  else {
    if(ckprm == -1L)
      prmndx <- length(grpcd) + 1L
    else
      prmndx <- ckprm
    prmgp <- c(names(grpcd), "All+--+include+all+parameter+groups")[prmndx]
    prmgp <- gsub(" ", "+", prmgp)
    prmgp <- gsub(",", "%2C", prmgp)
    ## Always retrieve name and short name to produce column name if necessary
    myurl <- url(paste("https://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=param_group",
                       "&pm_group=", prmgp,
                       "&format=rdb", "&show=parameter_group_nm"[group],
                       "&show=parameter_nm", "&show=casrn"[CASRN],
                       "&show=srsname", "&show=parameter_units"[units],
                       sep=""))
    retval <- importRDB(myurl)
    close(myurl)
    ## get the requested pcodes if needed
    if(ckprm == -1L)
      retval <- retval[retval$parameter_cd %in% params, ]
  }
  ## Recode to shorter names if groupcodes were requested
  if(group)
    retval$parameter_group_nm <- factor(grpcd[retval$parameter_group_nm], levels=grpcd)
  ## Add column names if requested
  if(col.name)
    retval <- merge(retval, with(retval,
                                 makeColNames(parameter_cd, parameter_nm, srsname)),
                    by.x="parameter_cd", by.y="parm_cd")
  ## remove unwanted columns if necessasry
  if(!name)
    retval$parameter_nm <- NULL
  if(!short)
    retval$srsname <- NULL
  return(retval)
}
