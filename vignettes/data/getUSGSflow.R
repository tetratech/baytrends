#' Retrieve USGS daily flow data in a wide format
#'
#' Retrieve USGS daily flow data from NWIS based on list of site IDs (relying on
#' dataRetrieval::readNWISdv and dataRetrieval::renameNWISColumns. The flow
#' and data qualifier code for each site is organized into two columns (e.g.,
#' "q10174500", "q101745000cd" for USGS gage 10174500). Flow is stored as cubic
#' meters per second [cms].
#'
#' This function automatically 'fills in' missing values [unless the user turns
#' this feature off] using code developed by USGS (see smwrBase::fillMissing). 
#' The user can also control the maximum gap size to fill in. If a daily flow
#' value is missing, then the corresponding data qualifier fields is set to
#' 'NaN'. The user can use this setting to identify which flows are filled in.
#'
#' @param siteNumber List of site numbers
#' @param yearStart Beginning year of data retrieval
#' @param yearEnd Ending year of data retrieval
#' @param fill TRUE[default]/FALSE field indicating whether to fill in missing
#'   values using with USGS' fillMissing function
#' @param span the maximum number of observations on each side of each range of
#'   missing values to use in constructing the time series model [default=10]
#' @param max.fill the maximum gap to fill [default=10]
#' @return USGS daily flow data for sites in a wide format
#' @examples
#' # set retrieval parameters
#' yearStart   <- 2014
#' yearEnd     <- 2014
#' siteNumber <- c('01578310')
#'
#' # regular retrieval (default usage)
#' df <- getUSGSflow(siteNumber, yearStart, yearEnd)
#'
#' @export
#'
getUSGSflow <- function(siteNumber, yearStart, yearEnd, fill=TRUE,
                          span=10, max.fill=10) {

# -----< Change history >--------------------------------------------
# 24Nov2017: JBH: transitioned to internalized smwrBase functions

# Error traps and Initialization ####
  # Set yearEnd if not supplied
  if (is.null(yearEnd)) {
    yearEnd <- lubridate::year(trunc(Sys.time(),units='days') - 10*24*3600)
  }

  # Error trap to make sure begin year is not greater than end year
  if(yearStart > yearEnd ) {
    stop(paste("Begin year", yearStart,"is greater than",yearEnd))
  }

  # Set flow parameter code
  parameterCd <- c("00060")                   # Set flow parameter code

  # Fill out start/end date based on user supplied start/end year
  dateStart   <- paste0(yearStart,"-01-01")
  dateEnd     <- paste0(yearEnd,"-12-31")

  # create a sequence of dates from beginning to end. When merged with
  # daily flow values in next step, it will ensure that the user will
  # get a complete daily sequence of flow values.
  df0 <- data.frame( date=seq.Date(as.Date(dateStart),as.Date(dateEnd),by="day"))

# Retrieve daily flow data data and rename variables ####
  for (i in 1:length(siteNumber)) {
    df1       <- dataRetrieval::readNWISdv(siteNumber[i], parameterCd, dateStart, dateEnd)
    df1       <- dataRetrieval::renameNWISColumns(df1)
    df1$Flow  <- 0.028316847 * df1$Flow   #convert from cfs to cms
    names(df1)[names(df1) == 'Date']    <- 'date'
    names(df1)[names(df1) == 'Flow']    <- paste0('q',siteNumber[i])
    names(df1)[names(df1) == 'Flow_cd'] <- paste0('q',siteNumber[i],'cd')
    df1 <- df1[,!(names(df1) %in% c("agency_cd","site_no"))]
    df0<-merge(df0,df1, by="date" ,all=TRUE)
  }

# Convert date to POSIXct ####
  df0$date <- as.POSIXct(strptime(df0$date, "%Y-%m-%d"))
  attr(df0,"variableInfo") <- "Streamflow [cms]"
  attr(df0,"queryTime") <- Sys.time()

# If user requests for data to be filled in, then do so. ####
  if (fill==TRUE) {
    for (i in 1:length(siteNumber)) {
      # find column with flow and qualifier code for the particular site
      vColq<-NaN; vColcd<-NaN
      vColq  <- which(names(df0) %in% paste0('q',siteNumber[i]))
      vColcd <- which(names(df0) %in% paste0('q',siteNumber[i], 'cd'))
      # if there is missing flow data, then assign the qualifier cd to NaN
      df0[, vColcd] <- ifelse(is.nan(df0[, vColq]), "NaN", df0[, vColcd])
      # fill in missing flow values
      df0[, vColq]  <- fillMissing(df0[, vColq], span=span, max.fill=max.fill)
    }
  }

# Drop all in last 10 days ####
  df0<-df0[ df0$date<as.POSIXct(trunc(Sys.time(),units='days') - 10*24*3600), ]

  return(df0)

}


