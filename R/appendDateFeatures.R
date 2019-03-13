###########################################################
#' Appends date features to data frame
#'
#' Appends date features to data frame. Creates new column 'date' based on var
#' if date is not already in the data frame. The newly created (or existing)
#' date column is truncated to day. Columns for year, day of year (doy), decimal
#' year (dyear), and month are added based on date. This function relies on
#' smwrBase::baseDay and smwrBase::baseDay2decimal for doy and decimal year.
#'   
#' The baseDay and baseDay2decimal functions have been added to this package 
#' from smwrBase package.
#'
#' @param df data frame
#' @param var variable with stored date
# @examples
#
# # retrieve Secchi depth for Station LE3.1, missing Secchi depth values are
# # maintained (see value for secchi on 2/12/2008), no transformations are applied
# dfr   <- selectData(dataCensored, 'secchi', 'LE3.1', 'S', transform=FALSE, missing=FALSE)
# dfr1  <- dfr[[1]]
# iSpec <- dfr[[2]]
# # retrieve surface corrected chlorophyll-a concentrations for Station LE3.1,
# # missing values are removed and transformation applied
# dfr   <- selectData(dataCensored, 'chla', 'LE3.1', 'S')
# dfr2  <- dfr[[1]]
# iSpec <- dfr[[2]]
#
#' @return Returns data frame with appended date features including year, doy,
#'   decimal year, and month
#' @keywords internal
#' 
.appendDateFeatures <-function(df, var="date") {

# -----< Change history >--------------------------------------------
# 29Oct2016: JBH: migrated to helper function
# 27Apr2016: JBH: Explicit use of "::" for non-base functions added.

  # Create a columnn 'date' in the df. If date is already in df, it is truncated
  # day level. Throw error if var is not found in df
  if (var %in% names(df)) {
    df$date <-  trunc( df[, names(df)==var] , units='days')
  } else {
    stop(paste0("The field containing date, '",var, "', was not found in data set."))
    return(df)
  }

  # Create variables for year, doy, and yearDec (note special handling that
  # 2/28 and 3/1 are always doy = 59 and 61, respectively when using
  # smwrBase::baseDay)
  df$year  <- lubridate::year(df$date)
  df$doy   <- as.numeric(baseDay(df$date))
  df$dyear <- df$year + baseDay2decimal(baseDay(df$date, numeric=FALSE))

  # Create month variables
  df$month <- months(df$date, abbreviate = TRUE)
  df$month <- factor(df$month,levels=c("Jan","Feb","Mar","Apr","May","Jun",
                                       "Jul","Aug","Sep","Oct","Nov","Dec"))
  df$nummon <- as.numeric(df$month)

  # make sure date is in POSIXct format
  df$date <- as.POSIXct(df$date)

  return(df)
}
