#' Reduce dataframe and parameter list based on user selected parameterFilt
#'
#' Reduce dataframe and parameter list based on user selected parameterFilt
#'
#' @param df data frame
#' @param parameterFilt parameter list
#' @examples
#' #df <- chkParameter(df,parameterFilt=c("tn", "tp"))
#' @return n/a
#' @export
#'
.chkParameter <- function(df,parameterFilt=parameterFilt) {

  # QC check fix, 20180503
  parameterList <- baytrends::parameterList
  
  # Level 1 error trapping

  # error trap ... stop if no station found
  if (!("station" %in% names(df))) {
    stop("The variable 'station' not found in data set.")
    return(df)
  }

  # error trap ... stop if no date found
  if (!("date" %in% names(df))) {
    stop("The variable 'date' not found in data set.")
    return(df)
  }

  # error trap ... stop if no layer found
  if (!("layer" %in% names(df))) {
    warning("The variable 'layer' not found in data set.")
  }

  # NULL trap ... user parameter list and derive a list
  if (length(parameterFilt)==0) {
    warning("No parameter filter specified by user--master parameter list applied.")
    parameterFilt <- parameterList$parm
    parameterFilt <- parameterFilt[  (parameterFilt %in% names(df))]
  }

  # error trap ... stop if no values from user selected parameterFilt are in df
  if (length(parameterFilt[(parameterFilt %in% names(df))])==0) {
    stop("No parameter filter specified by user is in data set.")
    return(df)
  }

  # error trap ... stop if no values from user selected parameterFilt are in parameter list
  if (length(parameterFilt[(parameterFilt %in% parameterList$parm)])==0) {
    stop("No parameter filter specified by user appears to be a valid parameter.")
    return(df)
  }

  # Level 2 error trapping

  # identify variables in df that are not in  station, date, layer and valid parameter list
  tmp <- names(df[!(names(df) %in% c(c("station", "date", "layer"), parameterList$parm))])
  if (!length(tmp)==0) {
    warning(paste("Following variable(s) found in data set will",
                  "be removed:",tmp))
  }

  # identify parameters in user parameter filter list that are not in valid parameter list
  tmp <- parameterFilt[!(parameterFilt %in% parameterList$parm)]
  if (!length(tmp)==0) {
    warning(paste("User selected parameter filter value not used: ",tmp))
  }

#   # warning message commented out as per 04Dec2015 conference call with Jeni
#   # identify user parameter filter list that are not in df
#   tmp <- parameterFilt[!(parameterFilt %in% names(df))]
#   if (!length(tmp)==0) {
#     warning(paste("Following variable found in user selected parameter filter is not",
#                   "in the data set and will not be considered:",tmp))
#   }

  # Begin processing

  # save beginning list of variables
  tmp <- names(df)

  # reduce df to station, date, layer and valid parameters
  df <- df[, names(df) %in% c(c("station", "date", "layer"), parameterList$parm)]

  # processe date
  attr(df, "ProcessDate") <- Sys.time()

  # attribute ending list of variables
  attr(df, "beginVar") <-  tmp
  attr(df, "endVar") <-  names(df)

  # reduce user parameter filter list to valid parameters
  parameterFilt <- parameterFilt[  (parameterFilt %in% parameterList$parm)]

  # further reduce user parameter filter list to those in the df
  parameterFilt <- parameterFilt[  (parameterFilt %in% names(df))]

  # attribute list of id variables
  attr(df, "idVar") <-  names(df[names(df) %in% c("station", "date", "layer")])

  # attribute list of dependent variables
  attr(df, "depVar") <-  parameterFilt

  # attribute list of id variables
  attr(df, "othVar") <-  names(df[!(names(df) %in% c(attr(df,"idVar"), attr(df,"depVar")))])

  return(df)
}
