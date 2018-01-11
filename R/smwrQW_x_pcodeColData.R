#' Selected USGS parameter Codes
#'
#' Selected U.S. Gelogical Survey (USGS) parameter codes to create 
#' column names and data type conversion when importing data from NWISweb
#' 
#' @name pcodeColData
#' @docType data
#' @usage pcodeColData
#' @format Data frame with 1178 rows and 3 columns\cr
#'\tabular{lll}{
#'Name \tab Type \tab Description\cr
#'parm_cd \tab character \tab The 5-digit parameter code\cr
#'col_name \tab character \tab The column name to use when importing data from NWISweb\cr
#'data_type \tab character \tab The data type to convert the imported data\cr
#'}
#' @note This data set can be copied and modifed by the user to add or change column names.
#'The \code{data_type} must be either "qw" or "numeric" any other value will be ignored.
#' @references
#'Lorenz, D.L., 2016, smwrQW---An R Package for Managing and Analyzing 
#'Water-Quality Data, Version 0.7.3
#' @keywords internal datasets
#' @examples
#' data(pcodeColData)
#' # just print some rows
#' head(pcodeColData)
#' tail(pcodeColData)
NULL
