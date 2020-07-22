# ####
#' @title Converts Surv object into a 3-column matrix
#'
#' @description Converts Surv object into a 3-column matrix
#' 
#' @details The third column of the returned matrix (type) has the following meanings:
#' 
#'  1  -- no censoring
#'  
#'  2  -- left censored ("less than in a survival sense", e.g., [-Inf to 10], <10)
#'  
#'  3  -- interval censored ("less than in a water quality sense", e.g., "0 - <3", "1 - 3")
#'  
#'  NA -- missing value
#'  
#' The user can specify the names of the low and high columns in the output.
#' Defaults are "lo" and "hi".
#'  
#' @param x vector (Surv object)
#' @param col_lo Output column name for "lo" values.  Default = "lo"
#' @param col_hi Output column name for "hi" values.  Default = "hi"
#' 
#' @examples 
#' df1 <- dataCensored[dataCensored$station=="CB3.3C","chla"][1:30]
#' colnames(df1)
#' # Default values
#' df2 <- unSurv(df1)
#' colnames(df2)
#' # User values
#' df3 <- unSurv(df1, "LOW", "HIGH")
#' colnames(df3)
#' 
#' @return Returns a 3-column matrix: lo, hi, type
#'  
#' @export
# ####
unSurv <- function(x, col_lo = "lo", col_hi = "hi") {
  # extract lo and hi columns from Surv objects and place into a 3 column matrix
  type = x[ , 3]
  
  indx.0  <- !is.na(type) &  type == 0 # right censored ("greater thans in a survival sense" [10 to Inf], e.g., >1000)
  indx.1  <- !is.na(type) &  type == 1 # no censoring
  indx.2  <- !is.na(type) &  type == 2 # left censored ("less than in a survival sense" [-Inf to 10], i.e., <10)
  indx.3  <- !is.na(type) &  type == 3 # interval censored ("less than in a water quality sense", i.e., "0 - <3", "1 - 3")
  indx.NA <-  is.na(type)                # missing value
  
  hi <- lo  <- x[, 1]         # default:    set hi and lo to 1st column of x
  hi[indx.3]  <- x[indx.3, 2] # int. cens:  replace hi with 2nd column of x 
  lo[indx.2]  <- -Inf         # left cens:  replace lo with -Inf for 'survival-based' less thans
  hi[indx.0]  <- Inf          # right cens: replace hi with Inf 
  
  df_result <- cbind(lo,hi,type)
  colnames(df_result) <- c(col_lo, col_hi, "type")
  return(df_result)
}## FUNCTION ~ unSurv ~ END

# ####
#' @title Converts Surv objects in a dataframe to "lo" and "hi" values
#'
#' @description Converts Surv objects in a dataframe to "lo" (i.e., lower) and
#'   "hi" (i.e., upper) values. The user can specify their own values or use the
#'   defaults.
#' 
#' @param df dataframe with Surv objects 
#' @param suf_lo Column name suffix for "lo" values.  Default = "_lo"
#' @param suf_hi Column name suffix for "hi" values.  Default = "_hi"
#' 
#' @examples 
#' df <- dataCensored[dataCensored$station=="CB3.3C", ][1:20,]
#' colnames(df)
#' # Default values
#' df2 <- unSurvDF(df)
#' colnames(df2)
#' # User values
#' df3 <- unSurvDF(df, "_LOW", "_HIGH")
#' colnames(df3)
#' 
#' @return Returns dataframe with censored data converted to lo/hi format
#'  
#' @export
# ####
unSurvDF <- function(df, suf_lo = "_lo", suf_hi = "_hi") {
  
  df_out <- data.frame(row.names = rownames(df))
  for (col in names(df)) {
    if (survival::is.Surv(df[[col]])) {
      df_out[paste0(col, suf_lo)] <- baytrends::unSurv(df[[col]])[,1]
      df_out[paste0(col, suf_hi)] <- baytrends::unSurv(df[[col]])[,2]
    } else {
      df_out[[col]] <- df[[col]]
    }## IF ~ END
  }## FOR ~ END
  
  return(df_out)
}## FUNCTION ~ unSurvDF ~ END

