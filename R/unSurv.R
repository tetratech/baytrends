# ####
#' Converts Surv object into a 3 column matrix
#'
#' @param x variable x
#' \dontrun{
#' x1 <- dataCensored[dataCensored$station=="CB3.3C","chla"][1:30]
#' x2 <- unSurv(x1)
#' }
#' 
#' @return Returns a 3 column matrix: lo, hi, type
#'  meaning of type: 
#'  1  -- no censoring
#'  2  -- left censored ("less than in a survival sense" [-Inf to 10], i.e., <10)
#'  3  -- interval censored ("less than in a water quality sense", i.e., "0 - <3", "1 - 3")
#'  NA -- missing value
#'  
#' @export
# ####
unSurv <- function(x) {
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
  
  return(cbind(lo,hi,type))
}
