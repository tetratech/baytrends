#' @title Convert data frame to include Surv objects
#' 
#' @description Examines/evaluates dataframe for fields with "_lo" and "_hi" suffixes to convert 
#' to Surv objects.
#' 
#' @param df name of data frame
#' 
#' @examples
#' \dontrun{
#' df <- dataCensored[1:20,]
#' df1 <- unSurvDF(df)
#' df2 <- makeSurvDF(df1)
#' }
#'
#' @return dataframe with Surv fields
#' 
#' @export
#' 
makeSurvDF <- function(df) {
  
  # list of input dataframe fields 
  varList <- names(df)
  
  # look for fields to convert to Surv objects ####
  {
    # fields with "_lo" 
    varSurv.lo     <- data.frame(lo = (varList[grepl("_lo",varList)]))
    varSurv.lo$var <- sub("_lo","",varSurv.lo$lo)
    
    # fields with "_hi" 
    varSurv.hi     <- data.frame(hi = (varList[grepl("_hi",varList)]))
    varSurv.hi$var <- sub("_hi","",varSurv.hi$hi)
    
    # fields with "_lo" and "_hi" to process as Surv
    varSurv        <- data.frame(var = (unique(c(varSurv.lo$var, varSurv.hi$var))))
    varSurv$order  <- seq.int(nrow(varSurv)) 
    varSurv        <- merge(varSurv, varSurv.lo, all.x = TRUE)
    varSurv        <- merge(varSurv, varSurv.hi, all.x = TRUE)
    varSurv$boo.to.Surv <- !is.na(varSurv$lo) & !is.na(varSurv$hi)
    varSurv        <- varSurv[order(varSurv$order), ]
  }
  
  # migrate non-Surv fields to output dataframe ####
  {
    df2     <- df[, !(varList %in% c(varSurv[varSurv$boo.to.Surv, "lo"], 
                                     varSurv[varSurv$boo.to.Surv, "hi"]))]
  }  
  
  # look for/fix values that cannot be converted to Surv ####
  # (i.e., "*_lo" > "*_hi") and replace with NA
  {
    for (var in varSurv[varSurv$boo.to.Surv, "var"]) {
      # initialize counter
      if (var == varSurv[varSurv$boo.to.Surv, "var"][1]) {
        num_bad = 0    
      }
      var.lo <- paste0(var,"_lo")
      var.hi <- paste0(var,"_hi")
      # identify, count, and report number of bad values
      bad <- df[,var.lo] > df[,var.hi]
      bad[is.na(bad)] <- FALSE
      num_bad <- num_bad + sum(bad)
      if (sum(bad)>0) {
        print(paste("Num obs w/ *_lo > *_hi:", var, "--> ",sum(bad)))
      }
      df[bad,c(var.lo,var.hi)] <- NA
      # print total number of observations set to NA on last iteration
      if (num_bad > 0 & var ==varSurv[varSurv$boo.to.Surv,"var"][length(varSurv[varSurv$boo.to.Surv,"var"])]) {
        print(paste("  Number of observations set to NA ==> ",num_bad))
      }
    }
  }
  
  # convert variables to Surv objects ####
  {
    for (var in varSurv[varSurv$boo.to.Surv, "var"]) {
      var.lo <- paste0(var,"_lo")
      var.hi <- paste0(var,"_hi")
      df2[ ,var]  <- survival::Surv( df[ ,var.lo], df[ ,var.hi], type = "interval2")
    }  
  }
  
  return(df2)
}