#' @title Convert dataframe to include survival (Surv) objects 
#' 
#' @description Within a dataframe, paired numeric fields that use a "_lo" and
#'   "_hi" suffix naming convention (e.g., "conc_lo" "conc_hi") are combined
#'   into a single Surv object (e.g., "conc") using the "interval2" option
#'   provided by through the \code{survival::Surv(conc_lo, conc_hi, type =
#'   "interval2")} syntax.
#' 
#' @details
#' Converting fields to Surv objects works with field pairs that have a "_lo" 
#' and "_hi" suffix naming convention. The numeric value for "_hi" must be greater than 
#' or equal to the "_lo" value. Source data that violate this requirement are 
#' set to NA with a summary report outputted to the console.
#' 
#' The user can specify their own values for the lo/hi suffixes or use the defaults.
#' 
#' @param df name of data frame
#' @param suf_lo Column name suffix for "lo" values.  Default = "_lo"
#' @param suf_hi Column name suffix for "hi" values.  Default = "_hi"
#' 
#' @examples
#' df <- dataCensored[1:20,]
#' colnames(df)
#' df1 <- unSurvDF(df)
#' colnames(df1)
#' # Default values
#' df2 <- makeSurvDF(df1)
#' colnames(df2)
#' # User values
#' df3 <- unSurvDF(df, "_LOW", "_HIGH")
#' colnames(df3)
#' df4 <- makeSurvDF(df3, "_LOW", "_HIGH")
#' colnames(df4)
#'
#' @return dataframe with Surv fields
#' 
#' @seealso \code{\link{unSurv}},  \code{\link{unSurvDF}},  \code{\link{impute}},  \code{\link{imputeDF}},  \code{\link{saveDF}},  
#' 
#' @export
#' 
makeSurvDF <- function(df, suf_lo = "_lo", suf_hi = "_hi") {
  
  # list of input dataframe fields 
  varList <- names(df)
  
  # look for fields to convert to Surv objects ####
  {
    # fields with "_lo" 
    varSurv.lo     <- data.frame(lo = (varList[grepl(suf_lo,varList)]))
    varSurv.lo$var <- sub(suf_lo,"",varSurv.lo$lo)
    
    # fields with "_hi" 
    varSurv.hi     <- data.frame(hi = (varList[grepl(suf_hi,varList)]))
    varSurv.hi$var <- sub(suf_hi,"",varSurv.hi$hi)
    
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
      var.lo <- paste0(var, suf_lo)
      var.hi <- paste0(var, suf_hi)
      # identify, count, and report number of bad values
      bad <- df[, var.lo] > df[, var.hi]
      bad[is.na(bad)] <- FALSE
      num_bad <- num_bad + sum(bad)
      if (sum(bad)>0) {
        print(paste("Num obs w/ *", suf_lo, " > *", suf_hi, ":", var, "--> ",sum(bad)))
      }
      df[bad,c(var.lo,var.hi)] <- NA
      # print total number of observations set to NA on last iteration
      if (num_bad > 0 & var ==varSurv[varSurv$boo.to.Surv, "var"][length(varSurv[varSurv$boo.to.Surv, "var"])]) {
        print(paste("  Number of observations set to NA ==> ",num_bad))
      }
    }## FOR ~ var ~ END
  }
  
  # convert variables to Surv objects ####
  {
    for (var in varSurv[varSurv$boo.to.Surv, "var"]) {
      var.lo <- paste0(var, suf_lo)
      var.hi <- paste0(var, suf_hi)
      df2[ ,var]  <- survival::Surv(df[, var.lo], df[, var.hi], type = "interval2")
    }## FOR ~ var ~ END  
  }
  
  return(df2)
}## FUNCTION ~ END