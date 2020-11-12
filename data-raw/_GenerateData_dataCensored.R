# Generate data
##############################################################
# Last Update: 21May2020
#
# Create baytrends::dataCensored from raw csv file
#
# Input data sets: dataCensored_00.csv
# 
# Output data set: dataCensored.rda

{# 0. Prep####
  # assume wd is package directory
  wd <- file.path(getwd(),"data-raw")
  
  # 1. Generate Data####
  # Load libraries
  library(survival)
  
  # read data into DF 
  DF <- read.table(file.path(wd,"dataCensored_00.csv"), sep=","
                   , header=TRUE  
                   #, colClasses = "character"
  )
  
  DF[,"date"] <- as.POSIXct(DF[,"date"])
  str(DF)
  head(DF)
  #
  
  # list of variables 
  varList <- names(DF)
  
  # look for list of variables to convert to Surv objects
  varSurv.lo     <- data.frame(lo = sort(varList[grepl("_lo",varList)]))
  varSurv.lo$var <- sub("_lo","",varSurv.lo$lo)
  
  varSurv.hi     <- data.frame(hi = sort(varList[grepl("_hi",varList)]))
  varSurv.hi$var <- sub("_hi","",varSurv.hi$hi)
  
  varSurv        <- data.frame(var = sort(unique(c(varSurv.lo$var, varSurv.hi$var))))
  varSurv        <- merge(varSurv, varSurv.lo, all.x = TRUE)
  varSurv        <- merge(varSurv, varSurv.hi, all.x = TRUE)
  varSurv$boo.to.Surv <- !is.na(varSurv$lo) & !is.na(varSurv$hi)
  
  # variables to keep as is
  DF2     <- DF[, !(varList %in% c(varSurv[varSurv$boo.to.Surv, "lo"], 
                                   varSurv[varSurv$boo.to.Surv, "hi"]))]
  
  # variables to convert to Surv objects  
  for (var in varSurv[varSurv$boo.to.Surv, "var"]) {
    var.lo <- paste0(var,"_lo")
    var.hi <- paste0(var,"_hi")
    DF2[ ,var]  <- Surv( DF[ ,var.lo], DF[ ,var.hi], type = "interval2")
  }
}

# 2. Save as RDA for use in package####
#
dataCensored <- DF2
#setwd("..")
usethis::use_data(dataCensored, overwrite = TRUE)

