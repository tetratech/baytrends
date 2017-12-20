# Generate data
#
# Erik.Leppo@tetratech.com
# 20171206
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# 0. Prep####
# assume wd is package directory
wd <- file.path(getwd(),"data-raw")

# 1. Generate Data####
# Reconstruct dataCensored from parts
library(baytrends)
#library(baytrends)  # need for as.qw to create class
#
# Names
myVar <- c("secchi"
             , "chla"
             , "do"
             , "tn"
             , "tp"
             , "po4f"
             , "pp"
             , "tdp"
             , "no23f"
             , "nh4f"
             , "tdn"
             , "pn"
             , "tss")
# Create DF
#DF <- data.frame(matrix("NA", nrow=7719, ncol=16))
#
DF <- read.table(file.path(wd,"dataCensored_00.csv"), sep=","
                 , header=TRUE, row.names=1
                 , colClasses = "character"
                  )
str(DF)
head(DF)
#
# convert date to POSIXct class (add EST)
DF[,"date"] <- as.POSIXct(DF[,"date"],tz="EST")
str(DF)
head(DF)
#
# Convert to qw format
## loop through each variable and add info
slotNames("qw")
mySlotNames <- c(".Data"
                 , "remark.codes"
                 , "value.codes"
                 , "reporting.level"
                 , "reporting.method"
                 , "reporting.units"
                 , "analyte.method"
                 , "analyte.name"
                 , "rounding"
                 , "unique.code" 
                 , "names") 

#
i <- myVar[1]
}
#
for (i in myVar) {##FOR.i.START
  #
  {
  i.num <- match(i, myVar)
  i.len <- length(myVar)
  #
  # Report Progress to User
  print(paste0("Processing; item i, ", i," (",i.num," of ",i.len,")."))
  flush.console()
  #
  # Load all slotname files
  #myName <- c("values","value2)")
  myFile <- file.path(wd,paste("dataCensored",i,mySlotNames[1],".csv",sep="_"))
    df.var.slot01 <- read.table(myFile, header=TRUE, row.names=1, sep=",")
    
  myName <- mySlotNames[2] 
  myColClass <- "character"
  myFile <- file.path(wd,paste("dataCensored",i,mySlotNames[2],".csv",sep="_"))
    df.var.slot02 <- read.table(myFile, header=TRUE, row.names=1, sep=",", colClasses=myColClass, na.strings="NA")
    names(df.var.slot02) <- myName
  
  # myName <- mySlotNames[3]  
  # myColClass <- "character" 
  # myFile <- file.path(wd,paste("dataCensored",i,mySlotNames[3],".csv",sep="_"))
  #   df.var.slot03 <- read.table(myFile, header=TRUE, row.names=1, sep=",", colClasses=myColClass)
  #   names(df.var.slot03) <- myName
  #   
  # myName <- mySlotNames[4]  
  # myColClass <- "character"
  # myFile <- file.path(wd,paste("dataCensored",i,mySlotNames[4],".csv",sep="_"))
  #   df.var.slot04 <- read.table(myFile, header=TRUE, row.names=1, sep=",", colClasses=myColClass)
  #   names(df.var.slot04) <- myName
  #   
  # myName <- mySlotNames[5]   
  # myColClass <- "character" 
  # myFile <- file.path(wd,paste("dataCensored",i,mySlotNames[5],".csv",sep="_"))
  #   df.var.slot05 <- read.table(myFile, header=TRUE, row.names=1, sep=",", colClasses=myColClass)
  #   names(df.var.slot05) <- myName
  # 
  # myName <- mySlotNames[6]  
  # myColClass <- "character"
  # myFile <- file.path(wd,paste("dataCensored",i,mySlotNames[6],".csv",sep="_"))
  #   df.var.slot06 <- read.table(myFile, header=TRUE, row.names=1, sep=",", colClasses=myColClass)
  #   names(df.var.slot06) <- myName
  # 
  # myName <- mySlotNames[7]  
  # myColClass <- "character"
  # myFile <- file.path(wd,paste("dataCensored",i,mySlotNames[7],".csv",sep="_"))
  #   df.var.slot07 <- read.table(myFile, header=TRUE, row.names=1, sep=",", colClasses=myColClass)
  #   names(df.var.slot07) <- myName
  # 
  # myName <- mySlotNames[8]  
  # myColClass <- "character"
  # myFile <- file.path(wd,paste("dataCensored",i,mySlotNames[8],".csv",sep="_"))
  #   df.var.slot08 <- read.table(myFile, header=TRUE, row.names=1, sep=",", colClasses=myColClass)
  #   names(df.var.slot08) <- myName
  #   
  # # Rounding is not included in as.qw (#09)
  # myName <- mySlotNames[9]  
  # myColClass <- "numeric"
  # # myFile <- file.path(wd,paste("dataCensored",i,mySlotNames[9],".csv",sep="_"))
  # #   df.var.slot09 <- read.table(myFile, header=TRUE, row.names=1, sep=",", colClasses=myColClass)
  # #   names(df.var.slot09) <- myName
  # 
  # myName <- mySlotNames[10]  
  # myColClass <- "character"
  # myFile <- file.path(wd,paste("dataCensored",i,mySlotNames[10],".csv",sep="_"))
  #   df.var.slot10 <- read.table(myFile, header=TRUE, row.names=1, sep=",", colClasses=myColClass)
  #   names(df.var.slot10) <- myName
  #   
  # # names is not included in as.qw (#11)
  # myName <- mySlotNames[11] 
  # myColClass <- "character"
  # # myFile <- file.path(wd,paste("dataCensored",i,mySlotNames[11],".csv",sep="_"))
  # #   df.var.slot11 <- read.table(myFile, header=TRUE, row.names=1, sep=",", colClasses=myColClass)
  # #   names(df.var.slot11) <- myName
  # #
    
  # combine
  df.var.slotData <- cbind(df.var.slot01, df.var.slot02 ) #, df.var.slot03
                           # , df.var.slot04, df.var.slot05, df.var.slot06
                           # , df.var.slot07, df.var.slot08, df.var.slot10)
  # remove to clear memory
  rm(df.var.slot01, df.var.slot02) #, df.var.slot03
      # , df.var.slot04, df.var.slot05, df.var.slot06
      # , df.var.slot07, df.var.slot08, df.var.slot10)

  slotNames("qw")
  getSlots("qw")
  showClass("qw")
  }
  # # same as qw-class.R
  # setClass("qw2", slots=list(remark.codes="character", value.codes="character",
  #                           reporting.level="numeric", reporting.method="character",
  #                           reporting.units="character", analyte.method="character",
  #                           analyte.name="character", rounding="numeric",
  #                           unique.code="character",names="character"),
  #          contains="matrix")
  # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # setGeneric("as.qw2", function(values, remark.codes, value.codes, reporting.level,
  #                              reporting.method, reporting.units, analyte.method,
  #                              analyte.name, unique.code, value2)
  #   standardGeneric("as.qw2")
  # )
  # 
  # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # setMethod("as.qw2", signature(values="numeric", remark.codes="character",
  #                              value.codes="character",
  #                              reporting.level="numeric", reporting.method="character",
  #                              reporting.units="character", analyte.method="character",
  #                              analyte.name="character", unique.code="character", value2="missing"),
  #           function(values, remark.codes, value.codes, reporting.level, reporting.method,
  #                    reporting.units,analyte.method, analyte.name, unique.code, value2) {
  #             ## Length of remark.codes must match length of values
  #             N <- length(values)
  #             if(length(remark.codes) != N)
  #               stop("lengths of values and remark.codes must match")
  #             ## In practical terms, it is conceivable that single values could be
  #             ##  supplied by the rest
  #             value.codes <- rep(value.codes, length.out=N)
  #             reporting.level <- rep(reporting.level, length.out=N)
  #             reporting.method <- rep(reporting.method, length.out=N)
  #             reporting.units <- rep(reporting.units, length.out=N)
  #             analyte.method <- rep(analyte.method, length.out=N)
  #             analyte.name <- rep(analyte.name, length.out=N)
  #             unique.code <- rep(unique.code, length.out=N)
  #             value2 <- values
  #             ## Logic check on remarks--only "<", ">", and " " make sense
  #             remark.codes[is.na(remark.codes)] <- " "
  #             remark.codes[remark.codes == ""] <- " "
  #             ## Special check on remark code "M"
  #             if(any(pickM <- remark.codes == "M")) { # recode to < RL if possible
  #               picks <- !is.na(reporting.level) & pickM
  #               if(any(picks)) {
  #                 remark.codes[picks] <- "<"
  #                 value2[picks] <- reporting.level[picks]
  #                 values[picks] <- 0
  #                 warning("Special remark code M converted to less-than value")
  #               }
  #             }
  #             remarks.uniq <- unique(remark.codes)
  #             remarks.ignore <- !(remarks.uniq %in% c("<", ">", " "))
  #             if(any(remarks.ignore))
  #               warning("Special remark.codes: ", paste(remarks.uniq[remarks.ignore], collapse=', '),
  #                       " retained, but may require interpretation by the user.")
  #             ## Modify value2 for > and values for <
  #             value2[remark.codes == ">"] <- Inf
  #             values[remark.codes == "<"] <- 0
  #             ## Change lower limit if logged--will this ever happen?
  #             LogVal <- grepl("^log\\(", reporting.units)
  #             if(any(LogVal))
  #               values[remark.codes == "<" & LogVal] <- 0
  #             ## Pack it up and ship it off
  #             mat <- cbind(values=values, value2=value2)
  #             retval <- new("qw", mat, remark.codes=remark.codes,
  #                           value.codes=value.codes,
  #                           reporting.level=reporting.level,
  #                           reporting.method=reporting.method,
  #                           reporting.units=reporting.units,
  #                           analyte.method=analyte.method, analyte.name=analyte.name,
  #                           unique.code=unique.code, rounding=c(2L,3L),
  #                           names=as.character(seq(N))
  #                           )
  #             # Not sure why necessary
  #             retval@names <- as.character(seq(N))
  #             return(retval)
  #           })
  # # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # 
  
  DF[,i] <- as.qw(values       = df.var.slotData[,"values"]
                  , value2           = df.var.slotData[,"value2"]
                  , remark.codes     = df.var.slotData[,"remark.codes"]
                  , value.codes      = ""
                  , reporting.level  = NA_real_
                  , reporting.method = ""
                  , reporting.units  = ""
                  , analyte.method   = ""
                  , analyte.name     = ""
                  , unique.code      = ""
                  )
  # creates "qw" object file but assignment to data.frame is the issue
  abc <- as.qw(values       = df.var.slotData[,"values"]
               , value2           = df.var.slotData[,"value2"]
               , remark.codes     = df.var.slotData[,"remark.codes"]
               , value.codes      = ""
               , reporting.level  = NA_real_
               , reporting.method = ""
               , reporting.units  = ""
               , analyte.method   = ""
               , analyte.name     = ""
               , unique.code      = ""
  )
  str(abc)
  
  DF[,i] <- abc
  
  #assignInNamespace("as.qw","as.qw",ns="smwrQW",pos="package:smwrQW")
  #https://stackoverflow.com/questions/38388570/how-to-modify-unexported-object-in-a-package
  # this didn't go anywhere
  
  # Can't use "as".  No method for "coerce"
  # x <- ""
  # as(x, "qw") <- ""
  
#  DF[,i] <- as.qw(rep("",10))
  
  
  #https://stackoverflow.com/questions/4713968/r-what-are-slots
  
  
  # # Define class object (not sure if need baytrends::as.qw)
  # DF[,i] <- as.qw(values       = df.var.slotData[,"values"]
  #                         , value2           = df.var.slotData[,"value2"]
  #                         , remark.codes     = df.var.slotData[,"remark.codes"]
  #                         , value.codes      = ""
  #                         , reporting.level  = NA_real_
  #                         , reporting.method = ""
  #                         , reporting.units  = ""
  #                         , analyte.method   = ""
  #                         , analyte.name     = ""
  #                         , unique.code      = ""
  #                         )
  # # use @ to assign values
  # DF[]
  
  
  DF[,i]@rounding <- c(3,4)
  
  # define names
  values.X <- df.var.slotData[,"values"]
  N.X <- length(values.X)
  names.X=as.character(seq(N.X))
  DF[,i]@names <- as.character(seq(N.X))
  str(DF)
  #View(#
}##FOR.i.END


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
dataCensored <- DF
setwd("..")
devtools::use_data(dataCensored, overwrite = TRUE)

