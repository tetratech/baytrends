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
  #
  DF[,i]@rounding <- c(3,4)  # default is 2,3
  #
  # # define names
  # values.X <- df.var.slotData[,"values"]
  # N.X <- length(values.X)
  # names.X=as.character(seq(N.X))
  # DF[,i]@names <- as.character(seq(N.X))
  # str(DF)
  #View(#
}##FOR.i.END


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
dataCensored <- DF
#setwd("..")
devtools::use_data(dataCensored, overwrite = TRUE)

