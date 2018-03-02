#' @title qw.import
#' 
#' @description Import the contents of tab delimited text files to a QW object.
#' 
#' @details This internal function will import data from tab delimited text files 
#' to create a data frame with columns of the qw class.  
#' 
#' Each slot name of the qw object (is required and) will be a column in the imported files 
#' (e.g., cond.remark.codes).  The qw slot value 'reporting' is handled by a file with just that information.
#' 
#' To get all qw slot names use the command slotNames("qw").  
#' The slot name ".Data" includes ".Data.values" and ".Data.value2".
#' 
#' Rounding is handled slightly differently and can be in a one column by two row file ("file")
#' or as a direct input ("value").
#' 
#' The function returns a data frame with QW enabled columns.  The user must save the file.  
#' Any modification of column classes needs to be handled by the user (e.g., character to date or POSIXct).
#' 
#' 
#' @keywords internal
#' 
#' @param var.qw Censored data column names to be converted to QW.
#' @param var.info Data columns to be attached to the final data frame that are not QW fields, e.g., StationID. 
#' @param fn.qw Vector of file names that match 1 for 1 with var.qw.  
#' @param dir.data Data directory for files to import.  Default = getwd().
#' @param rounding.source Source of the rounding data (file or value).  Default is NA and uses c(2,3).
#' @param rounding.data Rounding information.
#' 
#' @return Returns a QW enabled data frame.
#' 
#' @examples
#' # Show slot names for a qw object.
#' slotNames("qw")
#' 
#' # Export example data (dataCensored) to used for import
#' myDF <- dataCensored
#' dir.save <- getwd()
#' fn.prefix <- "dataCensored" 
#' qw.export(myDF, dir.save, fn.prefix)
#' 
#' # Reimport dataCensored as a new dataframe
#' 
#' var.qw <- c("secchi", "chla", "do", "tn", "tp", "po4f", "pp", "tdp"
#'               , "no23f", "nh4f", "tdn", "pn", "tss")
#' var.info <- c("station", "date", "layer")
#' fn.qw <- paste0("dataCensored.", var.qw, ".tsv")
#' rounding.source <- "file" # file or value or NA (default)
#' rounding.file <- paste0("dataCensored.", var.qw, ".rounding.tsv")
#' rounding.value <- c(3, 4) #default is c(2, 3)
#' rounding.data <- rounding.file
#' 
#' dataCensored.test<- qw.import(var.qw, var.info, fn.qw, dir.data=getwd()
#'                               , rounding.source=rounding.source
#'                               , rounding.data=rounding.value)
#' str(dataCensored.test)
#' 
#' # convert date field to POSIXct
#' dataCensored.test[,"date"] <- as.POSIXct(dataCensored.test[,"date"])
#' str(dataCensored.test)
#' 
#' # as.numeric() and as.integer() can be used to convert columns of those types.
#'
#' save(dataCensored.test, file="dataCensored.test.rda")
#'
# # ~~~~~~~~~~~~~~~~
# # QC / Testing
# # ~~~~~~~~~~~~~~~~
# # i <- fn.qw[1]
# 
# 
# var.qw <- c("kd")
# var.info <- c("station", "date", "layer") 
# dir.data <- "C:\\Users\\Erik.Leppo\\OneDrive - Tetra Tech, Inc\\MyDocs_OneDrive\\ChesBay_R\\datasets_qw_convert\\04_CBP_RebeccaMurphy\\qw.export"
# fn.qw <- paste0("df_kd.", var.qw, ".tsv")
# rounding.source <- "value"
# rounding.data <- c(3, 4)
# #rounding.data <- paste0("df_kd.", var.qw, ".rounding.tsv")
# 
# 
# i <- var.qw[1]
# 
#
#' @export
qw.import <- function(var.qw, var.info, fn.qw, dir.data=getwd()
                      , rounding.source=NA, rounding.data=c(2,3)){##FUNCTION.qw.export.START
  #
  # troubleshooting
  boo.QC <- TRUE
  # use for reporting later
  var.qw.len <- length(var.qw)


  # import files one by one
  for (i in var.qw){##FOR.i.START
    i.num <- match(i, var.qw)
    i.len <- length(var.qw)
    #
    i.fn <- fn.qw[i.num]
    # User feedback
    if(boo.QC==TRUE){##IF.boo.QC.START
      myMsg <- paste0("Processing; ", i.num, "/", i.len, " (", i, ").")
      print(myMsg)
      flush.console()
    }##IF.boo.QC.END

    # Import data ####
    df.i <- read.table(file.path(dir.data, i.fn), sep="\t"
                       , header=TRUE, colClasses="character"
                       , na.strings="NA")

    if(i.num==1){##IF.i.num.START
      df.master <- df.i[, var.info]

    } else {
      # match?

    }##IF.i.num.END

    # check for NA and use "" if all NA
    
    
    df.master[,i] <- df.i[,paste0(i, ".", ".Data.values")]
    
    # Define each argument of the QW object
    i.values           <- as.numeric(df.i[,paste0(i, ".", ".Data.values")])
    i.value2           <- as.numeric(df.i[,paste0(i, ".", ".Data.value2")])
    i.remark.codes     <- as.character(df.i[,paste0(i, ".", "remark.codes")])
    i.value.codes      <- ""
    i.reporting.level  <- NA_real_
    i.reporting.method <- ""
    i.reporting.units  <- ""
    i.analyte.method   <- ""
    i.analyte.name     <- ""
    i.unique.code      <- ""
    
    
    # 
    # i.remark.codes     <- df.i[,paste0(i, ".", "remark.codes")]
    # i.value.codes      <- df.i[,paste0(i, ".", "value.codes")]
    # i.reporting.level  <- NA_real_
    # i.reporting.method <- df.i[,paste0(i, ".", "reporting.method")]
    # i.reporting.units  <- df.i[,paste0(i, ".", "reporting.units")]
    # i.analyte.method   <- df.i[,paste0(i, ".", "analyte.method")]
    # i.analyte.name     <- df.i[,paste0(i, ".", "analyte.name")]
    # i.unique.code      <- df.i[,paste0(i, ".", "unique.code")]
    

    
    # Create the QW object ####
    suppressWarnings(
    df.master[,i] <- as.qw(values            = i.values
                           , value2           = i.value2
                           , remark.codes     = i.remark.codes
                           , value.codes      = i.value.codes
                           , reporting.level  = i.reporting.level
                           , reporting.method = i.reporting.method
                           , reporting.units  = i.reporting.units
                           , analyte.method   = i.analyte.method
                           , analyte.name     = i.analyte.name
                           , unique.code      = i.unique.code
    )##as.qw.END
    )##suppressWarnings.END
    
     
   # str(df.master)


    
    
    
    # reporting ####
    
    # QC check for number of rounding inputs
    # stop if not matching

    if(rounding.source=="files"){
      df.reporting <- read.delim(rounding.data[i.num], col.names=TRUE)
      val.reporting <- df.reporting["x", 2:3]
    } else {
      # default of NA has default of c(2,3)
      val.reporting <- rounding.data
    }##IF.rounding.source.END
    
    df.master[,i]@rounding <- val.reporting

  }##FOR.i.END

  return(df.master)

}##FUNCTION.qw.import.END