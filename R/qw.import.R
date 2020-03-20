#' @title qw.import
#' 
#' @description Import the contents of tab delimited text files to a QW object.
#' 
#' @details This internal function will import data from a comma delimited (CSV) file 
#' to create a data frame with columns of the qw class.  
#' 
#' Only three slots are recognized by this function; ".Data" and "remark.codes" 
#' The slot name ".Data" includes ".Data.values" and ".Data.value2".
#' 
#' These three slots are marked as "_lo", "_hi", and "_symbol" in the import file 
#' the prefix of the parameter name to be used in the qw object; e.g., secchi_lo,
#' secchi_hi, and secchi_symbol.
#'  
#' To view all qw slot names use the command slotNames("qw").  
#'  
#' The function returns a data frame with QW enabled columns.  The user must save the file.  
#' Any modification of column classes needs to be handled by the user (e.g., character to date or POSIXct).
#'
# @keywords internal
#' 
#' @param fn.import Filename (with path) containing data.  Comma separated (CSV) format only.
#' @param qw.names Names of parameters for QW columns.
#' @param rounding OPTIONAL.  Rounding values for QW columns.  Default is c(3,4) but can be changed.
#' 
#' @return Returns a QW enabled data frame.
#' 
#' @importFrom utils flush.console
#' @importFrom utils write.table
#' @importFrom utils read.table
#' 
#' @examples
#' # Use internal function to export dataCensored as example for import
#' qw.export(dataCensored, getwd(), "data_censored_test.csv")
#' 
#' 
#' # Import Test file as a qw object
#' 
#' # Define function parameters
#' fn.import <- file.path(".", "data_censored_test.csv")
#' qw.names <- c("secchi", "chla", "do", "tn", "tp", "po4", "tdp"
#'               , "no23", "nh4", "tdn", "tss")
#' rounding <- c(3, 4)
#'
#' # Import
#' dataCensored.test<- qw.import(fn.import, qw.names, rounding)
#' 
#' # Check for qw class
#' str(dataCensored.test)
#' 
#' # Save
#' save(dataCensored.test, file="data_censored_test.rda")
#' 
#' 
#' # Show slot names for a qw object.
#' slotNames("qw")
#' 
#' ####
#' # convert date field to POSIXct
#' #dataCensored.test[,"date"] <- as.POSIXct(dataCensored.test[,"date"])
#' # str(dataCensored.test)
#' # as.numeric() and as.integer() can be used to convert columns of those types.
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

# only data.values is required

# User provides QW data names.  Will need _hi, _lo, and _symbol.
# e.g., <6 becomes 0, 6, <
# and 0.5 - 1.0 becomes 0.5, 1.0, and blank ("" or NA).
# Other QW slots are not required and will be marked as NA.
# The default for rounding will be 3, 4 for all QW fields.  This can be changed.

# fn.import <- file.path(".", "dataCensored_test.tsv")
# rounding <- c(3, 4)
# qw.names <- c("secchi", "chla", "do", "tn", "tp", "po4f", "pp", "tdp"
#                  , "no23f", "nh4f", "tdn", "pn", "tss")
#' @export
qw.import <- function(fn.import, qw.names
                      , rounding=c(3,4)){##FUNCTION.qw.export.START
  # troubleshooting
  boo.QC <- TRUE
  # Import ####
  df.import <- read.table(fn.import, sep=","
                          , header=TRUE, colClasses="character"
                          , na.strings="NA")
  # Find Non-QW Col ####
  #col.qw.names <- paste(qw.names, c("lo","hi","symbol"), sep="_")
  col.qw.names <- as.vector(outer(qw.names, c("lo", "hi", "symbol"), paste, sep="_"))
  col.qw.T <- names(df.import) %in% col.qw.names
  col.qw.F <- names(df.import)[!(col.qw.T)]
  
  # QC, QW col ####
  if (sum(col.qw.T)==0){
    Msg <- "No matching qw data columns with lo, hi, or symbol."
    stop(Msg)
  }
  #
  # Create DF ####
  df.qw <- df.import[, col.qw.F]

  # Add QW cols ####
  for (i in qw.names){##FOR.i.START
    #
    i.num <- match(i, qw.names)
    i.len <- length(qw.names)
    #
    # User feedback
    if(boo.QC==TRUE){##IF.boo.QC.START
      myMsg <- paste0("Processing; ", i.num, "/", i.len, " (", i, ").")
      print(myMsg)
      flush.console()
    }##IF.boo.QC.END
    #
    # Define each argument of the QW object
    i.values           <- as.numeric(df.import[, paste0(i, "_", "lo")])
    i.value2           <- as.numeric(df.import[, paste0(i, "_", "hi")])
    i.remark.codes     <- as.character(df.import[, paste0(i, "_", "symbol")])
    i.value.codes      <- ""
    i.reporting.level  <- NA_real_
    i.reporting.method <- ""
    i.reporting.units  <- ""
    i.analyte.method   <- ""
    i.analyte.name     <- ""
    i.unique.code      <- ""
    #
    # Create the QW object ####
    suppressWarnings(
      df.qw[,i] <- as.qw(values            = i.values
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
    #
    # Rounding
    df.qw[,i]@rounding <- rounding
    #
  }##FOR.i.END
  #
  # Return ####
  return(df.qw)
  #
}##FUNCTION.qw.import.END