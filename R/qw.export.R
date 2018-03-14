#' @title qw.export
#' 
#' @description Export the contents of a QW object to comma delimited file (csv).  
#' 
#' @details This internal function will export each qw field of a given data frame 
#' to separate columns in a single comma separated file (csv).  The left header information 
#' will be used "as is" but the data fields will be named with the qw column name 
#' and the qw slot names .Data.values, .Data.value2, and remark.codes (e.g., secchi..Data.values).  
#' These slots names will be renamed _lo, _hi, _symbol (e.g., secchi_lo). 
#' The remaning qw slot values will not be exported. 
#' 
#' The exported file will be in a directory specified by the user (defaults to working directory).
#' The file will be named as specified by the user.
#' 
#' @keywords internal
#' 
#' @param df.qw Data frame with qw fields to be exported.
#' @param dir.output Directory where output files will be saved. 
#' Default is working directory.
#' @param fn.output Name of output file.
#' 
#' @return Returns a user named tab delimited files to the user defined directory with all of the qw slot data.
#' 
#' 
#' @examples
#' # define data frame with qw column classes
#' myDF <- dataCensored
#' 
#' # directory to save output
#' dir.save <- getwd()
#' 
#' # prefix for file names ()
#' fn.out <- "dataCensored_TEST.csv" 
#' 
#' # run function
#' qw.export(myDF, dir.save, fn.out)
# ~~~~~~~~~~~~~~~~ 
# # QC
# myDF <- dataCensored
# dir.save <- getwd()
# fn.out <- "dataCensored_test.tsv"
# #
# df.qw <- myDF
# dir.output <- dir.save
# fn.output <- fn.out
# ~~~~~~~~~~~~~~~~
# i <- col.qw.T[1]
# j <- slotNames("qw")[1]
# Testing
# dir.default <- "C:\\Users\\Erik.Leppo\\OneDrive - Tetra Tech, Inc\\MyDocs_OneDrive\\ChesBay_R\\CreateQW\\qw_output"
# fn <- tk_choose.files(default="dir.qw.orig", caption="Select smwr QW file to convert")
# load(fn)
# # get name of qw object
# name.qw <- load(fn, verbose=TRUE)
# # assign to a generic name so can manipulate (reassign at end)
# df.qw <- get(name.qw)
# name.qw
# str(df.qw)
# load file
#deparse(substitute(dataCensored))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
#' @export
qw.export <- function(df.qw, dir.output=getwd(), fn.output=NULL){##FUNCTION.qw.export.START
  #
  # Parameter Check ####
  if(is.null(fn.output)){##IF.fn.output.START
    Msg <- "No file name provided; e.g., myFile.tsv."
    stop(Msg)
  }##IF.fn.output.END
  #
  # troubleshooting ####
  boo.QC <- FALSE
  #
  # Find QW and nonQW columns ####
  ## Get column classes
  col.class.ls <- lapply(df.qw, class)
  col.class <- unlist(col.class.ls, use.names=FALSE)
  boo.col.qw <- grepl("qw", col.class)
  ## ID column names by class
  ### Extra condition for some POSIX fields with extra classes.  
  col.qw.T.num <- sum(boo.col.qw)
  col.qw.F.num <- ncol(df.qw) - col.qw.T.num
  #  # Assume qw fields at the end
  col.qw.F <- names(df.qw)[1:col.qw.F.num]
  col.qw.T <- names(df.qw)[(col.qw.F.num+1):ncol(df.qw)]
  #
  # Start new DF ####
  # non-qw columns
  df.output <- as.data.frame(df.qw[, col.qw.F])
  #
  # Export ####
  ## cycle each qw column
  for (i in col.qw.T) {##FOR.i.START
    i.num <- match(i, col.qw.T)
    i.len <- length(col.qw.T)
    # Report progress to user
    myMsg <- paste0("Processing; QW field, ", i.num, "/", i.len, " (", i, ").")
    # print(myMsg)
    # flush.console()
    ## qw slot
    mySlotNames <- slotNames("qw")[1:2]
    ## cycle slots
    for (j in mySlotNames) {##FOR.j.START
      j.num <- match(j, mySlotNames)
      j.len <- length(mySlotNames)
      # 
      # QC for slot
      boo.slot <- .hasSlot(df.qw[,i], j)
      if (boo.slot==TRUE) {##IF.boo.slot.START
        #
        if(boo.QC==TRUE) {##IF.boo.QC.START
          myMsg <- paste0("Processing; QW field, ", i.num, "/", i.len, " (", i
                          , "); Slot, ", j.num, "/", j.len, " (", j, ").")
          print(myMsg)
          flush.console()
        }##IF.boo.QC.END
        #
        # # special condition for "rounding"
        if(j == "rounding"){##IF.j.rounding.START
         # rounding save to 2nd file
           #fn.output.rounding <- file.path(dir.output, paste0(output.fn.prefix, "."
          #                                                    , i, ".rounding", ".tsv"))
          # write.table(slot(df.qw[,i], j), fn.output.rounding, quote=TRUE
          #             , sep="\t", row.names=FALSE, col.names=TRUE)
          
          # do nothing
        } else if (j==".Data") {
          name.col <- paste(i,"lo", sep="_")
          df.output[, name.col] <- slot(df.qw[,i], j)[,"values"]
          name.col <- paste(i,"hi", sep="_")
          df.output[, name.col] <- slot(df.qw[,i], j)[,"value2"]
          
        } else {
          name.col <- paste(i,j, sep=".")
          df.output[, name.col] <- slot(df.qw[,i], j)
          
          # normal save
          #name.col <- paste(i,j, sep=".")
          #df.output[, name.col] <- slot(df.qw[,i], j)
        }##IF.j.rounding.END
        #
      }##IF.boo.slot.END
      #
    }##FOR.j.END
    #

  }##FOR.i.END
  #
  # a <- "secchi..Data.values"
  # b <- "secchi..Data.value2"
  # c <- "secchi.remark.codes"
  # gsub("..Data\\.values", "_lo", a)
  # gsub("..Data\\.value2", "_hi", b)
  # gsub("\\.remark\\.codes", "_symbol", c)
  # 
  # 
  # 
  
  # .Data is a column with 2 list elements.
  #head(df.output[,"secchi..Data"])[,"values"]
  #head(df.output[,"secchi..Data"])[,"value2"]
  
  # Modify Column Names
  # names(df.output) <- gsub("\\.\\.Data.values", "_lo", names(df.output))
  # names(df.output) <- gsub("\\.\\.Data.value2", "_hi", names(df.output))
  names(df.output) <- gsub("\\.remark\\.codes", "_symbol", names(df.output))
  #
  #df.output <- as.data.frame(df.output)
  # Save Output ####
  #fn.output <- file.path(dir.output, paste0(output.fn.prefix, ".", i, ".csv"))
  write.table(df.output, fn.output, quote=TRUE, sep=","
              , row.names=FALSE, col.names=TRUE)
  # Clean Up ####
  myMsg <- "QW object saved to user named file in user specified directory;"
  print(myMsg)
  myMsg <- dir.output
  print(myMsg)
  flush.console()
  #
}##FUNCTION.qw.export.START