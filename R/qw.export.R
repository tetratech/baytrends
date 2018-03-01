#' @title qw.export
#' 
#' @description Export the contents of a QW object to tab delimited text files.  
#' 
#' @details This internal function will export each qw field of a given data frame 
#' to a separate tab separated file (tsv).  The left header information wil be attached to each file.
#' The following columns will be named with the qw column named and the qw slot names (e.g., cond.remark.codes). 
#' The exported files will be in a directory specified by the user (defaults to working directory)
#' and the files will be named with the name of the qw columns with a user 
#' defined prefix (defaults to qw.)
#' 
#' @keywords internal
#' 
#' @param df.qw Data frame with qw fields to be exported.
#' @param dir.output Directory where output files will be saved. 
#' Default is working directory.
#' @param output.fn.prefix Prefix for output files.  
#' Files will be named with prefix and QW column name.  Default is "qw".
#' 
#' @return Returns a series of tab delimited files to the user defined directory 
#' matching the number of qw fields in the input data frame.
#' 
#' @examples
#' # define data frame with qw column classes
#' myDF <- dataCensored
#' 
#' # directory to save output
#' dir.save <- getwd()
#' 
#' # prefix for file names ()
#' fn.prefix <- "dataCensored" 
#' 
#' # run function
#' qw.export(myDF, dir.save, fn.prefix)
# ~~~~~~~~~~~~~~~~ 
# QC
# df.qw <- myDF
# dir.output <- dir.save
# output.fn.prefix <- fn.prefix
# ~~~~~~~~~~~~~~~~
# i <- col.qw.T[1]
# j <- slotNames("qw")[1]
# # Testing
# # dir.default <- "C:\\Users\\Erik.Leppo\\OneDrive - Tetra Tech, Inc\\MyDocs_OneDrive\\ChesBay_R\\CreateQW\\qw_output"
# # fn <- tk_choose.files(default="dir.qw.orig", caption="Select smwr QW file to convert")
# # load(fn)
# # # get name of qw object
# # name.qw <- load(fn, verbose=TRUE)
# # # assign to a generic name so can manipulate (reassign at end)
# # df.qw <- get(name.qw)
# # name.qw
# # str(df.qw)
# # load file
#deparse(substitute(dataCensored))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
#' @export
qw.export <- function(df.qw, dir.output=getwd(), output.fn.prefix="qw"){##FUNCTION.qw.export.START
  #
  # troubleshooting
  boo.QC <- TRUE
  # Find QW and nonQW columns
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
  # Export
  ## cycle each qw column
  for (i in col.qw.T) {##FOR.i.START
    i.num <- match(i, col.qw.T)
    i.len <- length(col.qw.T)
    # Report progress to user
    myMsg <- paste0("Processing; QW field, ", i.num, "/", i.len, " (", i, ").")
    # print(myMsg)
    # flush.console()
    # Create Export data frame
    ## non-qw columns
    df.output <- df.qw[, c(col.qw.F)]
    ## qw slot
    mySlotNames <- slotNames("qw")
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
        # special condition for "rounding"
        if(j == "rounding"){##IF.j.rounding.START
          # rounding save to 2nd file
          fn.output.rounding <- file.path(dir.output, paste0(output.fn.prefix, "."
                                                             , i, ".rounding", ".tsv"))
          write.table(slot(df.qw[,i], j), fn.output.rounding, quote=TRUE
                      , sep="\t", row.names=FALSE, col.names=FALSE)
        } else {
          # normal save
          name.col <- paste(i,j, sep=".")
          df.output[, name.col] <- slot(df.qw[,i], j)
        }##IF.j.rounding.END
        #
      }##IF.boo.slot.END
      #
    }##FOR.j.END
    #
    fn.output <- file.path(dir.output, paste0(output.fn.prefix, ".", i, ".tsv"))
    write.table(df.output, fn.output, quote=TRUE
                , sep="\t", row.names=FALSE, col.names=TRUE)
    # clean up
    rm(df.output)
  }##FOR.i.END
  #
  myMsg <- "QW object saved to files in directory;"
  print(myMsg)
  myMsg <- dir.output
  print(myMsg)
  flush.console()
  #
}##FUNCTION.qw.export.START