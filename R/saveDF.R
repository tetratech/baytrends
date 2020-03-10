#' @title Save R object to disk
#'
#' @description Saves R object to disk using csv and/or Rdata format.
#'
#' @details Output files are saved with an "rObj_note_YYYY_MM_DD_HHMMSS" naming
#'   convetion. By default, files are saved as csv files to a '_save_df'
#'   subdirectory relative to the working directory and include a time stamp in
#'   the file name using utils::write.csv. The default folder can be changed
#'   with the folder argument. Inclusion of a time stamp in the file name
#'   enables saving the same object at multiple steps through an R script, but
#'   can be turned off with the timeStamp argument. To also save object as rData
#'   file, set rData=TRUE.
#'   
#' @param rObj Name of R object to save.
#' @param note Suffix to include in file name.
#' @param rData Logical field to save rObj as an rData file (FALSE [default]).
#' @param csv Logical field to save rObj as an a csv file (TRUE [default]).
#' @param attr Logical field to save data frame attributes as a text file (FALSE [default]).
#' @param timeStamp Logical field to include date/time stamp in file name (TRUE [default]).
#' @param folder Subdirectory for saving file ('_save_df is default)
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x=c(1:100))
#' saveDF(df, 'test_note')
#' }
#' 
#' @return Nothing returned. Saves R object to disk using csv and/or Rdata format.
#' 
#' @export
saveDF <- function(rObj, note=NULL, rData=FALSE, csv=TRUE, attr=FALSE, timeStamp=TRUE, folder="_save_df") {

# ----- Change history --------------------------------------------
# 12Jul2016: JBH: added comments; cleaned up documentation
# 29Apr2016: JBH: set default values for rData and attr to FALSE
# 28Feb2016: JBH: Added folder as argument to function to allow user to specify folder in
#            which to save data, updated csv save to not include row names

# Establish folders and file name conventions ####
  # create the subdirectory if it does not exist
  if (!file.exists(folder)){
    dir.create(file.path(folder))
  }

  # identify if rObj is a data frame
  dataframe<- class(rObj)[1]=="data.frame"

  # create a "YYYY_MM_DD_HHMMSS" formatted string for version control
  n <- gsub("-", "_",gsub(" ", "_",gsub(":", "",as.character(Sys.time()))))

  # put name of rObj into a string
  s<-deparse(substitute(rObj))

  # create base file name
  if         (is.null(note) &  timeStamp) {
    fileNameBase <- paste0(s,"_",n)
  } else if  (is.null(note) & !timeStamp) {
    fileNameBase <- s
  } else if  (                 timeStamp) {
    fileNameBase <- paste0(s,"_",note,"_",n)
  } else {
    fileNameBase <- paste0(s,"_",note)
  }

# Save object #####
  # output rObj as rData file
  if(rData ==TRUE ) {
    fileName <- paste0(fileNameBase,".rda")
    save(rObj, file=file.path(getwd(), folder, fileName))
  }

  #output rObj as csv
  if(csv ==TRUE ) {
    fileName <- paste0(fileNameBase,".csv")
    utils::write.csv(rObj, file=file.path(folder, fileName), row.names = FALSE)
  }

  # output rObj attributes (excluding row.names)
  if(attr ==TRUE & dataframe) {
    attrs <- attributes(rObj)
    fileName <- paste0(fileNameBase,".log")
    con <- file(file.path(folder, fileName))
    sink(con, append=TRUE)
    sink(con, append=TRUE, type="message")
    print(attrs[(!(names(attrs)=="row.names"))])
    # Restore output to console
    sink()
    sink(type="message")
  }

}


