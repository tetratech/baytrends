#' Find Recent File Information
#'
#' Find recent file information based on folder and file name (allows for wildcard
#' structure)
#'
#' @param folder folder (i.e., directory to look in, can use relative path )
#' @param file file (can use wildcards, e.g., "*.csv")
#' @param n number of files to return (e.g., value of 1 returns most recent
#'   file, value of 'all' returns all files)
#' @param fileNameOnly logical field, if TRUE only return file name
#' @details This function is used to search a selected directory for information
#'   about the most recently modified files in that directory. The default
#'   setting searches the current working directory. Relative directory
#'   addresses can be used. The default settings returns the name of the most
#'   recently modified file. Employing wildcards in the file argument can narrow
#'   the file search, e.g., "*.csv" will only return comma delimited files.
#'
#'   The default setting for the argument, n (of 1), will only return a single
#'   file. This value can be increased to any number (2, 3,...) to change the
#'   maximum number of files returned; or the argument can be set to 'all' to
#'   return all files. Setting the argument, fileNameOnly, to FALSE will result
#'   in returning additional file meta data related to file size, modified
#'   date/time and create date/time.
#'
#'   The results are in descending order of modified date/time.
#' @examples
#' # name of most recently modified file
#' \dontrun{
#' .findFile()         # current directory
#' .findFile("..")     # one directory up
#' #
#' # list of files and common attributes one directory up
#' .findFile(folder="..", file="*.*", n=2, fileNameOnly=FALSE)      #two most recent files
#' .findFile(folder="..", file="*.*", n="all", fileNameOnly=FALSE)  #all files
#' }
#' @return returns file name as a character string
#' @keywords internal
#' @export
.findFile <- function(folder='.', file='*.*', n=1, fileNameOnly=TRUE) {

# ----- Change history --------------------------------------------
# 12Jul2016: JBH: added comments
# 26Jun2016: JBH: code updated to remove dplyr dependency
# 12Mar2019: EWL: Add "dot" to examples.

# Access list of files #####
  fileName <- data.frame(fileName=list.files(path = folder, utils::glob2rx(file), ignore.case=TRUE))

# Store current working directory and change to subdirectory ####
  tmpSave <- getwd()
  tmpwd   <- file.path(folder)
  setwd(tmpwd)

# Access file information ####
  fileInfo <- file.info(dir(), extra_cols = FALSE)
  fileInfo$fileName <- rownames(fileInfo)
  rownames(fileInfo) <- NULL
  fileInfo <- merge(fileInfo,fileName, by='fileName', all.y=TRUE)
  fileInfo <- fileInfo[with(fileInfo, order(mtime, decreasing = TRUE)), ]

# Reset working directory
  setwd(tmpSave)

# Down select file information to match user request
  if(n=='all') n=nrow(fileInfo)

  if(fileNameOnly) {
    return(fileInfo[1:n,"fileName"])
  } else {
    return(fileInfo[1:n,])
  }

}
