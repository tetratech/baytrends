# ####
#' Load/Clean Excel sheet
#'
#' Load and clean one sheet from an Excel file
#'
#' @param file file (can use wildcards, e.g., "*.xlsx")
#' @param sheet worksheet name to load from Excel file
#' @param folder folder (i.e., directory to look in, can use relative path )
#' @param pk vector of columns that form the primary key for data set
#' @param remDup logical field indicating whether duplicate rows are deleted
#' @param remNAcol logical field indicating whether columns with all NA are deleted
#' @param remNArow logical field indicating whether rows with all NA are deleted
#' @param convDates vector or logical field indicating whether date-like columns
#'   should be converted to POSIXct format (see details)
#' @param tzSel time zone to use for date conversions (default: "America/New_York")
#'
#' @details This function reads in a single sheet from an Excel file using
#'   readxl::read_excel to load the data
#'
#'   After reading data in with readxl::read_excel, some specific additional
#'   steps are implemented:
#'
#'   1. Double quotes are removed from beginning and ending of all
#'   fields. The purpose is to maintaion leading zeroes (e.g., USGS flow gages).
#'   To effectively use this functionality, data maintained in a spreadsheet
#'   would be enclosed in quotes (e.g., "01578310"). If exported to csv or txt
#'   files the field would be in triple quotes (e.g., """01578310"""). Any
#'   column read in as integer is converted to numeric.
#'
#'   2. Rows and columns with no data (i.e., all NA) are deleted unless default
#'   settings for remNAcol and remNArow are changed to FALSE.
#'
#'   3. Completely duplicate rows are deleted unless default setting for remDup
#'   is changed to FALSE.
#'
#'   4. If a primary key (either single or multiple columns) is selected, the
#'   function enforces the primary key by deleting duplicate entries based on
#'   the primary key. Columns corresponding to the primary key (when specified)
#'   are moved to the first columns.
#'
#'   5. If convDates is a vector (i.e., \code{c('beginDate', 'endDate')}), then
#'   a date conversion to \code{as.POSIXct} is attempted for the corresponding
#'   columns found in the input file. If TRUE, then a date conversion is
#'   attempted for all columns found in the input file with 'date' in the name,
#'   If FALSE, no date conversion is attempted.
#'
#'   Some other common time zones include the following: America/New_York,
#'   America/Chicago, America/Denver, America/Los_Angeles, America/Anchorage,
#'   America/Honolulu, America/Jamaica, America/Managua, America/Phoenix,
#'   America/Metlakatla
#'
#'   A brief table reporting the results of the import are printed.
#'
#'   Note that columns containing just F, T, FALSE, TRUE are stored as logical fields
#'
#' @return Returns data frame
#' @export
# ####
loadExcel <- function(file=NA, sheet=1, folder='.', pk=NA, remDup=TRUE, remNAcol=TRUE, remNArow=TRUE,
                     convDates=TRUE, tzSel="America/New_York"
                     , tables = TRUE) {

# ----- Change history --------------------------------------------
# 18May2020: JBH: updated to handle input of Excel sheets with just 1 column 
# 20Jul2017: JBH: modified for excel files
# 14Mar2017: JBH: added to baytrends package
# 02Mar2017: JBH: added naChar option
# 02Dec2016: JBH: moved file to first argument
# 18Nov2016: JBH: columns with only NA are set to character fields;
#                 added list of example time zones to documentation
# 06Nov2016: JBH: reviewed documentation
# 29Oct2016: JBH: added code to process date fields
# 12Jul2016: JBH: added code to ignore rows beginning with '#'

# Function to extract file extension from a file name ####
  fileExtension <- function (x) {
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
  }

# Pick up file name #####
  if(is.na(folder) & is.na(file)) {
    # open windows explorer window
    tmp <- file.choose()
    if(is.na(tmp)) return('No file chosen.')
    fname  <- basename(tmp)
    folder <- dirname(tmp)
  } else if(!is.na(folder) & !is.na(file)) {
    # pick a file based on search criteria
    fname <- .findFile(folder=folder, file=file )
    if(is.na(fname)) stop('No file not found.')
  } else if(is.na(folder) & !is.na(file)) {
    # pick a file in current working directory
    if(is.na(folder)) folder <- '.'
    fname <- .findFile(folder=folder, file=file )
    if(is.na(fname)) stop('No file not found.')
  } else {
    return('No file chosen.')
  }

# # append naChar check 02Mar2017
#   if(!is.na(naChar)) naChar = c(NA, naChar)

# Read in file as either csv or tab delimited ####
  if (fileExtension(fname) %in% c("xls", "xlsx", "xlsm")) {
    df <- data.frame( readxl::read_excel(file.path(folder,fname), sheet=sheet))
  } else {
    stop('File extension is not recognized.')
  }

# Confirm primary key is in list of variables ####
  if (!is.na(pk[1]) & !(length(intersect(pk,names(df)))==length(pk))) {
    stop(paste0("Primary key, ",pk,", not found in data set."))
  }

# Clean up data ####
  # Clean up character fields: trim leading/trailing quote; set "" to NA
  varChar <- sapply(df, is.character)
  for (j in 1:length(df)) {
    if(varChar[j]) {
      #trim trailing "
      iTmp <- !is.na(df[,j]) & substring(df[,j],nchar(df[,j]),nchar(df[,j]))=="\""
      df[iTmp, j] <- substring(df[iTmp,j],1,nchar(df[iTmp,j])-1)
      #trim leading "
      iTmp <- !is.na(df[,j]) & substring(df[,j],1,1)=="\""
      df[iTmp, j] <- substring(df[iTmp,j],2)
      #set "" to NA
      iTmp <- !is.na(df[,j]) & df[,j]==""
      df[iTmp, j] <- rep(NA,sum(iTmp))
    }
  }

  # set NA fields to char
  i <- sapply(df, is.logical)
  varLog <- names(df)[i]
  for (var in varLog) {
    if (sum(!is.na(df[,var]))==0) {
      df[,var] <- as.character(df[,var])
    }
  }

  # set integer fields to numeric
  i <- sapply(df, is.integer)
  df[i] <- lapply(df[i], as.numeric)

# Initialize load summary table ####
  loadResult <- data.frame(Description= c("1) File Name / Sheet",
                                          "2) Folder Name",
                                          "3) Primary Key",
                                          "4) Rows Read In",
                                          "5) Columns Read In") ,
                           Value   =    c( paste0(fname ," / ", sheet) ,
                                           folder ,
                                           paste(pk, collapse = ' + ') ,
                                           nrow(df) ,
                                           length(df) ),
                           stringsAsFactors = FALSE)

# Trim null rows, null columns, duplicates ####
  # trim records with null rows
  if(remNArow & ncol(df)>1) {
    df <- df[rowSums(is.na(df))<length(df), ]
    loadResult <- rbind(loadResult, data.frame(Description="6) Rows After Blank Rows Removed",
                                               Value=nrow(df)))
  } else {
    loadResult <- rbind(loadResult, data.frame(Description="6) Rows After Blank Rows Removed",
                                               Value='n/a'))
  }

  # trim records with null columns
  if(remNAcol & ncol(df)>1) {
    df <- df[, colSums(is.na(df))<nrow(df)]
    loadResult <- rbind(loadResult, data.frame(Description="7) Columns After Blank Columns Removed",
                                               Value=length(df)))
  } else {
    loadResult <- rbind(loadResult, data.frame(Description="7) Columns After Blank Columns Removed",
                                               Value='n/a'))
  }

  # trim duplicate rows
  if(remDup & ncol(df)>1) {
    df<-df[!duplicated(df), ]
    loadResult <- rbind(loadResult, data.frame(Description="8) Rows After Duplicate Rows Removed",
                                               Value=nrow(df)))
  } else {
    loadResult <- rbind(loadResult, data.frame(Description="8) Rows After Duplicate Rows Removed",
                                               Value='n/a'))
  }

  # trim duplicate rows based on pk and move pk file to first columns
  if(!is.na(pk[1]) & ncol(df)>1) {
    df<-df[!duplicated(df[pk]), ]
    df<-df[c(pk, setdiff(names(df), pk))]
    loadResult <- rbind(loadResult, data.frame(Description=paste("9) Rows After Duplicate PK Removed"),
                                               Value=nrow(df)))
  } else {
    loadResult <- rbind(loadResult, data.frame(Description=paste("9) Rows After Duplicate PK Removed"),
                                               Value='n/a'))
  }

# Convert dates ####

  loadResultNum <- 9

  # identify potential fields
  if(convDates==TRUE) {
    iDateFields <- sort(grep("date",tolower(names(df))))
  } else if (!convDates==TRUE) {
    iDateFields <- sort(which(names(df) %in% convDates)  )
  }

  # perform evaluation for each field
  for (i in iDateFields) {
    df[,i] <- as.POSIXct(paste(lubridate::year(df[,i]),
                               month = lubridate::month(df[,i]),
                               day   = lubridate::day(df[,i]), format="%Y-%m-%d", sep="-"),
                         tz    = "America/New_York")
  }
    


# Print a summary report and return ####
  if (tables) {
    print(knitr::kable(loadResult))
  }

  return(df)

# ####
}
