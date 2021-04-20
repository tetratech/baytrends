# ####
#' Load/Clean CSV and TXT Data File
#'
#' Load and clean comma delimited (*.csv) or tab delimited (*.txt) file and
#' perform some rudimentary data cleaning.
#'
#' @param file file (can use wildcards, e.g., "*.csv")
#' @param folder folder (i.e., directory to look in, can use relative path )
#' @param pk vector of columns that form the primary key for data set
#' @param remDup logical field indicating whether duplicate rows are deleted
#' @param remNAcol logical field indicating whether columns with all NA are deleted
#' @param remNArow logical field indicating whether rows with all NA are deleted
#' @param convDates vector or logical field indicating whether date-like columns
#'   should be converted to POSIXct format (see details)
#' @param tzSel time zone to use for date conversions (default: "America/New_York")
#' @param commChar character for comment line to be skipped
#' @param naChar characters to treat as NA
#'
#' @details This function reads in a single comma delimited (*.csv) or tab
#'   delimited (*.txt) file using either \code{utils::read.table} or \code{utils::read.csv}
#'   based on the file extension. The user can use the wildcard feature for the
#'   file argument (e.g., file='*.csv') and the function will identify the most
#'   recently modified csv or txt file in the folder for importing.
#'
#'   Some specific features of this function include the following:
#'
#'   1. Leading '0's in character strings that would otherwise be trimmed and
#'   treated as numeric variables (e.g., USGS flow gages, state and county FIPS
#'   codes) are preserved. To effectively use this functionality, data
#'   maintained in a spreadsheet would be enclosed in quotes (e.g., "01578310").
#'   When exported to csv or txt files the field would be in triple quotes
#'   (e.g., """01578310"""). Any column read in as integer is converted to numeric.
#'
#'   2. Rows and columns with no data (i.e., all NA) are deleted unless default
#'   settings for remNAcol and remNArow are changed to FALSE.
#'
#'   3. Completely duplicate rows are deleted unless default setting for remDup
#'   is changed to FALSE.
#'
#'   4. Rows beginning with '#' are skipped unless commChar set to ""
#'
#'   5. If a primary key (either single or multiple columns) is selected, the
#'   function enforces the primary key by deleting duplicate entries based on
#'   the primary key. Columns corresponding to the primary key (when specified)
#'   are moved to the first columns.
#'
#'   6. If convDates is a vector (i.e., \code{c('beginDate', 'endDate')}), then a date
#'   conversion is attempted for the corresponding columns found in the input
#'   file. If TRUE, then a date conversion is attempted for all columns found in
#'   the input file with 'date' in the name, If FALSE, no date conversion is
#'   attempted.
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
loadData <- function(file=NA, folder='.', pk=NA, remDup=TRUE, remNAcol=TRUE, remNArow=TRUE,
                     convDates=TRUE, tzSel="America/New_York", commChar="#", naChar=NA ) {

# ----- Change history --------------------------------------------
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

# append naChar check 02Mar2017
  if(!is.na(naChar)) naChar <- c(NA, naChar)

# Read in file as either csv or tab delimited ####
  if (fileExtension(fname)=="csv") {
    df <- utils::read.csv(file.path(folder,fname), header=TRUE,
                   sep = ",", quote = "\"", na.strings=naChar, comment.char = commChar,
                   dec=".", strip.white=TRUE, stringsAsFactors = FALSE)
  } else if (fileExtension(fname)=="txt") {
    df <- utils::read.table(file.path(folder,fname), header=TRUE,
                     sep = "\t", quote = "\"", na.strings=naChar, comment.char = commChar,
                     dec=".", strip.white=TRUE, stringsAsFactors = FALSE)
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
  loadResult <- data.frame(Description= c("1) File Name",
                                          "2) Folder Name",
                                          "3) Primary Key",
                                          "4) Rows Read In",
                                          "5) Columns Read In") ,
                           Value   =    c( fname ,
                                           folder ,
                                           paste(pk, collapse = ' + ') ,
                                           nrow(df) ,
                                           length(df) ),
                           stringsAsFactors = FALSE)

# Trim null rows, null columns, duplicates ####
  # trim records with null rows
  if(remNArow) {
    df <- df[rowSums(is.na(df))<length(df), ]
    loadResult <- rbind(loadResult, data.frame(Description="6) Rows After Blank Rows Removed",
                                               Value=nrow(df)))
  } else {
    loadResult <- rbind(loadResult, data.frame(Description="6) Rows After Blank Rows Removed",
                                               Value='n/a'))
  }

  # trim records with null columns
  if(remNAcol) {
    df <- df[, colSums(is.na(df))<nrow(df)]
    loadResult <- rbind(loadResult, data.frame(Description="7) Columns After Blank Columns Removed",
                                               Value=length(df)))
  } else {
    loadResult <- rbind(loadResult, data.frame(Description="7) Columns After Blank Columns Removed",
                                               Value='n/a'))
  }

  # trim duplicate rows
  if(remDup) {
    df<-df[!duplicated(df), ]
    loadResult <- rbind(loadResult, data.frame(Description="8) Rows After Duplicate Rows Removed",
                                               Value=nrow(df)))
  } else {
    loadResult <- rbind(loadResult, data.frame(Description="8) Rows After Duplicate Rows Removed",
                                               Value='n/a'))
  }

  # trim duplicate rows based on pk and move pk file to first columns
  if(!is.na(pk[1])) {
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
    # test many date formats
    dateFormat<- lubridate::guess_formats(df[!is.na(df[,i]),i], c("dbY", "bdY", "mdY", "dmY", "Ymd", "Ymd HM",
                                         "mdY HM",  "Ymd HM p", "mdY HM p","dmy"))
    if (!is.null(dateFormat))  {
      # summarize potential options into a frequency table
      dateFormat <- as.data.frame(table(dateFormat))
      # pick date format (opt 1: perfect match; opt 2: save column as *.Orig and pick best format
      if (length(which(dateFormat$Freq == sum(!is.na(df[,i])))) > 0) {
        dateFormat <- dateFormat$dateFormat[min(which(sum(!is.na(df[,i])) == dateFormat$Freq))]
        dateFormatChkWrn <- ""
      } else {
        df[, paste0(names(df)[i],".Orig")] <- df[,i]
        dateFormat <- dateFormat[sum(!is.na(df[,i])) > dateFormat$Freq ,]
        dateFormat <- dateFormat$dateFormat[min(which.min(sum(!is.na(df[,i])) - dateFormat$Freq))]
        dateFormatChkWrn <- paste0("(Col: ",names(df)[i],".Orig added)")
      }

      # apply selected format
      if(!is.na(dateFormat)) {
        df[,i] <- lubridate::parse_date_time(df[,i], dateFormat, tz=tzSel)
      }
      loadResultNum <- loadResultNum + 1
      loadResult <- rbind(loadResult,
                          data.frame(Description=paste0(loadResultNum, ") ", names(df)[i] , " converted with"),
                                     Value=paste(dateFormat, 'format', dateFormatChkWrn)))
    }
  }


# Print a summary report and return ####
  print(knitr::kable(loadResult))

  return(df)

# ####
}
