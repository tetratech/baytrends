########################################################################
#' Check Data Range -- function that checks for allowable values
#'
#' Check Data Range -- function that checks for allowable values.
#'
#' @param df Data frame with data to check
#' @param var Variable to perform screen check on
#' @param varScrn Range to check (see examples)
#' @param numNA How to treat missing numeric values (TRUE: treat as pass,
#'   FALSE[default]: treat as fail)
#' @param deleteOption Option for how to return df ("pass": return rows that
#'   pass check, "fail": return rows that fail check, "mark": return column with
#'   TRUE/FALSE for pass/fail)
#' @return data frame modified based on user selected options. see attributes
#'   for screening results
#' @examples
#' # create an example data frame
#' df <- data.frame(
#'        x1 = c("X1","Y2","A1","B2","C1", "X1","","A1","","C1"),
#'        x2 = seq(5, 14 ) + runif(10) ,
#'        x3 = as.POSIXct(c("1/10/2008", "1/21/2008", "3/1/2008", "3/26/1993",
#'                          "11/1/2012", "6/10/2000", "8/2/1990", "7/8/2005",
#'                          "1/6/2008", "9/11/2008"),
#'                          format="%m/%d/%Y"), stringsAsFactors =FALSE)
#' # add a few missing values
#' df[1,1]=NA
#' df[3,2]=NA
#' df[5,3]=NA
#' df
#'
#' # establish allowable values for screening
#' x1Scrn <- as.character(c("A1", "B2", "C1", "Y2"))   # character
#' x2Scrn <- c(7,13)                                   # min/max value
#' x3Scrn <- as.POSIXct(c("1999-01-01", "2008-09-10")) # min/max date (POSIXct format)
#'
#' # return df with new column indicating pass [TRUE] / fail [FALSE]
#' .checkRange(df, var="x1", varScrn=x1Scrn, numNA=FALSE, deleteOption='mark')
#' .checkRange(df, var="x2", varScrn=x2Scrn, numNA=FALSE, deleteOption='mark')
#' .checkRange(df, var="x3", varScrn=x3Scrn, numNA=FALSE, deleteOption='mark')
#'
#' # return df with only rows that pass check
#' .checkRange(df, var="x1", varScrn=x1Scrn, numNA=FALSE, deleteOption='pass')
#' .checkRange(df, var="x2", varScrn=x2Scrn, numNA=FALSE, deleteOption='pass')
#' .checkRange(df, var="x3", varScrn=x3Scrn, numNA=FALSE, deleteOption='pass')
#' @export
#'
.checkRange <-function(df, var, varScrn=NULL, numNA=FALSE, deleteOption="pass") {

  # internal function
  is.POSIXct <- function(x) inherits(x, "POSIXct")
  is.Date <- function(x) inherits(x, "Date")

  # determine what column the variable that is being tested for is located in
  # and assign that column number to iCol
  iCol <- grep(paste0("^",var,"$") , colnames(df))

  # error trap ... stop if var is not found in df
  if (length(iCol) == 0) {
    warning(paste0("The variable, ", var,", was not found in the data frame."))
    attr(df,paste0(var,"NotInRangeNum")) <- "Not evaluated"
    return(df)
  }

  # error trap ... stop if the domain list or range contained in varScrn is null
  if (length(varScrn) == 0) {
    warning(paste0("The domain list or range for checking was NULL. No screen check performed."))
    attr(df,paste0(var,"NotInRangeNum")) <- "Not evaluated"
    return(df)
  }

  # Begin checking. There are currently three options: i) character/factor,
  #                 ii) POSIXct, and iii) numeric executed in an if/else structure
  #                 There is an error trap "iv)" if one of the above 3 dont catch the
  #                 variable type

  # i) Process for character or factor variables
  if (is.factor(df[,iCol]) | is.character(df[,iCol])) {
    # error trap ... varScrn has to be same object type as var
    if  (!(is.factor(varScrn) | is.character(varScrn)))  {
      warning(paste0("The domain list in varScrn is not same type as ", var,"."))
      attr(df,paste0(var,"NotInRangeNum")) <- "Not evaluated"
      return(df)
    }
    # do check and store summary information in attribute
    dfScrn <- data.frame( id= c(1:nrow(df)) )
    dfScrn$Scrn <- df[,iCol] %in% varScrn
    attr(df,paste0(var,"NotInRangeVal")) <- setdiff(unique(df[,iCol])  ,varScrn)
    attr(df,paste0(var,"NotInRangeNum")) <- nrow(df) - sum(dfScrn$Scrn)
    if ( length(attr(df,paste0(var,"NotInRangeNum")))==0) {
      attr(df,paste0(var,"NotInRangeNum")) <- 0
    }

    # ii) Process POSIXct
  } else if (is.POSIXct(df[,iCol]) ) {
    # error trap ... varScrn has to be same object type as var
    if  (!(is.POSIXct(varScrn) ))  {
      warning(paste0("The domain list in varScrn is not same type as ", var,"."))
      attr(df,paste0(var,"NotInRangeNum")) <- "Not evaluated"
      return(df)
    }
    # error trap if varScrn[2]<varScrn[1]
    if  (varScrn[2]<varScrn[1] )  {
      warning(paste0("The 1st value in varScrn is greater than 2nd value."))
      attr(df,paste0(var,"NotInRangeNum")) <- "Not evaluated"
      return(df)
    }
    # do check and store summary information in attribute
    dfScrn <- data.frame( id= c(1:nrow(df)) )
    dfScrn$Scrn <- df[,iCol] >= varScrn[1] & df[,iCol] <= varScrn[2]
    dfScrn$Scrn[is.na(dfScrn$Scrn)]   <- numNA
    attr(df,paste0(var,"Range")) <- varScrn
    attr(df,paste0(var,"NotInRangeNum")) <- nrow(df) - sum(dfScrn$Scrn)
    if ( length(attr(df,paste0(var,"NotInRangeNum")))==0) {
      attr(df,paste0(var,"NotInRangeNum")) <- 0
    }

    # iii) Process NUMERIC
  } else if (is.numeric(df[,iCol]) ) {
    # error trap ... varScrn has to be same object type as var
    if  (!(is.numeric(varScrn) ))  {
      warning(paste0("The domain list in varScrn is not same type as ", var,"."))
      attr(df,paste0(var,"NotInRangeNum")) <- "Not evaluated"
      return(df)
    }
    # error trap if varScrn[2]<varScrn[1]
    if  (varScrn[2]<varScrn[1] )  {
      warning(paste0("The 1st value in varScrn is greater than 2nd value."))
      attr(df,paste0(var,"NotInRangeNum")) <- "Not evaluated"
      return(df)
    }
    # do check and store summary information in attribute
    dfScrn <- data.frame( id= c(1:nrow(df)) )
    dfScrn$Scrn <- df[,iCol] >= varScrn[1] & df[,iCol] <= varScrn[2]
    dfScrn$Scrn[is.na(dfScrn$Scrn)]   <- numNA
    attr(df,paste0(var,"Range")) <- varScrn
    attr(df,paste0(var,"NotInRangeNum")) <- nrow(df) - sum(dfScrn$Scrn)
    if ( length(attr(df,paste0(var,"NotInRangeNum")))==0) {
      attr(df,paste0(var,"NotInRangeNum")) <- 0
    }

    # iv) throw error ... if the variable, var, doesn't fit in one of the above categories
  } else {
    warning(paste0("Error checking is not enabled for this variable type."))
    return(df)
  }

  # return Scrn results (see description associated with "deleteOption")
  if (deleteOption=="pass") {
    df<-df[ (dfScrn[,2]==TRUE), ]
  } else if  (deleteOption=="fail") {
    df<-df[ (dfScrn[,2]==FALSE), ]
  } else {
    df$dfScrn <- dfScrn$Scrn
  }

  return(df)
}
