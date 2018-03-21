#
# Install needed packages
#
# chkPackages <-function() {
#   packages <- c("lubridate", "pander", "dataRetrieval")
#   if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#     install.packages(setdiff(packages, rownames(installed.packages())))  
#   }
# }
# 
#   
#
# Functions adopted from baytrends or predecessor CBPstats (2017-11-24): 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create filter weights
#'
#' Create filter weights. Typically used to compute even,
#' weighted, or center-weighted averages for smoothing. Can
#' be used as a vector of weights in stats::filter
#'
#' @param n number of values
#' @param type  Averaging method ("even", "weighted" [default], or "centered") for creating weights.
#' @examples
#' wgts<- filterWgts(0,"even")
#' wgts<- filterWgts(7,"even")
#' wgts<- filterWgts(7,"centered")
#' wgts<- filterWgts(7,"weighted")
#' x <- 1:100
#' filter(x, filterWgts(7,"weighted"), sides=1)
#' @return Returns vector of weights
#' @keywords internal
#' @export
#'
filterWgts <-function(n=NULL , type="weighted") {
  
# Change history
# 27Jan2016: JBH: corrected 'weighted' option. previously vector was created
#            in increasing value, i.e., 1/6, 1/3, 1/2. This resulted in
#            applying the largest weight to the value furthest away. Now
#            'weighted' option does decreasing order, i.e., 1/2, 1/3, 1/6.
  
  # error trap ... n cannot be null
  if(is.null(n)) {
    stop("The value of n in filterWgts was NULL, please correct.")
  }
  
  # error trap ... n must be postive integer or zero
  if(!(all.equal(n, as.integer(n))) | n<0) {
    stop("The value of n is not an integer or is <0, please correct.")
  }
  
  # error trap ... type must be uniform, weighted, or centered
  if(!(type %in% c("uniform", "weighted", "centered"))) {
    stop("The value of type was not a valid choice. Please use 'uniform', 'weighted', or 'centered'.")
  }
  
  # error trap ... n must be odd if sides =2
  if((-1)**n==1 & type=="centered") {
    stop("Value of n must be odd if User ")
  }
  
  if(n==0 |n==1) {
    wgts <- 1
  } else if(type=="uniform") {
    wgts <- rep(1,n) / n
  } else if (type=="weighted") {
    wgts <- rep(n:1) / sum(rep(1:n))  #27Jan2016: wgts <- rep(1:n) / sum(rep(1:n))
  } else if (type=="centered") {
    wgts <- (n+1)/2 - abs((1:n) - (n+1)/2)
    wgts <- wgts / sum(wgts)
  } else {
    stop("Unknow option for filter weight encountered, please contact CBP.")
  }
  
  return(wgts)
  
}

# Function: Appends date features to data frame ####
# 
# Source: Derived from CBPstats::appendDateFeatures
# Date:   12Jul2017
# 
#' @keywords internal
#' @export
appendDateFeatures <-function(df, var="date") {
  
  # Create a columnn 'date' in the df. If date is already in df, it is truncated
  # day level. Throw error if var is not found in df
  if (var %in% names(df)) {
    df$date <-  trunc( df[, names(df)==var] , units='days')
  } else {
    stop(paste0("The field containing date, '",var, "', was not found in data set."))
    return(df)
  }
  
  # Create variables for year, doy, and yearDec (note special handling that
  # 2/28 and 3/1 are always doy = 59 and 61, respectively when using
  # smwrBase::baseDay)
  df$year  <- lubridate::year(df$date)
  df$doy   <- as.numeric(baseDay(df$date))
  df$dyear <- df$year + baseDay2decimal(baseDay(df$date, numeric=FALSE))
  
  # Create month variables
  df$month <- months(df$date, abbreviate = TRUE)
  df$month <- factor(df$month,levels=c("Jan","Feb","Mar","Apr","May","Jun",
                                       "Jul","Aug","Sep","Oct","Nov","Dec"))
  df$nummon <- as.numeric(df$month)
  
  # make sure date is in POSIXct format
  df$date <- as.POSIXct(df$date)
  
  return(df)
}

# Function: Figure title ####
# 
# Source: Derived from CBPstats::.F and CBPstats::.P
# Date:   12Jul2017
#
# @export
.F <- function(text, n=NULL, t='e') {
  if(is.null(n)) {
    title <- paste0("Figure: ",text)
  } else {
    title <- paste0("Figure ", n, ". ", text)
  }
  
  .P()
  if(t=="e") {
    pander::pandoc.emphasis(title)
  } else {
    pander::pandoc.strong(title)
  }
  .P()
  
}

# @export
.P <- function(text=NULL) {
  if(is.null(text)) {
    pander::pandoc.p(' ')
  } else {
    pander::pandoc.p(text)
  }
}

# @export
.H2 <- function(text) {
  pander::pandoc.header(text, 2)
}