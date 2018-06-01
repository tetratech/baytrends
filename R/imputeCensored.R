###############################################################################
#' Impute Censored Values   
#'
#' Impute value for multiply censored data.
#'
#' @param x vector of type baytrends::qw  
#' @param imputeOption imputation method [default= "mid"], valid impute options
#'   are "lower", "upper", "mid", "norm", "lnorm"
#' @details
#' The imputeOption values of lower, upper and mid impute the lower limit, upper limit,
#' and midpoint between the lower and upper limit. In the context of typical water quality
#' data, these options would be eqivalent to zero, detection limit and 1/2 detection limit
#' substitution. Options for substituting the normal ["norm"] or lognormal ["lnorm"] expectation
#' can also be used.
#' 
#' @examples
#' x  <- dataCensored[1:20,"tdp"]
#' x.lower <- impute(x,'lower')
#' x.mid   <- impute(x,'mid')
#' x.upper <- impute(x,'upper')
#' x.norm  <- impute(x,'norm')
#' x.lnorm <- impute(x,'lnorm')
#' @return vector where x is transformed into a simple numeric variable
#' @export
#
impute <-function(x, imputeOption="mid") { 
  
# ----- Change history --------------------------------------------
# 01Jun2018: JBH: Updated impute function to include maximum likelihood
#                 estimation assuming normal and lognormal distribution
# 01May2018: JBH: changed function name from .impute to impute; focus
#                 lower, mid and upper options; accomodate NAs
# 11Oct2016: JBH: corrected documentation, switched to explicit '::'
#                 for external function calls 

# Initialization ####
  
  # Set valid imputeOption
  validImpute <- c("lower", "upper", "mid", "uniform", "norm", "lnorm")  #21May2018
  # validImpute <- c("lower", "upper", "mid", "uniform", "norm", "lnorm")

# Error traps ####
  
  # Error trap ... make sure x variable is a qw class
  if (!(class(x) %in% c("mcens", "qw"))) {
    stop("input vector not of type qw.")
  }
  
  # Error trap ... make sure imputeOption is one of the valid choices
  if (!(imputeOption %in% validImpute)) {
    stop("imputeOption not valid.")
  }
  
  ### If there are -Inf lower limits or Inf upper limits, then reset the 
  ###    lower and upper limits to address -Inf and Inf values
  
  if (any(x@.Data[,1] == -Inf, na.rm=TRUE) | any(x@.Data[,2] == Inf, na.rm=TRUE)) {  
    # pull out lower/upper values into easy-to-read vectors
    lower  <- x@.Data[,1]
    upper  <- x@.Data[,2]
    
    # compute lower reset: 25% lower than smallest value 
    lowerReset1 <- min( lower [!(lower == -Inf)] , na.rm=TRUE) # smallest lower
    lowerReset2 <- min( upper [!(upper == -Inf)] , na.rm=TRUE) # smallest upper
    lowerReset  <- min(lowerReset1, lowerReset2, na.rm=TRUE) - 
      0.25 * abs(min(lowerReset1, lowerReset2, na.rm=TRUE))
    
    # compute upper reset: 25% larger than non-Inf value
    upperReset1 <- max( lower [!(lower == Inf)], na.rm=TRUE) # largest non-Inf lower
    upperReset2 <- max( upper [!(upper == Inf)], na.rm=TRUE) # largest non-Inf upper
    upperReset  <- max(upperReset1, upperReset2, na.rm=TRUE) + 
      0.25 * abs(max(upperReset1, upperReset2, na.rm=TRUE))
    
    # apply lower and upper reset values
    x@.Data[x@.Data[,1] == -Inf , 1] <- lowerReset
    x@.Data[x@.Data[,2] ==  Inf , 2] <- upperReset  
  }

# Impute censored values ####
  
  # 01May2018: update to accomodate vector with NA's
  # create vector of NAs equal to the length of the input vector
  x2 <- rep(NA,length(x))
  
  if (imputeOption == "lower") {
    x2 <- x@.Data[,1]
  } else if (imputeOption == "upper") {
    x2 <- x@.Data[,2]
  } else if (imputeOption == "mid") {
    x2 <- 0.5 * (x@.Data[,1] + x@.Data[,2])
  } else if (imputeOption == "uniform") {
    x2[!is.na(x)] <- stats::runif(sum(!is.na(x)), x@.Data[!is.na(x),1], x@.Data[!is.na(x),2])
  } else if (imputeOption == "norm") {
    myData1 <- data.frame(x=x, left=x@.Data[,1], right=x@.Data[,2])
    fn1     <- fitdistrplus::fitdistcens(myData1[!is.na(myData1$x),], distr="norm")
    mu.norm <- fn1$estimate[1]
    sd.norm <- fn1$estimate[2]
    myData1$normExpec <- .ExpNmCens(myData1,dep="x",mu.norm,sd.norm)[,1]
    x2 <- myData1$normExpec
  } else if (imputeOption == "lnorm") {
    myData1  <- data.frame(x=x, left=x@.Data[,1], right=x@.Data[,2])
    fn1      <- fitdistrplus::fitdistcens(myData1[!is.na(myData1$x),], distr="lnorm")
    mu.lnorm <- fn1$estimate[1]
    sd.lnorm <- fn1$estimate[2]
    myData1$lnormExpec <- .ExpLNmCens(myData1,dep="x",mu.lnorm,sd.lnorm)[,1]
    x2 <- myData1$lnormExpec
  } 
  
  x <- x2

# Return ####   
  return(x)
}

# simCensored ####
#' Fit normal or log normal distribution; simulation in censored data
#' provided by Elgin Perry
#' phi.left and phi.right are distribution function, e.g., pnorm(q=10,mean=10,sd=1) =0.5
#' phi.sim is a uniform random number between phi.left and phi.right
#' sim is imputed value
.simCensored <- function(x,distr="norm") {
  
  tmp <- data.frame(left = x@.Data[,1], 
                    right = x@.Data[,2])
  fn1 <- fitdistrplus::fitdistcens(tmp, distr=distr)
  summary(fn1)
  if(distr=="norm")
  {                    
    tmp$phi.left  <- stats::pnorm(tmp$left,mean=fn1$estimate[1],sd=fn1$estimate[2])
    tmp$phi.right <- stats::pnorm(tmp$right,mean=fn1$estimate[1],sd=fn1$estimate[2])
    tmp$phi.sim   <- stats::runif(nrow(tmp),tmp$phi.left,tmp$phi.right)
    tmp$sim       <- stats::qnorm(tmp$phi.sim,mean=fn1$estimate[1],sd=fn1$estimate[2])
  }
  if(distr=="lnorm")
  {                    
    tmp$phi.left  <- stats::plnorm(tmp$left,meanlog=fn1$estimate[1],sdlog=fn1$estimate[2])
    tmp$phi.right <- stats::plnorm(tmp$right,meanlog=fn1$estimate[1],sdlog=fn1$estimate[2])
    tmp$phi.sim   <- stats::runif(nrow(tmp),tmp$phi.left,tmp$phi.right)
    tmp$sim       <- stats::qlnorm(tmp$phi.sim,meanlog=fn1$estimate[1],sdlog=fn1$estimate[2])
  }
  return(tmp$sim)
}