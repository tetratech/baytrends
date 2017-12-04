#' Flow Averaged Predictions
#'
#' Compute flow[salinity] weighted predictions
#'
#' @param pdat prediction data set
#' @param pgam gam model
#' @param normPct vector of percentiles to use in weighting
#' @return data frame of flow[salinity] averaged predictions and standard errors
#' @export
#'
flwAveragePred <- function(pdat, pgam, normPct) {
  normQ   <- qnorm(normPct)     # compute the quantiles
  normD   <- dnorm(normQ)       # density fct
  flw.wgt <- normD/sum(normD)   # normalize density to equal 1
  
  n       <- nrow(pdat)         # number of prediction dates
  m       <- length(normPct)    # number of flow predictions per date
  pdflw   <- expand.grid(pdat$date,normQ) # create a vector of flow quantiles of length mn
  names(pdflw) <- c('date','normQ')
  
  f.dat <- merge(pdat,pdflw,by=c('date'),all=TRUE) # create a prediction data set with m flow records per date
  f.dat <- f.dat[order(f.dat$date,f.dat$normQ),]
  f.dat$flw_sal <- f.dat$normQ*f.dat$flw_sal.sd    # get flow quantiles as product of normal quantiles as flow SD
  f.dat$pred    <- predict(pgam,newdata=f.dat)     # compute flow predictions
  fav.lp  <- predict(pgam,newdata=f.dat,type="lpmatrix")  # get prediction matrix for flow predictions 
  VCmat   <- pgam$Vp                  # variance-covariance matrix of coefficents
  beta    <- pgam$coefficients        # coefficients vector
  
  farow   <- c(flw.wgt, rep(0,n*m))     # create 1st row + m extra zeros
  favec   <- c(rep(farow,n-1),flw.wgt)  # build a long vector
  fa.mat  <- matrix(favec,nrow=n,byrow=TRUE)  # put the long vector into a matrix
  
  # create a new flow averaged (fav) column on the p.dat frame by averaging the 
  # sequential groups of m predictions in the f.dat frame
  pdat$fav.pred <- fa.mat%*%f.dat$pred  
  fa.lp         <- fa.mat%*%fav.lp  #multiply flow average matrix times linear predictors
  pdat$fav.se  <- sqrt(diag(fa.lp%*%VCmat%*%t(fa.lp)))   # compute Std. Err. by usual rules
  
  return(pdat[,c("fav.pred","fav.se")])
}