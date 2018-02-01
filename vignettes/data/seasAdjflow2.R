#' Create Daily Seasonally-adjusted Log Flow Residuals
#'
#' Create Daily Seasonally-adjusted Log Flow Residuals. The procedure to compute
#' daily seasonally-adjusted log flow residuals is the following: 1) Check to
#' make sure that the raw flow data are in the data set and that seasonally
#' adjusted values have not already been computed. If so, no additional
#' computations are performed. If not, then proceed with remaining steps. 2) Add
#' date features if not already in the data set. 3) Compute and store Log (ln) flow
#' as 'LogQ...' 4) Compute GAM model and store seasonally adjusted LogQ flow
#' residuals as 'sa0LogQ...' 5) Smooth seasonally adjusted Log flow residuals by
#' an averaged values based on dvAvgWin, dvAvgWgt, and dvAvgSides and store as
#' "saxLogQ..."
#'
#' @param dvFlow data frame with daily flow data. The flow and data qualifier
#'   code for each site is organized into two columns (e.g., "q01594440",
#'   "q01594440cd" for USGS gage 01594440)
#' @param siteNumber a single USGS gage ID
#' @param dvAvgWin Averaging window (days) for smoothing the residuals of the
#'   seasonally adjusted daily flow values.
#' @param dvAvgWgt Averaging method ("uniform", "weighted" [default], or
#'   "centered") for creating weights. If using "weighted" then use
#'   dvAvgSides=1.  If using "centered" then use dvAvgSides=2.
#' @param dvAvgSides If dvAvgSides=1 only past values are used, if dvAvgSides=2
#'   then values are centered around lag 0.
#' @param plotResid plot residuals for selected averaging windows.
#'
#' @examples
#' #Set Retrieval Parameters
#' yearStart   <- 1983
#' yearEnd     <- 2015
#' siteNumbers <- c("01578310")
#'
#' # Regular Retrieval (default usage)
#' df <- getUSGSflow(siteNumbers, yearStart, yearEnd, fill=TRUE)
#' # Apply default smoothing
#' df <- seasAdjflow(df,"01578310")
#'
#' @return return data frame of flow data with additional seasonally adjusted values
#' @export
#'
seasAdjflow <- function(dvFlow=dvFlow, siteNumber=NULL, dvAvgWin=c(7,31),
                      dvAvgWgt="weighted", dvAvgSides=1, plotResid=c(1)) {
  
# -----< Change history >--------------------------------------------
  
  options(scipen=5)
  plotResid <- paste0("d",plotResid)
  
  # 0) Override user selection if there is a mismatch of dvAvgWgt, dvAvgSides,
  # or dvAvgWin
  if (dvAvgWgt=="weighted" & !(dvAvgSides==1)) {
    dvAvgSides <- 1
    warning("dvAvgSides set to 1 because of dvAvgWgt selection.")
  }
  
  if (dvAvgWgt=="centered" & !(dvAvgSides==2)) {
    dvAvgSides <- 2
    warning("dvAvgSides set to 2 because of dvAvgWgt selection.")
  }
  
  # 1) Check to make sure that the raw flow data are in the data set
  #    and that seasonally adjusted values have not already been computed.
  #
  # determine variable names
  varQ       <- paste0('q',siteNumber)
  varQ2      <- paste0('q')
  varLogQ    <- paste0('LogQ')
  varGAMQ    <- paste0(varQ,'.gam')
  # varLogQ    <- paste0('LogQ',siteNumber)
  
  # error trap for raw flow data
  if (!(varQ %in% names(dvFlow))) {
    stop(paste("Raw flow data for USGS Gage",siteNumber,"was not found."))
  }
  
  # If Log Q data for the gage is already in the df then, no need to re-do it
  if (varLogQ %in% names(dvFlow)) {
    return(dvFlow)
  }
  
  # find what column the flow data are in
  iCol   <- grep( paste0("^",varQ,"$") , colnames(dvFlow))
  
  
# Begin Calculations ####  
  # 2) Add date features if not already in the data set.
  #
  # add date features if not already in dvFlow
  if ( !("year"  %in% names(dvFlow) |
         "doy"   %in% names(dvFlow) |
         "month" %in% names(dvFlow) )) {
    dvFlow<-appendDateFeatures(dvFlow)
  }
  
  # 3) Take and store Log flow as 'LogQ...'
  #
  dvFlow[,varQ2]  <- dvFlow[,iCol]
  dvFlow[,varLogQ]  <- log(dvFlow[,iCol])
  
  # 4) Compute GAM model and store seasonally adjusted
  #    Log flowresiduals as 'sa1LogQ...'
  #
  LogQgam   <- mgcv::gam(dvFlow[,varLogQ] ~  s(dvFlow[,"doy"],bs='cc'))
  predLogQ  <- predict(LogQgam)
  dvFlow[,varGAMQ] <- exp(predLogQ)
  varsa1LogQ <- paste0('d1')
  dvFlow[,varsa1LogQ]  <- residuals(LogQgam)
  
  # plot daily flow 
  plot(dvFlow[,"doy"],dvFlow[,iCol]  , log="y", main=siteNumber,
       xlab="Day of Year", ylab="Daily Flow [cms]", las=1)
  lines(c(1:366),exp(predLogQ[1:366]),col='red',lwd=3)
  figNum <<- figNum + 1 
  title <- paste( "Daily Flow [cms] as a Function of Day of Year.",
                  "(Solid red line is the predicted GAM model using",
                  "the model: gam(LogQ ~ s(doy,bs='cc')).)")
  .F(title, figNum)
  
  # get y range on sa1LogQ so that graphics have same scale
  yrange= max(abs(dvFlow[,varsa1LogQ]), na.rm=TRUE )
  yrange= c(-1.0*yrange,yrange)
  
  # 5) Compute average seasonally adjusted Log flow residuals
  #    by smoothing windows and store as "saxLogQ..."
  
  # Create averagedf "saxLogQ..."
  for (iRow in dvAvgWin[!(dvAvgWin %in% 1)]) {
    varsaxLogQ <- paste0('d',iRow)
    wgts <- filterWgts( iRow  ,   dvAvgWgt)
    dvFlow[,varsaxLogQ]  <- filter(dvFlow[,varsa1LogQ], wgts, sides=dvAvgSides)
  }
  
  # Plot 
  for(iRow in dvAvgWin) {
    varsaxLogQ <- paste0('d',iRow)
    if (varsaxLogQ %in% plotResid) { 
      plot(dvFlow[,"doy"],dvFlow[,varsaxLogQ]  ,  main=paste0(siteNumber,' (Avg. Per.: ', varsaxLogQ, ")"),
           ylim=yrange,
           xlab="Day of Year", ylab="Seas. Adj. Res. LogQ ", las=1)
      lines(c(1,366),c(0,0),col='red',lwd=3)
      figNum <<- figNum + 1
      title <- paste0( iRow, "-day-smoothed Seasonally-adjusted Log Flow Residuals",
                       " as a Function of Day of Year.")
      .F(title, figNum)
    }
  }
  
  return(dvFlow)

}

