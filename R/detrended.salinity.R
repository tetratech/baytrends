#' @title Create Seasonally Detrended Salinty Data Set
#'
#' @description This function creates a seasonally detrended salinity data set for selected
#'   stations. The created data set is used to support application of GAMs
#'   that include a hydrologic term as one of the independent variables. The output
#'   from this function should be stored as an .rda file for repeated use with 
#'   baytrends.
#'   
#' @param df.sal data frame with salinty data (required variables in data frame
#'   are: station, date, layer, and salinity)
#' @param dvAvgWinSel Averaging window (days) selection for pooling data to 
#'   compute summary statistics
#' @param lowess.f lowess smoother span applied to computed standard deviation
#'   (see Details). This gives the proportion of points which influence the
#'   smooth at each value. Larger values give more smoothness.
#' @param minObs Minimum number of observations for performing analysis (default
#'   is 40)
#' @param minObs.sd Minimum number of observations in averaging window for
#'   calculation of the standard deviation (default is 10)
#' 
#' @return Returns a list of seasonally detrended salinity data. You should save
#'   the resulting list as salinity.detrended for use with baytrends.  This
#'   function also creates diagnostic plots that can be saved to a report when
#'   this function is called from an .Rmd script.
#' 
#' @details This function returns a list of seasonally detrended salinity and companion 
#'   statistics; and relies on a user supplied data frame that contains the
#'   following variables: station, date, layer, and salinity. See
#'   structure of sal data in example below.  
#'   
#'   It is the user responsibility to save the resulting list as
#'   \bold{salinity.detrended} for integration with baytrends.
#'   
#'   For the purposes of baytrends, it is expected that the user would identify
#'   a data set with all salinity data that are expected to be evaluated so that
#'   a single data file is created. The following computation steps are performed: 
#'   
#'   1) Extract the list of stations, minimum year, and maximum year in data set.
#'   Initialize the \bold{salinity.detrended} list with this information along with
#'   meta data documenting the retrieval parameters. 
#'   
#'   2) Downselect the input data frame to only include data where the 
#'   layer is equal to 'S', 'AP', 'BP' or 'B'.
#'   
#'   3) Average the 'S' and 'AP' salinity data; and the 'B' and 'BP salinity
#'   data together to create average salinity values for SAP (surface and above pycnocline)
#'   and BBP (bottom and below pycnocline), respectively. These values are stored
#'   as the variables, \bold{salinity.SAP} and \bold{salinity.BBP} together with the 
#'   \bold{date} and day of year (\bold{doy}) in a data frame corresponding to the
#'   station ID.
#'   
#'   4) For each station/layer combination with atleast \bold{minObs} observations,
#'   a seasonal GAM, i.e., gamoutput <- gam(salinity ~  s(doy, bs='cc')) is
#'   evaluated and the predicted values stored in the above data frame 
#'   as \bold{salinity.SAP.gam} and \bold{salinity.BBP.gam}.  
#'   
#'   5) The GAM residuals, i.e., "residuals(gamoutput)" are extracted and stored
#'   as the variable, \bold{SAP} or \bold{BBP} in the above data frame. (These are the 
#'   values that are used for GAMs that include salinity.) 
#'   
#'   6) After the above data frame is created and appended to the 
#'   list \bold{salinity.detrended}, the following four (4) additional
#'   data frames are created for each station.  
#'   
#'   \bold{mean} -- For each doy (i.e., 366 days of year), the mean across all 
#'   years for each value of d. Since samples are not collected on a daily basis
#'   it is necessary to aggregate data from within a +/- one-half of
#'   \bold{dvAvgWinSel}-day window around d. (This includes wrapping around the
#'   calendar year. That is, the values near the beginning of the year, say
#'   January 2, would include values from the last part of December and the
#'   first part of January. The variables in the mean data frame are doy, SAP, and BBP.   
#'   
#'   \bold{sd} -- For each doy (i.e., 366 days of year), the standard deviation 
#'   across all years for each value of d. (See mean calculations for additional details.)    
#'   
#'   \bold{nobs} -- For each doy (i.e., 366 days of year), the number of observations 
#'   across all years for each value of d. (See mean calculations for additional details.)     
#'   
#'   \bold{lowess.sd} -- Lowess smoothed standard deviations. It is noted that
#'   some stations do not include regular sampling in all months of the year or
#'   for other reasons have few observations from which to compute standard
#'   deviations. Through visual inspection of plots, we found that the standard
#'   deviation could become unstable when the number  of observations is small.
#'   For this reason, when the number of observations is less than
#'   \bold{minObs.sd}, the corresponding value of lowess.sd is removed and
#'   interpolated from the remaining observations.
#'    
#'   The above four data frames (mean, sd, nobs, and lowess.sd) are created, they
#'   are added to a list using a \bold{station.sum} naming convention and appended to the 
#'   list \bold{salinity.detrended}.
#'  
#' @importFrom utils modifyList  
#' @importFrom stats aggregate
#' @importFrom stats na.pass
#' @importFrom stats residuals
#' @importFrom stats sd
#' @importFrom stats lowess
#' @examples
#' # Show Example Dataset (sal)
#' str(sal)
#'
#' # Define Function Inputs
#' df.sal        <- sal
#' dvAvgWinSel   <- 30
#' lowess.f      <- 0.2
#' minObs        <- 40
#' minObs.sd    <- 10
#'                  
#' # Run Function
#' salinity.detrended <- detrended.salinity(df.sal, dvAvgWinSel, 
#'                                  lowess.f, minObs, minObs.sd)               
#' @export
detrended.salinity <- function(df.sal, dvAvgWinSel=30, lowess.f=0.2, 
                               minObs=40, minObs.sd=10) { ##FUNCTION.START
  
  # Initialize settings and load input salinity data ----
  {
    # * set up flow retrieval and seasonal adjustment parameters ----
    salinity.detrended <- list(analysisDate  = Sys.time(),
                               stations      = unique(df.sal$station),
                               yearStart     = year(min(df.sal$date)),
                               yearEnd       = year(max(df.sal$date)),
                               dvAvgWinSel   = dvAvgWinSel,
                               lowess.f      = lowess.f,
                               minObs        = 40,
                               minObs.sd     = 10 )
  }
  
  # Create SAP and BBP average salinity ----
  {  
    df <- df.sal
    
    # ** create SAP and BBP df's ----  
    df.SAP <- df[df$layer %in% c("S","AP"),]
    df.BBP <- df[df$layer %in% c("B","BP"),]
    
    # ** average salinity ----
    df.SAP <- aggregate(salinity ~ station + date,
                        data=df.SAP, FUN=mean, na.action=na.pass, na.rm=TRUE)
    df.BBP <- aggregate(salinity ~ station + date,
                        data=df.BBP, FUN=mean, na.action=na.pass, na.rm=TRUE)
    
    df.SAP$layer <- 'SAP'
    df.BBP$layer <- 'BBP'
    
    # ** combine averaged data ----  
    salinity<-rbind(df.SAP,df.BBP)
    rm(df.BBP,df.SAP, df.sal)
  } 
  
  # station by station processing ----
  for (i.station in salinity.detrended[["stations"]]        ) {

    # * Average salinity data ----
    # ** set up variable names for raw and summary data ----
    var       <- i.station
    var.sum   <- paste0(i.station, ".sum") 
    
    # ** create df of SAP and BBP salinity for ith station ----
    df <- salinity[salinity$station == i.station,]
    df <- reshape (df, v.names=c("salinity"), idvar=c("station", "date"),
                   timevar=c("layer"), drop=c(""), direction = "wide")
    attr(df,'reshapeWide') <-NULL
    
    # ** test for presense of at least minObs obs ---- 
    hasSAP <- if("salinity.SAP" %in% names(df)) TRUE else FALSE
    if(hasSAP) {
      hasSAP <- if(sum(!is.na(df[,'salinity.SAP'])) >= salinity.detrended$minObs) TRUE else FALSE  
    }
    hasBBP <- if("salinity.BBP" %in% names(df)) TRUE else FALSE
    if(hasBBP) {
      hasBBP <- if(sum(!is.na(df[,'salinity.BBP'])) >= salinity.detrended$minObs) TRUE else FALSE
    }
    print(paste0(var,' (Surface: ',hasSAP,' / Bottom: ',hasBBP,")"))
    
    # ** break out if nobs < minObs ----
    if(!hasBBP & !hasSAP) {
      tmp.list        <- list( data.frame(NA))
      names(tmp.list) <- var
      salinity.detrended  <- modifyList(salinity.detrended, tmp.list)
      names(tmp.list) <- var.sum
      salinity.detrended  <- modifyList(salinity.detrended, tmp.list)
      next
    }
    
    # * Perform gam modeling ----
    # ** add doy; sort column order ----
    df$doy   <- as.numeric(baytrends::baseDay(df$date))
    df<-df[,c('station', 'date', 'doy', setdiff(names(df), c('station', 'date','doy')))]
    
    # ** SAP gam model ----
    if(hasSAP) {
      # compute salinity gam models 
      gamSAP   <- mgcv::gam(df[,"salinity.SAP"] ~  s(df[,"doy"],bs='cc'))
      # compute/store predicted and residual salinities 
      df[!is.na(df$salinity.SAP),"salinity.SAP.gam"] <- predict(gamSAP)
      df[!is.na(df$salinity.SAP),"SAP"]              <- residuals(gamSAP)
    } else {
      df[,"salinity.SAP.gam"] <- NA_real_
      df[,"SAP"]              <- NA_real_
    }
    
    # ** BBP gam model ----
    if(hasBBP) {
      # compute salinity gam models 
      gamBBP   <- mgcv::gam(df[,"salinity.BBP"] ~  s(df[,"doy"],bs='cc'))
      # compute/store predicted and residual salinities 
      df[!is.na(df$salinity.BBP),"salinity.BBP.gam"] <- predict(gamBBP)
      df[!is.na(df$salinity.BBP),"BBP"] <- residuals(gamBBP)
    } else {
      df[,"salinity.BBP.gam"] <- NA_real_
      df[,"BBP"]              <- NA_real_
    }
    
    # ** put seasonal averages into a list ----
    # set embedded df to correspond to station id
    # append list to overall list 
    tmp.list        <- list(df[, !names(df) %in% c("station")]) 
    names(tmp.list) <- var
    salinity.detrended  <- modifyList(salinity.detrended, tmp.list)
    
    # * Compute doy based means and stdv ----
    # ** create artificial data set ----
    # append a +/- 1-year offset to allow for wrap around on data average window
    df.pos <- df.neg <- df
    df.neg$doy <- df.neg$doy - 366
    df.pos$doy <- df.pos$doy + 366
    df <- rbind( df.neg, df, df.pos); remove(df.neg, df.pos)
    
    # ** initialize data frames for mean, sd, and nobs ----
    df.mean <- data.frame(doy = as.numeric(c((1-366):(366+366)), SAP=NA_real_, BBP=NA_real_))
    df.sd   <- data.frame(doy = as.numeric(c((1-366):(366+366)), SAP=NA_real_, BBP=NA_real_))
    df.nobs <- data.frame(doy = as.numeric(c((1-366):(366+366)), SAP=NA_real_, BBP=NA_real_))
    
    # ** compute mean, sd, and obs for doy from 1:366 ----
    window <- salinity.detrended[["dvAvgWinSel"]]/2
    for (j in 1:1098) {
      j_day=df.mean[j,"doy"]
      tmp         <- (df[df$doy >= j_day-window & df$doy <= j_day+window, ])
      df.mean[j,"SAP"] <- mean  (tmp$SAP, na.rm=TRUE) 
      df.sd  [j,"SAP"] <- sd    (tmp$SAP, na.rm=TRUE)
      df.nobs[j,"SAP"] <- sum(!is.na(tmp$SAP))
      df.mean[j,"BBP"] <- mean  (tmp$BBP, na.rm=TRUE)
      df.sd  [j,"BBP"] <- sd    (tmp$BBP, na.rm=TRUE)
      df.nobs[j,"BBP"] <- sum(!is.na(tmp$BBP))
    }
    
    # ** compute lowess smooth ----
    df.lowess.sd <- as.data.frame(lowess(x=df.sd[,"doy"], 
                                         y=df.sd[,"SAP"] , 
                                         f= salinity.detrended$lowess.f))
    names(df.lowess.sd) <- c("doy","SAP")
    tmp          <- as.data.frame(lowess(x=df.sd[,"doy"], 
                                         y=df.sd[,"BBP"] , 
                                         f= salinity.detrended$lowess.f))
    names(tmp) <- c("doy","BBP")
    df.lowess.sd <- merge(df.lowess.sd, tmp, by='doy')
    
    
    # ** set lowess.sd to NA where nobs <= salinity.detrended$minObs.sd, then
    # use fillMissing to interpolate
    df.lowess.sd[df.nobs$SAP<=salinity.detrended$minObs.sd,"SAP"] <- NA
    df.lowess.sd$SAP <- fillMissing(df.lowess.sd$SAP, max.fill=9e9, span=1)
    
    df.lowess.sd[df.nobs$SAP<=salinity.detrended$minObs.sd,"BBP"] <- NA
    df.lowess.sd$BBP <- fillMissing(df.lowess.sd$BBP, max.fill=9e9, span=1)
    
    # now trim down to doy from 1:366 
    df.mean      <- df.mean[df.mean$doy %in% 1:366, ]
    df.sd        <- df.sd  [df.sd$doy   %in% 1:366, ]
    df.nobs      <- df.nobs[df.nobs$doy %in% 1:366, ]
    df.lowess.sd <- df.lowess.sd [df.lowess.sd$doy %in% 1:366, ]
    
    
    # ** put mean, sd, nobs, & lowess by doy into a list ----
    # set embedded df to correspond to station.sum
    # append list to overall list
    tmp.list        <- list(list(mean = df.mean, sd = df.sd, nobs = df.nobs, lowess.sd = df.lowess.sd))
    names(tmp.list) <- var.sum
    salinity.detrended  <- modifyList(salinity.detrended, tmp.list)
    
  }
  
  # Post processing check ----
  # are the list of stations in salinity.detrended the same as what i stored in 
  # salinity.detrended$stations
  names(salinity.detrended)[names(salinity.detrended) %in% salinity.detrended[["stations"]] ] ==
    salinity.detrended$stations
  
  # Plot results ----
  # plot mean, sd (and lowess.sd), and nobs per doy 
  
  # QC, 20180502
  # figNum not defined before used.
  if(!exists("figNum")){
    figNum<-0
  }
  
  figNum <<- 0
  
  for (station in salinity.detrended[["stations"]]) {
    
    station.sum <- paste0 (station ,".sum" )
    
    station.mean      <- salinity.detrended[[station.sum]][["mean"]][["SAP"]]
    station.sd        <- salinity.detrended[[station.sum]][["sd"]][["SAP"]]
    station.doy       <- salinity.detrended[[station.sum]][["sd"]][["doy"]]
    station.lowess.sd <- salinity.detrended[[station.sum]][["lowess.sd"]][["SAP"]]
    station.nobs      <- salinity.detrended[[station.sum]][["nobs"]][["SAP"]]
    
    if(is.null(station.mean)) next
    
    # plots to screen
    
    par(mfrow = c(3, 1))
    
    plot(station.doy, station.mean , ylim=c(-3.,3.),
         xlab=NA, ylab="mean"); title(paste0(station,"--SAP"));
    
    plot(station.doy, station.sd , ylim=c(0,6), col='grey',
         xlab=NA, ylab="sd"); #title(station); 
    
    lines(station.doy, station.lowess.sd, col='red',lwd=2)
    plot(station.doy, station.nobs , ylim=c(0,80) ,
         xlab='Day of Year', ylab="Nobs."); #title(station);
    #
    
    figNum <<- figNum + 1
    title <- paste0( "SAP mean, standard deviation, and number of observations ",
                     " as a Function of Day of Year.")
    .F(title, figNum)
  }
  
  for (station in salinity.detrended[["stations"]]) {
    
    station.sum <- paste0 (station ,".sum" )
    
    station.mean      <- salinity.detrended[[station.sum]][["mean"]][["BBP"]]
    station.sd        <- salinity.detrended[[station.sum]][["sd"]][["BBP"]]
    station.doy       <- salinity.detrended[[station.sum]][["sd"]][["doy"]]
    station.lowess.sd <- salinity.detrended[[station.sum]][["lowess.sd"]][["BBP"]]
    station.nobs      <- salinity.detrended[[station.sum]][["nobs"]][["BBP"]]
    
    if(is.null(station.mean)) next
    
    # plots to screen
    
    par(mfrow = c(3, 1))
    
    plot(station.doy, station.mean , ylim=c(-3.,3.),
         xlab=NA, ylab="mean"); title(paste0(station,"--BBP"));
    
    plot(station.doy, station.sd , ylim=c(0,6), col='grey',
         xlab=NA, ylab="sd"); #title(station); 
    
    lines(station.doy, station.lowess.sd, col='red',lwd=2)
    plot(station.doy, station.nobs , ylim=c(0,80) ,
         xlab='Day of Year', ylab="Nobs."); #title(station);
    #
    
    figNum <<- figNum + 1
    title <- paste0( "BBP mean, standard deviation, and number of observations ",
                     "as a Function of Day of Year.")
    .F(title, figNum)
  }

  
  return(salinity.detrended )
  
} ##FUNCTIOIN.END
