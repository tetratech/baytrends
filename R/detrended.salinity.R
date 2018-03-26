#' @title Detrended Salinty Data
#'
#' @description Create salintiy data set from user data, detrend it, and plot it.
#' 
#' @details Input is a a user file with 4 columns (Station, Sample_Date, Layer, and salinity).
#' See structure of sal data in example below.
#
# 7/18/2017: compute average salinity to SAP and BBP for all sites/dates in baytrends::tidalStations
#           based on data provide through Rebecca Murphy via email: 6/28/2017
# 2018-03-21: Updated to generalize special conditions.
#'
#' @param df.sal data frame with salinty data.
#' @param stations Stations
#' @param yearStart start year
#' @param yearEnd end year
#' @param dvAvgWinSel average window selection
#' @param lowess.f lowess
#' @param minObs Minimum number of observations (default is 40).
#' @param sdOverRide standard deviation override (default is 10)
#' @param dir.out Output directory for data file and plots.  Defaults to working directory.
#' 
#' @return Creates a data file (salinity.RDA) and plots in the user specified directory.
#' Plots are also output to the screen.
#' 
#' @examples
#' # Show Example Dataset (sal)
#' str(sal)
#'
#' # Define Function Inputs
#' df.sal        <- sal
#' stations      <- c("CB3.3C", "CB5.1", "CB5.4", "LE1.2", "LE3.1"
#'                          , "LE3.6", "TF2.2", "WT2.1") 
#' yearStart     <- 1984
#' yearEnd       <- 2016
#' dvAvgWinSel   <- c(30)
#' lowess.f      <- 0.2
#' minObs        <- 40
#' sdOverRide    <- 10
#' dir.out       <- file.path(getwd(), "data2")
#'                  
#' # Run Function
#' detrended.salinity(df.sal, stations, yearStart, yearEnd
#'                   , dvAvgWinSel, lowess.f, minObs, sdOverRide
#'                   , dir.out
#'                   )               
#' @export
detrended.salinity <- function(df.sal, stations
                               , yearStart, yearEnd
                               , dvAvgWinSel=30, lowess.f=0.2
                               , minObs=40, sdOverRide=10
                               , dir.out=getwd()){##FUNCTION.START
  
  # Rename inputs ####
  # name inputs to variables used in function
  myDir <- dir.out
  
  # Initialize settings and load input salinity data ----
  {
    #  setwd("E:/Dropbox/CBP/CBP_test - salinity cdfs")
    #getwd()
    #myDir <- file.path(dir.out)
    # setwd(myDir)
    
    # * set up flow retrieval and seasonal adjustment parameters ----
    salinity.detrended <- list(analysisDate  = Sys.time(),
                               stations      = stations,
                               yearStart     = yearStart, 
                               yearEnd       = yearEnd,
                               dvAvgWinSel   = dvAvgWinSel,
                               lowess.f      = lowess.f,
                               minObs        = 40,
                               sdOverRide    = 10
    )
    
    # * load salinity data ----
   # load(file.path(myDir, 'salinity_1984to2016.rdata'))
  }
  
  # Create SAP and BBP average salinity ----
  {  
    # # * down select input data set to those stations in baytrends list ----
    # df <- df.sal[df.sal$Station %in% salinity.detrended[["stations"]]
    #           , c("Station", "Sample_Date", "Layer", "salinity")]
    # 
    # # ** clean up data structure ----
    # trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    # df$Station     <- trim(as.character(df$Station))
    # df$Layer       <- trim(as.character(df$Layer))
    # df$Sample_Date <- trim(as.character(df$Sample_Date))
    # df$Sample_Date <- as.POSIXct(strptime(df$Sample_Date, "%m/%d/%Y"))
    
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
    
    # ** rename and save salinity data set ----
    names(salinity) <- c("station", "date", "salinity", "layer")
    save('salinity', file=file.path(dir.out, 'salinity.rda'))
  } 
  
  # Station by station processing ----
  for (i.station in salinity.detrended[["stations"]]        ) {
    #  i.station <-  salinity.detrended[["stations"]][46]
    
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
    
    
    # ** set lowess.sd to NA where nobs <= salinity.detrended$sdOverRide, then use smwrBase::fillMissing to interpolate
    df.lowess.sd[df.nobs$SAP<=salinity.detrended$sdOverRide,"SAP"] <- NA
    df.lowess.sd$SAP <- fillMissing(df.lowess.sd$SAP, max.fill=9e9, span=1)
    
    df.lowess.sd[df.nobs$SAP<=salinity.detrended$sdOverRide,"BBP"] <- NA
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
  
  # Store results ----
  #save(salinity.detrended, file='1984_2016_seasonally_detrended_salinity_data.rda')
  save(salinity.detrended, file=file.path(myDir, paste(yearStart, yearEnd
                                  , "seasonally_detrended_salinity_data.rda", sep="_")))
  
  
  #
  #17Jul2017:  plot mean, sd (and lowess.sd), and nobs per doy 
  
  
  #rm(list=ls())
  #cat("\014")
  #dev.off()
  
  #myDir <- file.path(getwd(), "data")
  #setwd(myDir)
  
  # Create directory "plots"
  # myDir.create <- file.path(myDir, "plots")
  # ifelse(dir.exists(myDir.create)==FALSE
  #        , dir.create(myDir.create)
  #        , "Directory already exists")
  
  # print(getwd())
  # flush.console()
  
  #load(file.path(myDir, '1984_2016_seasonally_detrended_salinity_data.rda'))
  names(salinity.detrended)
  
  for (station in stations) {
    
    station.sum <- paste0 (station ,".sum" )
    
    station.mean      <- salinity.detrended[[station.sum]][["mean"]][["SAP"]]
    station.sd        <- salinity.detrended[[station.sum]][["sd"]][["SAP"]]
    station.doy       <- salinity.detrended[[station.sum]][["sd"]][["doy"]]
    station.lowess.sd <- salinity.detrended[[station.sum]][["lowess.sd"]][["SAP"]]
    station.nobs      <- salinity.detrended[[station.sum]][["nobs"]][["SAP"]]
    
    if(is.null(station.mean)) next
    
    graph01 <- paste0(station,'_summary.jpg')
    jpeg(filename=file.path(dir.out, graph01), height=8,width=6.5, units="in", res=300)
        par(mfrow = c(3, 1))
        
        plot(station.doy, station.mean , ylim=c(-3.,3.)); title(station)
        
        plot(station.doy, station.sd , ylim=c(0,6), col='grey'); title(station)
        lines(station.doy, station.lowess.sd, col='red',lwd=2)
        
        plot(station.doy, station.nobs , ylim=c(0,80)); title(station)
    dev.off()
    #
    # Rerun plots and print to screen
    plot(station.doy, station.mean , ylim=c(-3.,3.)); title(station)
    
    plot(station.doy, station.sd , ylim=c(0,6), col='grey'); title(station)
    lines(station.doy, station.lowess.sd, col='red',lwd=2)
    
    plot(station.doy, station.nobs , ylim=c(0,80)); title(station)
    #
    fig.num <- match(station, stations)
    cat(paste("\n\n","Figure.",fig.num,". ", station,"\n\n"))
    flush.console()
  }
  
}##FUNCTIOIN.END
