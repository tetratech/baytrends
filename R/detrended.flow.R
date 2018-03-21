#' @title Detrended Flow Data
#'
#' @description Create flow data set from USGS gageIDs, detrend it, create stats, and plot it.
#' 
#' @details Input is a list of USGS gages and other parameters to define the dataset.
#' This function can be used in conjunction with an RMD file to knit (create) a report (DOCX or HTML).
#'
#' @param usgsGageID USGS GageIDs (e.g., 01491000)
#' @param siteName USGS SiteName (not used for matching only for plot titles)
#' @param yearStart start year
#' @param yearEnd end year
#' @param dvAvgWinSel average window selection
#' @param dvAvgWgtSel average weight selection
#' @param dvAvgSidesSel average sides selection
#' @param lowess.f lowess
#' @param dir.out Output directory for data file, plots, and such
#' @param fn.out data file output name (will save as RDA). 
#' 
#' @return Creates an RDA file in the user specified directory 
#'                   and creates plots (but does not save them).
#' 
#' @examples
#'
#' # Define Function Inputs
#' usgsGageID    <- c("01491000", "01578310")
#' siteName      <- c("Choptank River near Greensboro, MD",
#'                    "Susquehanna River at Conowingo, MD")
#' yearStart     <- 1983
#' yearEnd       <- 2016
#' dvAvgWinSel   <- c(1, 5, 10, 15, 20, 30, 40, 50, 60, 90, 120, 150, 180, 210)
#' dvAvgWgtSel   <- "uniform"
#' dvAvgSidesSel <- 1
#' lowess.f      <- 0.2
#' dir.out       <- file.path(getwd(), "data")
#' fn.out        <- paste(yearStart, yearEnd
#'                        ,"seasonally_detrended_flow_data.rda", sep="_")
#'                  
#' # Run Function
#' detrended.flow(usgsGageID, siteName, yearStart, yearEnd
#'               , dvAvgWinSel, dvAvgWgtSel, dvAvgSidesSel
#'               , lowess.f, dir.out, fn.out)
#'               
#' @export
detrended.flow <- function(usgsGageID, siteName
                           , yearStart, yearEnd
                           , dvAvgWinSel, dvAvgWgtSel, dvAvgSidesSel
                           , lowess.f
                           , dir.out
                           , fn.out){##FUNCTION.START
  #
  #knitr::opts_chunk$set(dpi=150)
  #
  ## Load Station, Parameters, and Layer Lookup Lists ####
  #Set station list and other parameters.
  #gageList <- baytrends::usgsGages
  gageList <- data.frame(usgsGageID=usgsGageID,
                         siteName=siteName,
                         stringsAsFactors = FALSE)
  
  # set up flow retrieval and seasonal adjustment parameters
  flow.detrended <- list(retreiveDate  = Sys.time(),
                         gages         = gageList,
                         yearStart     = yearStart,
                         yearEnd       = yearEnd,
                         dvAvgWinSel   = dvAvgWinSel,
                         dvAvgWgtSel   = dvAvgWgtSel,
                         dvAvgSidesSel = dvAvgSidesSel,
                         lowess.f      = lowess.f)
  
  # outputFileName <- file.path(dir.out, "data2"
  #                             , paste0(flow.detrended$yearStart, "-"
  #                                      , flow.detrended$yearEnd
  #                                      , " seasonally detrended flow data.rda"))
  outputFileName <- file.path(dir.out, fn.out)
    selectPlots <- flow.detrended$dvAvgWinSel[c(1,2,length(flow.detrended$dvAvgWinSel))]
  #
  
  
  ## Retrieve Flow and Detrend ####
  #Get the data and detrend it.
  #An RDA file will be created in the user specifid folder.  
  #The file name will have the start and end dates as a prefix to "seasonally 
  #detrended flow data.rda".
  figNum <<- 0 
  
  # perform analysis for each gage
  for (i.gage in 1:length(flow.detrended$gages$usgsGageID)) {
    
    .H2(paste0(flow.detrended$gages$usgsGageID[i.gage],"-",
               flow.detrended$gages$siteName[i.gage]))
    # set up variable names for raw and summary data
    var       <- paste0("q",flow.detrended$gages$usgsGageID[i.gage])
    var.sum   <- paste0("q",flow.detrended$gages$usgsGageID[i.gage],".sum") 
    
    # download flow data (relies on get the dataRetrieval::readNWISdv) 
    # siteNumber = flow.detrended$gages$usgsGageID[i.gage];  yearStart  = flow.detrended$yearStart
    # yearEnd    = flow.detrended$yearEnd; span       = 10; max.fill   = 10; fill       = TRUE
    df.flow <- getUSGSflow(siteNumber = flow.detrended$gages$usgsGageID[i.gage], 
                           yearStart  = flow.detrended$yearStart, 
                           yearEnd    = flow.detrended$yearEnd, 
                           span       = 10, 
                           max.fill   = 10,
                           fill       = TRUE)
    
    # compute seasonally adjusted (i.e., detrended) flows and compute seasonal averages
    df.flow <- seasAdjflow(dvFlow     = df.flow,
                           siteNumber = flow.detrended$gages$usgsGageID[i.gage],
                           dvAvgWin   = flow.detrended$dvAvgWinSel,
                           dvAvgWgt   = flow.detrended$dvAvgWgtSel, 
                           dvAvgSides = flow.detrended$dvAvgSidesSel,
                           plotResid  = selectPlots)
    
    # put seasonal averages into a list
    # set embedded df to correspond to GageID
    # append list to overall list 
    tmp.list        <- list(df.flow[,c(1,5,9:length(df.flow))]) 
    names(tmp.list) <- var
    flow.detrended  <- modifyList(flow.detrended, tmp.list)
    
    # calculate mean, sd, & nobs by doy
    df      <- flow.detrended[[var]];   
    df      <- df[,!(names(df) %in% c("date", "q", "LogQ", paste0(var,".gam") ))]
    df.mean <- aggregate(. ~ doy, FUN = mean,        data = df, na.action=na.pass, na.rm=TRUE) 
    df.sd   <- aggregate(. ~ doy, FUN = sd,          data = df, na.action=na.pass, na.rm=TRUE) 
    df.nobs <- aggregate(. ~ doy, FUN = gdata::nobs, data = df, na.action=na.pass, na.rm=TRUE) 
    
    # compute lowess smooth standard deviation (sd)
    if(exists("df.lowess")) rm("df.lowess")
    df.sd.filled <- df.sd
    for (i.day in flow.detrended$dvAvgWinSel) {
      var2  <- paste0("d",i.day)
      # fill in missing sd
      df.sd.filled[, var2] <- fillMissing(df.sd.filled[, var2] )
      # smooth sd
      df2 <- as.data.frame(lowess(x=df.sd.filled[,"doy"], 
                                  y=df.sd.filled[, var2] , 
                                  f= flow.detrended$lowess.f))
      names(df2) <- c("doy",var2)
      # append i.day'th result
      if(!exists("df.lowess")) {
        df.lowess <- df2 
      } else {
        df.lowess <- merge(df.lowess,df2, by='doy')
      }
    }        
    
    # put mean, sd, nobs, & lowess by doy into a list; 
    # set embedded df to correspond to GageID.sum
    # append list to overall list
    tmp.list        <- list(list(mean = df.mean, sd = df.sd, nobs = df.nobs, lowess.sd = df.lowess))
    names(tmp.list) <- var.sum
    flow.detrended  <- modifyList(flow.detrended, tmp.list)
    
  }  
  
  save(flow.detrended, file=outputFileName)
  
  ## Flow Statistics ####
  # Flow statistics and output will be created from the new data file.
  for (i.gage in 1:length(flow.detrended$gages$usgsGageID)) {
    
    # set up variable names for raw and summary data
    gage       <- paste0("q",flow.detrended$gages$usgsGageID[i.gage])
    gage.sum   <- paste0("q",flow.detrended$gages$usgsGageID[i.gage],".sum") 
    
    for (dvAvgWinSel in paste0("d",selectPlots) ) {
      .H2(paste0(flow.detrended$gages$usgsGageID[i.gage],"-",
                 flow.detrended$gages$siteName[i.gage],
                 "  (Avg. Per.: ",dvAvgWinSel,")"))    
      gage.mean      <- flow.detrended[[gage.sum]][["mean"]][[dvAvgWinSel]]
      gage.sd        <- flow.detrended[[gage.sum]][["sd"]][[dvAvgWinSel]]
      gage.doy       <- flow.detrended[[gage.sum]][["sd"]][["doy"]]
      gage.lowess.sd <- flow.detrended[[gage.sum]][["lowess.sd"]][[dvAvgWinSel]]
      gage.nobs      <- flow.detrended[[gage.sum]][["nobs"]][[dvAvgWinSel]]
      
      par(mfrow = c(3, 1))
      {
        plot(gage.doy, gage.mean , ylim=c(-0.4,0.4)); title(gage)
        plot(gage.doy, gage.sd , ylim=c(0,2), col='grey'); title(gage)
        lines(gage.doy, gage.lowess.sd, col='red',lwd=2)
        plot(gage.doy, gage.nobs , ylim=c(0,60)); title(gage)
        figNum <<- figNum + 1
        title <- paste0( dvAvgWinSel, ": Mean, standard deviation, and number of observations ",
                         " as a Function of Day of Year.")
        .F(title, figNum)
      }
    }
  }
  

  #
}##FUNCTIOIN.END
