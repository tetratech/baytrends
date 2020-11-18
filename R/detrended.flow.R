#' @title Create Seasonally Detrended Flow Data Set
#'   
#' @description This function creates a seasonally detrended flow data set for
#'   selected USGS gages. The created data set is used to support application of
#'   GAMs that include a hydrologic term as one of the independent variables.
#'   The output from this function should be stored as an .rda file for repeated
#'   use with baytrends.
#'   
#' @param usgsGageID USGS GageIDs (e.g., "01491000")
#' @param siteName USGS SiteName (only used for plots)
#' @param yearStart start year (recommended as at least one year before
#'   corresponding water quality data set)
#' @param yearEnd end year
#' @param dvAvgWinSel Averaging window (days) for smoothing the residuals of the
#'   seasonally adjusted daily flow values 
#'   [default = c(1, 5, 10, 15, 20, 30, 40, 50, 60, 90, 120, 150, 180, 210)]
#' @param dvAvgWgtSel Averaging method ("uniform", "weighted", or "centered")
#'   for creating weights. If using "weighted" then use dvAvgSidesSel=1.  If
#'   using "centered" then use dvAvgSidesSel=2. [default = "uniform"]
#' @param dvAvgSidesSel If dvAvgSidesSel=1 only past values are used, if
#'   dvAvgSidesSel=2 then values are centered around lag 0. [default = 1]
#' @param lowess.f lowess smoother span applied to computed standard deviation
#'   (see Details). This gives the proportion of points which influence the
#'   smooth at each value. Larger values give more smoothness. [default = 0.2]
#' @param span maximum number of observations on each side of range of missing
#'   values to use in filling in data [default = 10]
#' @param max.fill maximum gap to fill in [default = 10]
#'   
#' @return Returns a list of seasonally detrended flow data. You should save the
#'   resulting list as flow.detrended for use with baytrends.  This function
#'   also creates diagnostic plots that can be saved to a report when this
#'   function is called from an .Rmd script.
#'   
#' @details This function returns a list of seasonally detrended flow and companion
#'   statistics; and relies on USGS' dataRetrieval package to retrieve daily
#'   flow data.   
#'   
#'   It is the user responsibility to save the resulting list as \bold{flow.detrended} 
#'   for integration with baytrends.  
#'   
#'   For the purposes of baytrends, it is expected that the user 
#'   would identify all USGS gages that are expected to be evaluated so that a 
#'   single data file is created. To best match up with water quality data, we 
#'   recommend retrieving flow data for one year prior to the first year of 
#'   water quality data. This allows for creating a time-averaged flow data set 
#'   and not loose the first few months of water quality data due to lack of 
#'   matching flow data. Data retrievals should also be made in light of the 
#'   time needed by the USGS to review and approve their flow records.
#'   
#'   After retrieval, the following computation steps are performed to create a
#'   data frame for each USGS gage (the data frame naming convention is
#'   \bold{qNNNNNNNN} where NNNNNNNN is the USGS gage ID):   
#'   
#'   1) The daily flow data are converted to cubic meters per second [cms] and
#'   stored as the variable \bold{q}.  
#'   
#'   2) The day of year (\bold{doy}) is added to the data set. We use a 366 day
#'   calendar regardless of leap year.      
#'   
#'   3) The Log (ln) flow is computed and stored as \bold{LogQ}.    
#'   
#'   4) A seasonal GAM, i.e., gamoutput <- gam(LogQ ~  s(doy, bs='cc')) is
#'   evaluated and the predicted values stored as \bold{qNNNNNNNN.gam}.  
#'   
#'   5) The GAM residuals, i.e., "residuals(gamoutput)" are extracted and stored
#'   as the variable, \bold{d1}.  
#'   
#'   6) Based on the specifications for dvAvgWinSel, dvAvgWgtSel, and
#'   dvAvgSidesSel, the values of \bold{d1} are time averaged and additional
#'   variables \bold{dxxx} are added to the data frame where xxx corresponds to
#'   list of averaging windows specified in dvAvgWinSel. These values of
#'   \bold{dxxx} are used in GAMs that include a hydrologic independent
#'   variable.
#'   
#'   After the above data frame is created, the following four (4) additional
#'   data frames are created for each USGS gage and combined into a list named
#'   \bold{qNNNNNNNN.sum}:   
#'   
#'   \bold{mean} -- For each doy (i.e., 366 days of year), the mean across all
#'   years for each value of d in the above data frame, qNNNNNNNN.   
#'   
#'   \bold{sd} -- For each doy (i.e., 366 days of year), the standard deviation 
#'   across all years for each value of d in the above data frame, qNNNNNNNN.  
#'   
#'   \bold{nobs} -- For each doy (i.e., 366 days of year), the number of observations 
#'   across all years for each value of d in the above data frame, qNNNNNNNN.  
#'   
#'   \bold{lowess.sd} -- Lowess smoothed standard deviations. (These values are
#'   used for computing confidence intervals in the flow averaged GAM.)  
#'   
#'   The process of creating the above data frame, \bold{qNNNNNNNN}, and list,
#'   \bold{qNNNNNNNN.sum}, is repeated for each USGS gage and combined together
#'   in a single list. The beginning of the list includes meta data documenting
#'   the retrieval parameters.
#'   
#'   This function can be used in conjunction with an RMD file to knit (create)
#'   a report (DOCX or HTML).
#' @importFrom utils modifyList 
#' @importFrom stats aggregate
#' @importFrom stats na.pass
#' @importFrom stats sd
#' @importFrom stats lowess
# @importFrom utils globalVariables
#' @examples
#' \dontrun{
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
#'                  
#' # Run Function
#' flow.detrended <- detrended.flow(usgsGageID, siteName, yearStart, yearEnd
#'                                 , dvAvgWinSel, dvAvgWgtSel, dvAvgSidesSel
#'                                , lowess.f)
#' }
#' @export
detrended.flow <- function(usgsGageID, siteName
                           , yearStart, yearEnd
                           , dvAvgWinSel = c(1, 5, 10, 15, 20, 30, 40, 50, 60, 90, 120, 150, 180, 210)
                           , dvAvgWgtSel = "uniform"
                           , dvAvgSidesSel = 1
                           , lowess.f = 0.2
                           , span = 10
                           , max.fill = 10) {##FUNCTION.START

  # Initialization #### 
  fill = TRUE
  
  # create gageList data frame
  gageList <- data.frame(usgsGageID=usgsGageID,
                         siteName=siteName,
                         stringsAsFactors = FALSE)
  
  # set up flow retrieval parameters as header components for output list
  flow.detrended <- list(retreiveDate  = Sys.time(),
                         gages         = gageList,
                         yearStart     = yearStart,
                         yearEnd       = yearEnd,
                         dvAvgWinSel   = dvAvgWinSel,
                         dvAvgWgtSel   = dvAvgWgtSel,
                         dvAvgSidesSel = dvAvgSidesSel,
                         lowess.f      = lowess.f,
                         span          = span,
                         max.fill      = max.fill,
                         fill          = fill)
  
  #  reduce number of output plots 
    selectPlots <- unique(flow.detrended$dvAvgWinSel[c(1,2,length(flow.detrended$dvAvgWinSel))])

  # set figure number
    #utils::globalVariables("figNum")
   # figNum <- NULL  # CHECK fix
    assign("figNum", NULL)
    figNum <<- 0 
  
  # Retrieve data and do analysis for each gage ####
  for (i.gage in 1:length(flow.detrended$gages$usgsGageID)) {
    
    .H2(paste0(flow.detrended$gages$usgsGageID[i.gage],"-",
               flow.detrended$gages$siteName[i.gage]))
    
    # set up variable names for raw and summary data
    var       <- paste0("q",flow.detrended$gages$usgsGageID[i.gage])
    var.sum   <- paste0("q",flow.detrended$gages$usgsGageID[i.gage],".sum") 
    
    # getUSGSflow relies on dataRetrieval::readNWISdv
    df.flow <- getUSGSflow(siteNumber = flow.detrended$gages$usgsGageID[i.gage], 
                           yearStart  = flow.detrended$yearStart, 
                           yearEnd    = flow.detrended$yearEnd, 
                           span       = span, 
                           max.fill   = max.fill,
                           fill       = fill)
    
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
    # df.nobs <- aggregate(. ~ doy, FUN = gdata::nobs, data = df, na.action=na.pass, na.rm=TRUE)
    df.nobs <- aggregate(. ~ doy, FUN = nobs, data = df, na.action=na.pass, na.rm=TRUE)
    
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
    } # end i.day loop        
    
    # put mean, sd, nobs, & lowess by doy into a list; 
    # set embedded df to correspond to GageID.sum
    # append list to overall list
    tmp.list        <- list(list(mean = df.mean, sd = df.sd, nobs = df.nobs, lowess.sd = df.lowess))
    names(tmp.list) <- var.sum
    flow.detrended  <- modifyList(flow.detrended, tmp.list)
    
  } # end i.gage loop  
  
  # Plot Flow Statistics ####
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
        plot(gage.doy, gage.mean , ylim=c(-0.4,0.4),
             xlab=NA, ylab="mean"); title(paste0(gage," (Avg. Per.: ",dvAvgWinSel,")"))
        
        plot(gage.doy, gage.sd , ylim=c(0,2), col='grey',
        xlab=NA, ylab="sd") #title(gage)
        lines(gage.doy, gage.lowess.sd, col='red',lwd=2)
        plot(gage.doy, gage.nobs , ylim=c(0,60),
        xlab='Day of Year', ylab="Nobs.") #title(gage)
        figNum <<- figNum + 1
        title <- paste0( dvAvgWinSel, ": Mean, standard deviation, and number of observations ",
                         " as a Function of Day of Year.")
        .F(title, figNum)
      }
    }
  }

    return(flow.detrended)  

  #
}##FUNCTIOIN.END
