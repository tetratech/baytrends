# ####
#' Analysis Organization & Data Preparation
#'
#' This function assesses the user supplied specifications and prepares data for
#' analysis. In those cases where the user doesn't supply a needed
#' specification, a basic option is supplied by this function.
#'
#' @param df Data frame of water quality data
#' @param analySpec Specifications for analysis
#' @param reports Optional reports about parameters, layers and stations
#'   [default = c(0,1,2,3)]
#' @param parameterList User-supplied parameter list [default = NA]
#' @param stationMasterList User-supplied station list [default = NA]
#' @param layerLukup User-supplied layer lookup list [default = NA]
#'
#' @details
#'  The supplied data frame, df, is a data frame with the variables station,
#'  date, and layer included along with multiple additional columns for a
#'  variety of water quality variables structured as "qw" objects. An example
#'  data frame, dataCensored, is included with baytrends as an example.
#'  
#'  The argument, analySpec, is a list that includes basic specfications for
#'  performing GAM analyses. The components in analySpec are identified below.
#'  The user may create analySpec (which can include all or some of the below
#'  components; and pass the user-supplied analySpec to this function. Or, the
#'  user may accept the default argument and allow analysisOrganizeData to
#'  create and return analySpec. If the user passes a user-supplied analySpec,
#'  then analysisOrganizeData will "fill in" required arguments not provided by
#'  the user. The user can also adjust analySpec after it is returned from
#'  analysisOrganizeData although requirements for down selecting the data frame,
#'  df, or aggregating data by layer would need to be passed to analysisOrganizeData.    
#'  
#'  The default setting for the argument report is to provide tabular summary
#'  reports about the parameters, stations, and layers to be analyzed. Setting
#'  report=NA will suppress the tabular summary reports.
#'  
#'  The user can supply their own parameterList, stationMasterList, or
#'  layerLukup data sets; or the user can use the example data frames included
#'  with baytrends.
#' 
#'  The following steps are performed by analysisOrganizeData:
#'
#'  1) Review user supplied specifications through the analySpec argument. Fill
#'  in with default values. For example, if the user doesn't specify a list of
#'  stations, then all stations identified in the data set stationMasterList are
#'  used. Some other default values include the following: date range
#'  (1/1/1984-present), parameter list (all parameters in data set
#'  parameterList), layers (all layers in data set layerLukup), layer
#'  aggregation option (0, no aggregation), minimum number of observations to
#'  perform a GAM analysis (60), GAM formulas (Linear Trend with Seasonality,
#'  Non-linear Trend with Seasonality, and Non-linear trend with Seasonality
#'  (plus Interactions)), GAM alpha level for plots and output (0.05), periods
#'  for multi-time period analyses using GAM (Full Record, 1999/00-Present, and
#'  2005/06-Present), and seasons for sub-annual analyses using GAM (All,
#'  Spring1, Spring2, Summer1, Summer2, SAV1, SAV2, Winter, and Fall.)
#'
#'  2) Based on the settings in analySpec, the data frame df, is down selected
#'  based on parameters, stations, dates, and layers. A dependent variable list
#'  (depVarList) is created that includes variable descriptions, units, and log
#'  transformation selections. A station list (stationList) is created that
#'  includes the station ID, a selected USGS gage for correlating flow, and
#'  latitude/longitude.  
#'
#'  3) Aggregate data layers. If analySpec$layerAggOption is equal to 0, then
#'  there is no aggregation. The analySpec$layerAggOption of 1 would result in
#'  averaging (mean) surface and above pycnocline data. In this example, records
#'  with layer = "S" and layer = "AP" are relabeled as "SAP". Other
#'  layerAggOption values are 2) "B"&"BP"; 3) "S"&"AP" and "B"&"BP"; 4) all
#'  layers; and 5) "S"&"B", respectively. A layer list (layerList) is created
#'  and returned.
#'  
#'  4) Data are then averaged (mean) by date.  
#'
#'  5) Date features are added. Columns for year, day of year (doy), decimal
#'  year (dyear), and month are added based on date. Note that the doy is based on 
#'  a 366 day calendar regardless of leap year.
#'
#'  6) Reports on the number of records (0), parameters (1), layers (2) and
#'  stations (3) can be controlled with the reports option.
#'
#' @examples
#' # run analysis relying on default specifications, examine analySpec for
#' # default options
#' dfr <- analysisOrganizeData(dataCensored)
#' df        <- dfr[["df"]]
#' analySpec <- dfr[["analySpec"]]
#'
#' # analyze bottom dissolved oxygen at 2 stations
#' analySpec <-list()
#' analySpec$parameterFilt <- c('do')
#' analySpec$layerFilt     <- c('B')
#' analySpec$stationFilt   <- c('CB3.3C', 'CB5.4')
#' dfr <- analysisOrganizeData(dataCensored, analySpec)
#' df        <- dfr[["df"]]
#' analySpec <- dfr[["analySpec"]]
#'
#' @return Returns a list. Use dfr[["df"]] and dfr[["analySpec"]] to extract
#'   updated data frame and updated (or created) analySpec. analySpec is a list
#'   that includes the following components:
#'
#'   analyTitle       - Analysis trend title
#'
#'   parameterFilt    - Parameter filter used for data down selection
#'
#'   stationFilt      - Station filter used for data down selection
#'
#'   dateFilt         - Date filter for data down selection
#'
#'   setTZ            - time zone (default = "America/New_York")
#'
#'   layerFilt        - Layer filter
#'
#'   layerAggOption   - Layer averaging option (default = 0)
#'
#'   obsMin           - Minimum number of observations required to allow GAM
#'   analysis to proceed for a specific station, parameter, and layer
#'   combination (default = 60).
#'
#'   gamAlpha         - Alpha level used GAM analyses (default = 0.05).
#'
#'   censorTrim       - Values to apply for trimming data due to too much
#'   censoring (default = c(0.5, 0.4)).
#'
#'   gamModels        - model formulations
#'
#'   gamDiffPeriods   - list of time periods (years) used for computing changes
#'   (differences). The default options are: full record, 1999/00-present, and
#'   2005/06-present.  
#'
#'   gamDiffSeasons   - list of seasons used for sub-annual analyses. The default
#'   options include the following: All (months 1:12), Spring1 (3:5), Spring2
#'   (months: 4:6)), Summer1 (months: 6:9)), Summer2 (months: 7:9)), SAV1
#'   (months: 4:10)), SAV2 (months: 3:5,9:11)), Winter (months: 1:2)), and Fall
#'   (months: 10:12))).
#'
#'   gamPenalty       - allow the user to set the mgcv::gam select argument to
#'   TRUE, FALSE, or baytrend algorithm default (default = NA). When the default
#'   option is specified, then the mgcv::gam select argument is set to FALSE
#'   when none of the data are censored; otherwise (when some censoring exists
#'   in the data set) the select argument is set to TRUE
#'
#'   gamPenaltyCrit   - edf and F-stat values used to flag ANOVA table results
#'   (default = c(1, 9e9))
#'
#'   gamCoeffDeltaMaxCrit - convergence criteria for expectation maximization
#'   (default = 1e-6)
#'
#'   gamFlw_Sal.Wgt.Perc - percentiles of flow [or salinity] to use for
#'   computing flow [salinity] averaged result (default = c(0.05, 0.25, 0.50,
#'   0.75, 0.95))
#'
#'   gamLegend        - default settings for gam figure legend
#'
#'   idVar            - primary key for data frame returned as df
#'
#'   depVarList       - data frame of dependent variables (useful for setting up gam analyses in for loops)
#'
#'   stationList      - data frame of stations (useful for setting up gam analyses in for loops)
#'
#'   layerList        - data frame of layers (useful for setting up gam analyses in for loops)
#'
#' @export
# ####
analysisOrganizeData <- function(df, analySpec=list(), reports=c(0,1,2,3)
                                 , parameterList     = NA
                                 , stationMasterList = NA
                                 , layerLukup        = NA) { 

# df<-dataCensored; analySpec<-list(); parameterList<-stationMasterList<-layerLukup<-reports<-NA
# ----- Change history --------------------------------------------
# 01May2018: JBH: removed median as option for layer aggregation  
# 06Aug2017: JBH: added gamFlw_Sal.Wgt.Perc
# 05Aug2017: JBH: cleaned up change history date formats
# 21Jul2017: JBH: moved gamK_CritSel to gamModels; added gam4
# 20Jul2017: JBH: changed nomenclature from tidalStations to stationMasterList
# 14Mar2017: JBH: removed iTab, correct merge to use all.x=TRUE
# 05Feb2017: JBH: updated reports to allow for varied column organization
# 04Feb2017: JBH: added gamPenalty, gamPenaltyCrit, gamCoeffDeltaMaxCrit, gamK_CritSel
#                 changed s(cyear) to s(cyear, k=gamK)  for gam1, gam2, gam3
# 09Nov2016: JBH: reviewed documentation
# 04Nov2016: JBH: Update gam3 model to simplify the intervention term
# 01Nov2016: JBH: added gam model that includes intervention term; added default
#                 values for censored data trimming rule; added default time
#                 zone setting; added default plotting legend
# 29Oct2016: JBH: update for functions migrated to helper functions
# 14Oct2016: JBH: set argument analySpec to list() as default; expanded gam models
#             to allow options from 0-6
# 11Oct2016: JBH: removed seasonMonth downselect, updated documentation, migrated
#             to .functions for markdown
# 08Jul2016: JBH: removed gam3 from default settings gamModel
# 16Jun2016: JBH: updated list of gam models; migrated layer look up list to a
#             built in data frame; revised help file
# 15Jun2016: JBH: migrate layerLukup from hard-coded result to built-in dataframe
# 09Jun2016: JBH: updated help; suppressed record count reporting if
#            reports=NA
# 03Jun2016: JBH: Added list gamDiffPeriods, gamDiffSeasons. Replaced
#            gamModel with gamModels (gamModels is also a list).
# 27Apr2016: JBH: Explicit use of "::" for non-base functions added.
# 07Mar2016: JBH: Adjusted merging station filter with stationMasterList to keep
#            all all 'station filter' values even if not found in stationMasterList.
#            (Previously it would only keep those that are in stationMasterList.

# 0) Perform some useful housekeeping ####
# df<-dataCensored; analySpec<-list(); parameterList<-stationMasterList<-layerLukup<-reports<-NA
  
  # Store number of rows of data
  beginRecords <- nrow(df)
  
  # Use built-in data frames if not supplied by use, 20180503
  suppressWarnings(if (is.na(parameterList))     parameterList     <- baytrends::parameterList)
  suppressWarnings(if (is.na(stationMasterList)) stationMasterList <- baytrends::stationMasterList)
  suppressWarnings(if (is.na(layerLukup))        layerLukup        <- baytrends::layerLukup)

  # specify the average technique to be mean (median option was removed 01May2018)
  avgTechnique     <- "mean"
  
# 1) Review user supplied specifications.  ####

  # expand analySpec with some useful default values in cases where user doesn't specify something
  if (!"analyTitle"      %in% names(analySpec)) analySpec$analyTitle       <- paste0("CBP Trend Analysis: ", Sys.time())
  if (!"parameterFilt"   %in% names(analySpec)) analySpec$parameterFilt    <- parameterList$parm
  if (!"stationFilt"     %in% names(analySpec)) analySpec$stationFilt      <- stationMasterList$station
  if (!"dateFilt"        %in% names(analySpec)) analySpec$dateFilt         <- c( as.POSIXct('1984-01-01'),
                                                                                 as.POSIXct(Sys.time()))
  if (!"setTZ"           %in% names(analySpec)) analySpec$setTZ            <- "America/New_York"  #01Nov2016
  if (!"layerFilt"       %in% names(analySpec)) analySpec$layerFilt        <- layerLukup$layers
  if (!"layerAggOption"  %in% names(analySpec)) analySpec$layerAggOption   <- 0        # 0: no aggregation
  if (!"obsMin"          %in% names(analySpec)) analySpec$obsMin           <- 60       # need 60 obs
  if (!"gamAlpha"        %in% names(analySpec)) analySpec$gamAlpha         <- c(0.05)
  if (!"censorTrim"      %in% names(analySpec)) analySpec$censorTrim       <- c(0.5,0.40)  #01Nov2016

  if (!"gamModels"      %in% names(analySpec)) analySpec$gamModels       <- list(  #21Jul2017
    list(option=0, name= "Linear Trend with Seasonality",
         model= "~ cyear + s(doy,bs='cc')", 
         deriv=TRUE, gamK1=c(NA,NA), gamK2=c(NA,NA)),
    list(option=1, name= "Non-linear Trend with Seasonality",
         model= "~ cyear + s(cyear, k=gamK1) + s(doy,bs='cc')", 
         deriv=TRUE, gamK1=c(10,2/3), gamK2=c(NA,NA)),
    list(option=2, name= "Non-linear trend with Seas+Int",
         model= "~ cyear + s(cyear, k=gamK1) + s(doy,bs='cc')+ ti(cyear,doy,bs=c('tp','cc'))", 
         deriv=TRUE, gamK1=c(10,2/3), gamK2=c(NA,NA)),
    list(option=3, name= "Non-linear trend with Seas+Int. & Intervention",
         model= "~ intervention + cyear + s(cyear, k=gamK1) + s(doy,bs='cc') + ti(cyear,doy,bs=c('tp','cc'))", 
         deriv=TRUE, gamK1=c(10,2/3), gamK2=c(NA,NA)),  
    list(option=4, name= "Non-linear trend with Seas+Int. & Hydro Adj",
         model= paste0("~ cyear + s(cyear, k=gamK1) + s(doy,bs='cc') + ti(cyear,doy,bs=c('tp','cc')) + ",
                       "s(flw_sal,k=gamK2) + ti(flw_sal,doy,bs=c('tp','cc')) + ti(flw_sal, cyear,bs=c('tp' ,'tp')) + ",
                       "ti(flw_sal,doy,cyear, bs=c('tp','cc','tp'))"),
         deriv=TRUE, gamK1=c(10,1/3), gamK2=c(10,2/3)))

  # gam model with intervention and hydrologic adjustment -- could append to above list if desired in future version
  # list(option=5, name= "Non-linear trend with Seas+Int. & Inter/Hydro Adj",
  #      model= paste0("~ intervention + cyear + s(cyear, k=gamK1) + s(doy,bs='cc') + ti(cyear,doy,bs=c('tp','cc')) + ",
  #                    "s(flw_sal,k=gamK2) + ti(flw_sal,doy,bs=c('tp','cc')) + ti(flw_sal, cyear,bs=c('tp' ,'tp')) + ",
  #                    "ti(flw_sal,doy,cyear, bs=c('tp','cc','tp'))"),
  #      deriv=TRUE, gamK1=c(10,1/3), gamK2=c(10,2/3))

  if (!"gamDiffPeriods"  %in% names(analySpec)) analySpec$gamDiffPeriods   <- list(
    list( periodName = "Full Record",     periodStart = c(NA),        periodEnd = c(NA)),
    list( periodName = "1999/00-Present", periodStart = c(1999:2000), periodEnd = c(NA)),
    list( periodName = "2005/06-Present", periodStart = c(2005:2006), periodEnd = c(NA)))

  if (!"gamDiffSeasons"  %in% names(analySpec)) analySpec$gamDiffSeasons   <- list(
    list ( seasonName = "All",     seasonMonths = c(1:12)),
    list ( seasonName = "Spring1", seasonMonths = c(3:5)),
    list ( seasonName = "Spring2", seasonMonths = c(4:6)),
    list ( seasonName = "Summer1", seasonMonths = c(6:9)),
    list ( seasonName = "Summer2", seasonMonths = c(7:9)),
    list ( seasonName = "SAV1",    seasonMonths = c(4:10)),
    list ( seasonName = "SAV2",    seasonMonths = c(3:5,9:11)),
    list ( seasonName = "Winter",  seasonMonths = c(1:2)),
    list ( seasonName = "Fall",    seasonMonths = c(10:12)))

  if (!"gamPenalty"           %in% names(analySpec)) analySpec$gamPenalty           <- NA        #02Feb2017
  if (!"gamPenaltyCrit"       %in% names(analySpec)) analySpec$gamPenaltyCrit       <- c(1,9e9)  #02Feb2017
  if (!"gamCoeffDeltaMaxCrit" %in% names(analySpec)) analySpec$gamCoeffDeltaMaxCrit <- 1e-6      #02Feb2017
  if (!"gamFlw_Sal.Wgt.Perc"  %in% names(analySpec)) analySpec$gamFlw_Sal.Wgt.Perc  <- 
      c(0.05,0.25,0.5,0.75,0.95)      #06Aug2017

# 2) Down select primary data based on parameters, stations, dates, and layers ####
#    from updated specifications

  # Evaluate columns as idVar, depVar, or othVar
  df<-.chkParameter(df,analySpec$parameterFilt, parameterList)
  idVar  <- attr(df,"idVar")

  # Check to make sure all variables are of class 'qw'
  for (dep in c(attr(df,"depVar"),attr(df,"othVar"))) {
    jCol <- grep(paste0("^",dep,"$") , colnames(df))
    if(!(class(df[,jCol])=="qw")) stop(paste("Variable,",names(df[jCol]),"is not class qw!" ))
  }

  # .chkParameter also brought back dependent variables, merge with parameter list to
  # get proper name and transformation choice from built-in parameter list
  depVarList <- data.frame(deps=attr(df,"depVar"))
  depVarList$deps <- as.character(depVarList$deps)
  depVarList <- merge(depVarList, parameterList, by.x="deps", by.y="parm", all.x=TRUE)
  depVarList <- depVarList[ order(depVarList$parmRO1, depVarList$parmRO2) ,
                            names(depVarList) %in% c("deps", "parmName", "parmNamelc",
                                                     "parmUnits", "logTrans", "trendIncrease", "parmRecensor")]
  depVarList$depsGAM  <- as.character(depVarList$deps)
  depVarList[ depVarList$logTrans, "depsGAM"] <- paste0('ln',depVarList[ depVarList$logTrans, "deps"])

  # drop unwanted station data, get list of stations left, and merge with station list
  # to get USGS gage look up field and other meta data in built-in station list
  attr(df,"initialNumberRecords") <- beginRecords
  df <- .checkRange(df, var= "station",   varScrn = analySpec$stationFilt,   numNA = FALSE, deleteOption = "pass")
  stationList <- data.frame(stations = unique(df$station), stringsAsFactors = FALSE)
  stationList <- merge(stationList, stationMasterList, by.x="stations", by.y="station", all.x=TRUE)
  stationList <- stationList[ order(stationList$stationRO1, stationList$stationRO2) ,
                              !(names(stationList) %in% c(               "usgsGageMatch",
                                                          "stationRO1", "stationRO2"))  ]
  # drop data based on date
  df$date <- as.POSIXct(df$date)
  df <- .checkRange(df, var= "date",      varScrn = analySpec$dateFilt,      numNA = FALSE, deleteOption = "pass")

  # drop data based on layer
  df <- .checkRange(df, var= "layer",     varScrn = analySpec$layerFilt,     numNA = FALSE, deleteOption = "pass")

# 3) Aggregate data layers. #####

  if ("layer" %in% names(df)) {
    df<-.layerAggregation(df, avgTechnique=avgTechnique, layerAggOption=analySpec$layerAggOption)

    # create a "layer lookup table" that includes a proper layer name and has a built in preferred
    # order for which order to analyze the layers (mostly to get surface before bottom)
    layerList <- data.frame(layers = unique(df$layer), stringsAsFactors = FALSE)
    layerList <- merge(layerList,layerLukup,by="layers", all.x=TRUE)     #14Mar2017
    layerList <- layerList[ order(layerList$order) , names(layerList) %in% c("layers", "name")  ]

  } else {
    layerList <- NULL
  }

# 4) Date features are added. #####

  # add doy, month etc.
  df <- .appendDateFeatures(df)

# 5) Sort final df ####

  # sort the data by station, layer and date
  if("layer" %in% names(df)) {
    df <- df[order(df$station, df$layer, df$date),]
  } else {
    df <- df[order(df$station, df$date),]
  }

# 6) Final housekeeping ####

  # add gamPlot legend  #01Nov2016
  analySpec$gamLegend <- gamLegend
  
  # add newly created variables back to analySpec
  analySpec$idVar         <- idVar
  analySpec$depVarList    <- depVarList
  analySpec$stationList   <- stationList
  analySpec$layerList     <- layerList

  # pack up objects to return
  df <- list(df=df, analySpec=analySpec)

# 7) Reports on the number of records, parameters, layers and stations #####

  {
    # Record count report
    if(0 %in% reports) {
      .H3("Record Count")
      .P()
      .V(paste("Beginning Number of Records: ",  beginRecords ))
      .P()
      .V(paste("Number of Records After Processing: ",  nrow(df[[1]]) ))
      .P()
    }

    # Parameters report
    if(1 %in% reports) {
      .H3("Parameters")
      .T("List of Parameters.")
      print(knitr::kable(depVarList[ , c("deps", "parmName", "parmUnits",
                                         "logTrans", "depsGAM")],
                  format = "pandoc", padding = 0 ,  row.names=FALSE,
                  col.names= c("Dep. Var.", "Parameter Name", "Units",
                               "Log Tran.","GAM Dep. Var.")))
    }

    # Layer report
    if ("layer" %in% names(df[[1]]) & (2 %in% reports)) {
      .H3("Layers")
      .T("List of Layers.")
      # V("List of Layers.","s")
      print(knitr::kable(layerList,
                  format = "pandoc", padding = 0 ,  row.names=FALSE,
                  col.names=c("Layer ID","Layer Name")))
    }

    # Station report
    if(3 %in% reports) {
      .H2("")
      .H3("Stations")
      .T("List of Stations.")
      print(knitr::kable(stationList[   , c("stations", "latitude", "longitude",
                                            "cbSeg92", "usgsGageID", "stationMethodGroup") ],
                  format = "pandoc", digits=4, padding = 0, row.names=FALSE,
                  col.names=c("Station ID", "Latitude","Longitude",
                              "CB 92 Seg.", "Flow Adj. Gage", "Mth. Group")))
    }
  }

  return(df)

}
