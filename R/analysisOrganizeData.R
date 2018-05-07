# ####
#' Analysis Organization & Data Preparation
#'
#' This function assesses the user supplied specifications and prepares data for
#' analysis. In those cases where the user doesn't supply a needed
#' specification, a basic option is supplied by this function.
#'
#' @param df Data frame
#' @param analySpec Specifications for analysis
#' @param reports Optional reports about parameters, layers and stations
#'   [default = c(0,1,2,3)]
#'
#' @details
#'  The following steps are performed:
#'
#'  1) Review user supplied specifications. Fill in with default values. For
#'  example, if the user doesn't specify a list of stations, then all stations
#'  identified in the data set stationMasterList are used. Some other default values
#'  include the following: date range (1/1/1984-present), parameter list (all
#'  parameters in data set parameterList), layers (all layers in data set
#'  layerLukup), layer averaging technique ('mean'), layer aggregation option
#'  (0, no aggregation), minimum number of observations (60), GAM formulas
#'  (Linear Trend with Seasonality, Non-linear Trend with
#'  Seasonality, and Non-linear trend with Seasonality (plus Interactions)), GAM
#'  alpha level for plots and output (0.05), periods for multi-time period
#'  analyses using GAM (Full Record, 1999/00-Present, and 2005/06-Present), and
#'  seasons for sub-annual analyses using GAM (All, Spring1, Spring2, Summer1,
#'  Summer2, SAV1, SAV2, Winter, and Fall.)
#'
#'  2) Down select primary data set based on parameters, stations, dates, and layers
#'  from updated specifications. A dependent variable list (depVarList) is
#'  created that includes variable descriptions, units, and log transformation
#'  selections. A station list (stationList) is created that includes the
#'  station ID, a selected USGS gage for correlating flow, and
#'  latitude/longitude.
#'
#'  3) Aggregate data layers. analySpec$layerAggOption <- 0 results in no
#'  aggregation. The option analySpec$layerAggOption <- 1 would result in
#'  combining surface and above pycnocline data. In this example, records with
#'  layer = "S" and layer = "AP" are relabeled as "SAP". These records are then
#'  averaged to the date level. The averaging method (mean or median) is
#'  specified in by analySpec$avgTechnique. Other layerAggOption values are 2,
#'  3, 4, and 5 which average "B"&"BP"; "S"&"AP" and "B"&"BP"; all layers; and
#'  "S"&"B", respectively. A layer list (layerList) is created and returned.
#'
#'  4) Date features are added. Columns for year, day of year (doy), decimal
#'  year (dyear), and month are added based on date.
#'
#'  5) Reports on the number of records (0), parameters (1), layers (2) and
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
#' analySpec$stationFilt   <- c('CB3.3C', 'CB5.1')
#' dfr <- analysisOrganizeData(dataCensored, analySpec)
#' df        <- dfr[["df"]]
#' analySpec <- dfr[["analySpec"]]
#'
#' @return Returns a list. Use dfr[["df"]] and dfr[["analySpec"]] to extract
#'   updated data frame and updated (or created) analySpec. analySpec is a list
#'   that includes the following objects:
#'
#'   analyTitle       - Analysis trend title
#'
#'   parameterFilt    - Parameter filter used for down selection
#'
#'   stationFilt      - Station filter used for down selection
#'
#'   dateFilt         - Date filter for down selection
#'
#'   setTZ            - time zone
#'
#'   layerFilt        - Layer filter
#'
#'   layerAggOption   - Layer averaging option (see Details for more information)
#'
#'   obsMin           - Minimum number of observations required to allow GAM analysis to proceed
#'
#'   gamAlpha         - Alpha level used GAM analyses (where needed)
#'
#'   censorTrim       - Values to apply for trimming data due to too much censoring
#'
#'   gamModels        - model formulations
#'
#'   gamDiffPeriods   - time periods (years) used for computing changes (differences)
#'
#'   gamDiffSeasons   - seasons used for sub-annual analyses
#'
#'   gamPenalty       - allow the user to set the mgcv::gam select argument to TRUE, FALSE, or baytrend algorithm default
#'
#'   gamPenaltyCrit   - edf and F-stat values used to flag ANOVA table results
#'
#'   gamCoeffDeltaMaxCrit - convergence criteria for expectation maximization
#'
#'   gamFlw_Sal.Wgt.Perc - percentiles of flow [or salinity] to use for computing flow[salinity] averaged result
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
analysisOrganizeData <- function(df, analySpec=list(), reports=c(0,1,2,3)) { 

# df<-dataCensored; reports=NA
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

  # Store number of rows of data
  beginRecords <- nrow(df)
  
  # QC check fix, 20180503
  parameterList <- baytrends::parameterList
  stationMasterList <- baytrends::stationMasterList
  layerLukup <- baytrends::layerLukup

# 1) Review user supplied specifications.  ####

  # # create analySpec if user doesn't
  # if (!exists("analySpec"))  analySpec <-list()

  # expand analySpec with some useful default values in cases where user doesn't specify something
  if (!"analyTitle"      %in% names(analySpec)) analySpec$analyTitle       <- paste0("CBP Trend Analysis: ", Sys.time())
  if (!"parameterFilt"   %in% names(analySpec)) analySpec$parameterFilt    <- parameterList$parm
  if (!"stationFilt"     %in% names(analySpec)) analySpec$stationFilt      <- stationMasterList$station
  if (!"dateFilt"        %in% names(analySpec)) analySpec$dateFilt         <- c( as.POSIXct('1984-01-01'),
                                                                                 as.POSIXct(Sys.time()))
  if (!"setTZ"           %in% names(analySpec)) analySpec$setTZ            <- "America/New_York"  #01Nov2016

  if (!"layerFilt"       %in% names(analySpec)) analySpec$layerFilt        <- layerLukup$layers
#  if (!"avgTechnique"    %in% names(analySpec)) analySpec$avgTechnique     <- "mean"   #(median or mean)
  avgTechnique     <- "mean"   #(median or mean)
  
  if (!"layerAggOption"  %in% names(analySpec)) analySpec$layerAggOption   <- 0        # 0: no aggregation
  if (!"obsMin"          %in% names(analySpec)) analySpec$obsMin           <- 60

  if (!"gamAlpha"       %in% names(analySpec)) analySpec$gamAlpha        <- c(0.05)

  if (!"censorTrim"     %in% names(analySpec)) analySpec$censorTrim      <- c(0.5,0.40)  #01Nov2016

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
#  if (!"gamK_CritSel"         %in% names(analySpec)) analySpec$gamK_CritSel         <- c(10,2/3) #02Feb2017 #21Jul2017
  if (!"gamFlw_Sal.Wgt.Perc" %in% names(analySpec)) analySpec$gamFlw_Sal.Wgt.Perc <- 
      c(0.05,0.25,0.5,0.75,0.95)      #06Aug2017

# 2) Down select primary data based on parameters, stations, dates, and layers ####
#    from updated specifications

  # Evaluate columns as idVar, depVar, or othVar
  df<-.chkParameter(df,analySpec$parameterFilt)
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
                                                     "parmUnits", "logTrans", "trendIncrease")]
  depVarList$depsGAM  <- as.character(depVarList$deps)
  depVarList[ depVarList$logTrans, "depsGAM"] <- paste0('ln',depVarList[ depVarList$logTrans, "deps"])

  # drop unwanted station data, get list of stations left, and merge with station list
  # to get USGS gage look up field and other meta data in built-in station list
  attr(df,"initialNumberRecords") <- beginRecords
  df <- .checkRange(df, var= "station",   varScrn = analySpec$stationFilt,   numNA = FALSE, deleteOption = "pass")
  stationList <- data.frame(stations = unique(df$station))
  stationList <- merge(stationList, stationMasterList, by.x="stations", by.y="station", all.x=TRUE)
  stationList <- stationList[ order(stationList$stationRO1, stationList$stationRO2) ,
                              !(names(stationList) %in% c("usgsGageName","usgsGageMatch",
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
    layerList <- data.frame(layers = unique(df$layer))
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

  if(exists("reports") ) {

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
                                         "logTrans", "trendIncrease", "depsGAM")],
                  format = "pandoc", padding = 0 ,  row.names=FALSE,
                  col.names= c("Dep. Var.", "Parameter Name", "Units",
                               "Log Tran.","Inc. Conc.","GAM Dep. Var.")))
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
