# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Select data for analysis from a larger data frame
#'
#' Select data for analysis from a larger data frame based on dependent
#' variable, station, and layer. Removing records with missing
#' values, performing log-transformations, and adding a centering date are
#' performed based on settings.
#'
#' @param df data frame
#' @param dep dependent variable
#' @param stat station
#' @param layer layer (optional)
#' @param transform logical field to return log-transformed value (TRUE [default])
#' @param remMiss logical field to remove records where dependent
#'   variable, dep, is a missing value (TRUE [default])
#' @param analySpec analytical specifications
#'
#' @details
#'
#'  The returned data frame will include dyear and cyear. dyear is the decimal
#'  year computed using smwrBase::baseDay2decimal and smwrBase::baseDay. From
#'  this, the minimum and maximum 'dyear' are averaged. This averaged value,
#'  centerYear, is used to compute the centering date, cyear, using cyear =
#'  dyear - centerYear.
#'
#'  The variable identified by dep is copied to the variable name dep+".orig"
#'  (e.g., chla.orig) allowing the user to track the original concentrations. A
#'  new column, recensor, is added. The value of recensor is FALSE unless the
#'  value of dep.orig was <=0. In the cases where dep.orig is <= 0, recensor is
#'  set to TRUE and the value of dep is set to "less-than" a small positive
#'  value which is stored as iSpec$recensor. If transform=TRUE, the returned
#'  data frame will also include a variable "ln"+dep (i.e., "lnchla" for log
#'  transformed chla).
#'
#'  The data frame will include a column, intervention, which is a factor identifying
#'  different periods of record such as when different laboratory methods were
#'  used and is based on the data frame methodsList that is loaded into the
#'  global environment. This column is set to "A" with only 1 level if the data
#'  frame methodsList has not been loaded into the global environment.
#'
#'  The data frame will include a column, lowCensor, to indicate whether the
#'  data record occurs in a year with a low level of censoring over that
#'  particular year. The function gamTest uses this column to identify years of
#'  record (i.e., when lowCensor==FALSE) that should not be used in analyses.
#'
#'  If remMiss=TRUE, then the returned data frame will be down selected by
#'  removing records where the variable identified in 'dep' is missing;
#'  otherwise, no down selection is performed.
#'
#'  iSpec contains a large list of information
#'
#' dep - name of column where dependent variable is stored, could be "ln"+dep
#' for variables that will be analyzed after natural log transformation
#'
#' depOrig - name of original dependent variable, could be same as dep if no
#' transformation is used
#'
#' stat - name of station
#'
#' stationMethodGroup - name of station group that the station belongs to,
#' derived from station list (stationMasterList) and used to identify interventions
#' specified in methodsList table
#'
#' intervenNum - number of interventions found for this station and dependent
#' variable as derived from methodsList table, a value of 1 is assigned if no
#' methodsList entry is found
#'
#' intervenList - data frame of interventions identified by beginning and ending
#' date and labeled consecutively starting with "A"
#'
#' layer - layer
#'
#' layerName - layer name derived from layerLukup
#'
#' transform - TRUE/FALSE indicating whether log transformations were taken
#'
#' trendIncrease - an indicator for interpretation of an increasing concentration
#'
#' logConst - not currently used
#'
#' recensor - small value that observations <=0 are recensored to as "less than"
#' the small value
#'
#' censorFrac - data frame indicating the yearly number of observations and
#' fraction of observations reported as less than, uncensored, interval
#' censored, less than zero, and recensored; also includes a 'lowCensor' field
#' indicating which years will be dropped by gamTest due to high yearly
#' censoring
#'
#' yearRangeDropped - year range of data that will be dropped due to censoring
#'
#' censorFracSum - censoring overall summary
#'
#' centerYear - centering year
#'
#' parmName - parameter name
#'
#' parmNamelc - parameter name in lower case
#'
#' parmUnits - parameter units
#'
#' statLayer - station/layer label, e.g., "LE3.1 (S)"
#'
#' usgsGageID - USGS gage used for flow adjustments
#'
#' usgsGageName - USGS gage used for flow adjustments
#'
#' numObservations - number of observations
#'
#' dyearBegin - begin date in decimal form
#'
#' dyearEnd - end date in decimal form
#'
#' dyearLength - period of record length
#'
#' yearBegin - period of record begin year
#'
#' yearend - period of record end year
#'
#' dateBegin - begin date
#'
#' dateEnd - end date
#' 
#' The baseDay and baseDay2decimal functions have been added to this package 
#' from the smwrBase package.
#'
#' @return
#'
#'   A nest list is returned. The first element of the nest list is the down-selected
#'   data frame. The second element is the list, iSpec, contains specifications for
#'   data extraction. See examples for usage and details for further discussion of the data
#'   processing and components of each element.
#'
# @examples
# \dontrun{
# # retrieve Secchi depth for Station CB5.4, no transformations are applied
# dfr <- analysisOrganizeData(dataCensored)
# df        <- dfr[["df"]]
# analySpec <- dfr[["analySpec"]]
# dfr   <- selectData(dataCensored, 'secchi', 'CB5.4', 'S', transform=FALSE,
#                     remMiss=FALSE, analySpec=analySpec)
# dfr1  <- dfr[[1]]
# iSpec <- dfr[[2]]
# # retrieve surface corrected chlorophyll-a concentrations for Station CB5.4,
# # missing values are removed and transformation applied
# dfr   <- selectData(dataCensored, 'chla', 'CB5.4', 'S', analySpec=analySpec)
# dfr2  <- dfr[[1]]
# iSpec <- dfr[[2]]
# }
#' @export
# ~~~~~~~~~~~~~~~
selectData <- function(df, dep, stat, layer=NA, transform=TRUE,
                       remMiss=TRUE, analySpec) {

# -----< Change history >--------------------------------------------
# 02Jul2018: JBH: return NA if no data in lowCensor years  
# 02Jun2018: JBH: added iSpec$seasModels  to iSpec 
# 12Mar2018: JBH: only recensor data that will be logtransformed  
# 04Feb2018: JBH: count number of observations for each intervention
# 05Aug2017: JBH: corrected over-ride evaluation for setting iSpec$hydroTerm based on 
#                 inspecification of iSpec$flwParms and iSpec$salParms; cleaned
#                 change history dates
# 29Jul2017: JBH: expand iSpec to include more hydro terms  
# 22Jul2017: JBH: expand iSpec to return hydro terms from stationMasterList
# 20Jul2017: JBH: changed nomenclature from tidalStations to stationMasterList
# 22Mar2017: JBH: modified methodList to allow for a label
# 05Jan2016: JBH: reformatted line in example code
# 09Nov2016: JBH: updated documentation
# 03Nov2016: JBH: chk recensor vs. parameter lookup table
# 02Nov2016: JBH: added function to trim high censoring years; updated intervention analysis
#                 to only read TRUE methodsList items
# 29Oct2016: JBH: updated to all for migrated helper functions
# 20Oct2016: JBH: improved intervention look up to account for 24 hr clock
# 18Oct2016: JBH: added error trap for when all dependent variable data are NA
# 17Oct2016: JBH: updated for censored data
# 27Apr2016: JBH: depricated porBegin, porEnd, porLength
# 27Apr2016: JBH: Explicit use of "::" for non-base functions added.

# Error trap ####
  # make sure dep variable is in data frame
  if( !(dep %in% names(df)) ) {
    stop("Could not find dependent variable in data frame.")
  }

  # QC check fix, 20180503
  stationList <- analySpec$stationList
  depVarList     <- analySpec$depVarList
  layerList        <- analySpec$layerList
  
  # make sure layerList exists
  if(!exists("layerList")) stop("Layer look-up list not found. Operation stopped!")

  setTZ <- analySpec$setTZ

# Initialize iSpec ####
  # iSpec includes a list of variables that are transferred
  # back with downselected data set. See @return for description.
  iSpec              <- list()
  iSpec$dep          <- dep     # changed to "ln + dep" if transform == TRUE
  iSpec$depOrig      <- dep
  iSpec$stat         <- stat
  iSpec$stationMethodGroup <- stationList[stationList$stations==stat,"stationMethodGroup"]
  iSpec$hydroTerm <- tolower(stationList[stationList$stations==stat,"hydroTerm"]) #21Jul2017
  iSpec$flwAvgWin <- tolower(stationList[stationList$stations==stat,"flwAvgWin"]) #21Jul2017
  iSpec$flwParms  <- tolower(stationList[stationList$stations==stat,"flwParms"])  #21Jul2017
  iSpec$salParms  <- tolower(stationList[stationList$stations==stat,"salParms"])  #21Jul2017
  iSpec$hydroTermSel <- NA_character_                                                        #21Jul2017
  iSpec$hydroTermSel.var <- NA_character_                                                    #29Jul2017 
  iSpec$hydro.var.corr <- data.frame(NULL)                                                   #29Jul2017
  iSpec$intervenNum  <- NA_real_
  iSpec$intervenList <- data.frame(NULL)
  iSpec$layer        <- layer
  iSpec$layerName    <- NA
  iSpec$transform    <- transform
  iSpec$trendIncrease<- depVarList[depVarList$deps==dep, "trendIncrease"]
  iSpec$logConst     <- 0             # migrated to default value of 0 for censored data
  iSpec$recensor     <- NA_real_      # always computed
  iSpec$censorFrac   <- data.frame(NULL)
  iSpec$yearRangeDropped   <- NA_real_      # always computed
  iSpec$censorFracSum<- data.frame(NULL)
  iSpec$centerYear   <- NA_real_      # computed if center == TRUE
  iSpec$parmName     <- depVarList[depVarList$deps==dep, "parmName"]
  iSpec$parmNamelc   <- depVarList[depVarList$deps==dep, "parmNamelc"]
  iSpec$parmUnits    <- depVarList[depVarList$deps==dep, "parmUnits"]
  iSpec$statLayer    <- NA            # always computed
  iSpec$usgsGageID   <- stationList[stationList$stations==stat,"usgsGageID"]
  iSpec$usgsGageName <- stationList[stationList$stations==stat,"usgsGageName"]
  iSpec$numObservations <- NA_real_
  iSpec$dyearBegin   <- NA_real_      # always computed
  iSpec$dyearEnd     <- NA_real_      # always computed
  iSpec$dyearLength  <- NA_real_      # always computed
  iSpec$yearBegin    <- NA_real_      # always computed
  iSpec$yearEnd      <- NA_real_      # always computed
  iSpec$dateBegin    <- NA            # always computed
  iSpec$dateEnd      <- NA            # always computed
  iSpec$seasModels   <- analySpec$gamLegend[analySpec$gamLegend$season,c("descrip","legend")]
  iSpec$baytrends.ver <- getNamespaceVersion("baytrends")
  
# Set up flow/salinity modeling parameters #21Jul2017 ####
  # split strings into vectors
  iSpec$flwAvgWin <- suppressWarnings(as.numeric(unlist(strsplit(iSpec$flwAvgWin, " "))))
  iSpec$flwParms  <- unlist(strsplit(iSpec$flwParms, " "))
  iSpec$salParms  <- unlist(strsplit(iSpec$salParms, " "))

  # fill in iSpec$hydroTerm w/ 'flow' if not specified; then initialize iSpec$hydroTermSel
  iSpec$hydroTerm    <- if(is.na(iSpec$hydroTerm)) 'flow' else iSpec$hydroTerm
  iSpec$hydroTermSel <- iSpec$hydroTerm    

  # inspect iSpec$flwParms and iSpec$salParms to see if            #05Aug2017
  # they over-ride the default iSpec$hydroTerm specification 
  if (!is.na(iSpec$flwParms[1]) & iSpec$depOrig %in% iSpec$flwParms) iSpec$hydroTermSel <- 'flow'
  if (!is.na(iSpec$salParms[1]) & iSpec$depOrig %in% iSpec$salParms) iSpec$hydroTermSel <- 'salinity'

# Down select data based on layer and stations ####

  # Select data from df based on station, layer and dependent variable
  #
  # option 1: ok: layer specified (i.e., !=NA) and layer in df
  # option 2: ok: layer not spec. (i.e., =NA)  and layer not in df
  # option 3: warning: layer not spec. but layer in df (proceed as ok)
  # option 4: error: layer specified but not in df
  #
  if(!is.na(layer) & ("layer" %in% names(df))) {
    df <- df[ df$station==stat & df$layer==layer, ]
    iSpec$statLayer <- paste0 (stat," (",layer,")")
    iSpec$layerName<- layerList[layerList$layers==layer, "name"]
  } else if (is.na(layer) & !("layer" %in% names(df))) {
    df <- df[ df$station==stat , ]
    iSpec$statLayer <- paste0 (stat)
  } else if (is.na(layer) & ("layer" %in% names(df))) {
    warning("Layer not specified, but layer occurs in data set.")
    df <- df[ df$station==stat , ]
  } else {
    stop("Cannot find variable, 'layer', in the data set.")
  }

  # error trap ... stop if all data for dependent variable are NA
  if (sum(!is.na(df[, dep])) == 0)   return(NA)

  # Sort returned data by date if date is in data set
  if("date" %in% names(df)) {
    df<-df[ order(df$date), ]
  }

# Re-censor negative and non-less-than zero values to small positive value ####

  # put data into temporary data frame
  conc          <- as.data.frame(df[,dep], expand = TRUE)[c(1,2,3,5,8,9)]
  conc          <- cbind(df[,"date"], conc)
  names(conc)   <- c("date", "lower", "upper", "qualifier", "repLevel", "method", "lab")
  
  # compute 1/2 the minimum lower or upper bound greater than zero;
  recensor <- 0.5 * min(min(conc[!is.na(conc$lower) & conc$lower>0,'lower'], na.rm=TRUE),
                        min(conc[!is.na(conc$upper) & conc$upper>0,'upper'], na.rm=TRUE))
  
  # 03Nov compare recensor to parameter lookup table value & take minimum
  iSpec$recensor <- recensor <- min( depVarList[depVarList$deps==dep, "parmRecensor"],
                                     recensor,  na.rm=TRUE)
  
  if(transform) {  #12Mar2018 only recensor variable that will be log transformed
    
    # apply recensor to negative and non-LT zero values
    conc$treat <- FALSE
    conc[!is.na(conc$lower) & (conc$lower < 0 | conc$upper <= 0), "treat" ] <- TRUE
    conc[conc$treat, "lower"]     <- 0
    conc[conc$treat, "upper"]     <- recensor
    conc[conc$treat, "qualifier"] <- "<"
    
    # rename dep variable to dep.orig
    names(df)[names(df) == dep] <- paste0(dep,".orig")
    df[,'recensor']   <- conc$treat
    
    # store re-censored qw variable
    df[,dep] <- suppressWarnings (as.qw(values           = conc$lower,
                                        value2           = conc$upper,
                                        remark.codes     = conc$qualifier,
                                        value.codes      = "",
                                        reporting.level  = NA_real_,
                                        reporting.method = "",
                                        reporting.units  = "",
                                        analyte.method   = "",
                                        analyte.name     = "",
                                        unique.code      = "")  )
    df[,dep]@rounding <- c(3,4) 
    
  } else {     #12Mar2018 pass through non-log transformed data 
    conc$treat <- FALSE
    
    # rename dep variable to dep.orig
    names(df)[names(df) == dep] <- paste0(dep,".orig")
    df[,'recensor']   <- conc$treat
    
    # copy dep.orig to data dep 
    df[,dep] <- df[,paste0(dep,".orig")]
    
  } 

# Add log-transformed data if requested. ####
  if(transform) {

    # set up log-transformed variable name
    iSpec$dep <- lnvar <- paste0('ln',dep)

    conc$lower2 <- suppressWarnings(log(conc$lower))
    conc$upper2 <- suppressWarnings(log(conc$upper))

    # make ln-transformed qw variable
    df[,lnvar] <- suppressWarnings (as.qw(values           = conc$lower2,
                                          value2           = conc$upper2,
                                          remark.codes     = rep("",length(conc$lower2)),
                                          value.codes      = "",
                                          reporting.level  = NA_real_,
                                          reporting.method = "",
                                          reporting.units  = "",
                                          analyte.method   = "",
                                          analyte.name     = "",
                                          unique.code      = "")  )
    df[,lnvar]@rounding <- c(6,6)
  }

# Identify method/lab changes ####

  # create intervention list for specific station and parameter (02Nov2016)
  if(exists("methodsList")) {
    if(!('label' %in% names(methodsList))) methodsList$label <- as.character(methodsList$beginDate)
    intervenList <- methodsList[methodsList$intervention &
                                  methodsList$stationMethodGroup == iSpec$stationMethodGroup &
                                  methodsList$parameter        == iSpec$depOrig &
                                  methodsList$beginDate        >  min(df$date,na.rm=TRUE) &
                                  methodsList$beginDate        <  max(df$date,na.rm=TRUE) ,]
    intervenList <- intervenList[with(intervenList, order(beginDate)), c("beginDate","intervention","label")]
    intervenList <- rbind(data.frame(beginDate        = min(df$date,na.rm=TRUE),
                                     intervention     = TRUE,
                                     label            = NA,
                                     stringsAsFactors = TRUE),  intervenList)
    intervenList$intervention   <- LETTERS[1:nrow(intervenList)]
    intervenList$endDate        <- c(intervenList[-1,"beginDate"] - (24*3600), max(df$date,na.rm=TRUE))
    intervenList                <- intervenList[ , c("intervention", "beginDate", "endDate","label")]
    rownames(intervenList)      <- NULL
  } else {
    intervenList <- data.frame(intervention = "A",
                               beginDate    = min(df$date,na.rm=TRUE),
                               endDate      = max(df$date,na.rm=TRUE),
                               label        = NA,
                               stringsAsFactors = FALSE)
  }

  # extend beginning date to beginning of year/ ending date to end of year
  intervenList$beginDate[1] <-
    lubridate::ymd(paste0(lubridate::year(intervenList$beginDate[1]),"-01-01"),  tz=setTZ)
  intervenList$endDate[nrow(intervenList)] <-
    lubridate::ymd(paste0(lubridate::year(intervenList$endDate[nrow(intervenList)]),"-12-31"),  tz=setTZ)

  # apply intervention table back to data frame
  # extended end date to 11:59pm  (20Oct2016)
  tmp <- lapply(1:nrow(df), function(x)
    list(intervenList[ intervenList$beginDate <= df$date[x] &
                            intervenList$endDate +(24*3600)-1 >= df$date[x], c("intervention")]))
  tmp[sapply(tmp, is.null)] <- NA
  df$intervention <- sapply(1:nrow(df), function(x) unname(unlist(tmp[x])[1]))

  # calculate number of observations for each method and merge to intervention table  04Feb2018
  tmp <- as.data.frame(table(df$intervention))
  intervenList = merge(intervenList,tmp, by.x="intervention", by.y="Var1", all.x=TRUE)
  intervenList$Freq[is.na(intervenList$Freq)] <- 0
  
  # set intervention variables to factors and store to iSpec
  df$intervention           <- factor(df$intervention, levels = intervenList$intervention)
  intervenList$intervention <- factor(intervenList$intervention, levels = intervenList$intervention)
  iSpec$intervenList        <- intervenList
  iSpec$intervenNum         <- nrow(intervenList)

# Count up censoring levels by year ####

  #using dataframe conc from previous section
  conc      <- conc[!is.na(conc$lower),]
  conc$year <- year(conc$date)
  conc$N    <- 1
  conc$fracRecen <- conc$fracLT0 <- conc$fracInt <- conc$fracUnc <- conc$fracLT <- 0
  # lower < 0
  conc[conc$lower< 0, "fracLT0"] <- 1
  # 'less-thans' (29Oct2016: exclude recensored)
  conc[conc$lower==0 & conc$upper>=0 & !conc$treat, "fracLT"] <- 1
  # lower=upper
  conc[conc$lower>0 & conc$lower==conc$upper, "fracUnc"] <- 1
  # lower<upper
  conc[conc$lower>0 & conc$lower <conc$upper, "fracInt"] <- 1
  # recensored
  conc[conc$treat, "fracRecen"] <- 1

  censorFrac <- data.frame(
    year      = unique(conc$year),
    obs       = with(conc, tapply(N      , year, sum)),
    fracLT    = with(conc, tapply(fracLT , year, sum))   / with(conc, tapply(N, year, sum)),
    fracUnc   = with(conc, tapply(fracUnc, year, sum))   / with(conc, tapply(N, year, sum)),
    fracInt   = with(conc, tapply(fracInt, year, sum))   / with(conc, tapply(N, year, sum)),
    fracLT0   = with(conc, tapply(fracLT0, year, sum))   / with(conc, tapply(N, year, sum)),
    fracRecen = with(conc, tapply(fracRecen, year, sum)) / with(conc, tapply(N, year, sum)))

  rownames(censorFrac)      <- NULL

  # # calculate which years to trim off the beginning due to too much censoring (02Nov2016)
  censorFrac$censorGT0.5_frac2 <- cumsum(censorFrac$fracLT > analySpec$censorTrim[1]) /
    as.numeric(row.names.default(censorFrac))

  censorFrac$lowCensor <- TRUE
  conc$lowCensor <- TRUE
  df$lowCensor <- TRUE

  # Check for at least 2 years with more than censorTrim[1] fraction of censoring and
  # that the fraction of years with too much censoriong is more than censorTrim[2] (02Nov2016)
  if(sum(censorFrac$fracLT > analySpec$censorTrim[1]) > 1 &
     length(which(censorFrac$censorGT0.5_frac2 > analySpec$censorTrim[2])) > 0 ) {
    yearCensorIndex <- min(
      max(which(censorFrac$fracLT > analySpec$censorTrim[1])) ,
      max(which(censorFrac$censorGT0.5_frac2 > analySpec$censorTrim[2])) )
    iSpec$yearRangeDropped <- c(censorFrac$year[1], censorFrac$year[yearCensorIndex ])
    censorFrac[censorFrac$year >= iSpec$yearRangeDropped[1] &
                 censorFrac$year <= iSpec$yearRangeDropped[2] ,"lowCensor"] <- FALSE
    conc[conc$year >= iSpec$yearRangeDropped[1] & conc$year <= iSpec$yearRangeDropped[2] ,"lowCensor"] <- FALSE
    df[df$year >= iSpec$yearRangeDropped[1] & df$year <= iSpec$yearRangeDropped[2] ,"lowCensor"] <- FALSE
  } else {
    iSpec$yearRangeDropped <- NA
  }

  iSpec$censorFrac <- censorFrac
  conc <- conc[conc$lowCensor,]
  iSpec$numObservations <- sum(conc$N)
  iSpec$censorFracSum<- data.frame(fracLT     = sum(conc$fracLT)   / iSpec$numObservations,
                                   fracUnc    = sum(conc$fracUnc)  / iSpec$numObservations,
                                   fracInt    = sum(conc$fracInt)  / iSpec$numObservations,
                                   fracLT0    = sum(conc$fracLT0)  / iSpec$numObservations,
                                   fracRecen  = sum(conc$fracRecen)/ iSpec$numObservations  )

# Add centered date; includes calculating POR ####
    if( !("dyear" %in% names(df)) & !("date" %in% names(df))) {
      stop("Could not calculate centered date without dyear or date.")
    }
    if( !("dyear" %in% names(df)) & "date" %in% names(df)) {
      df$dyear <- year(df$date) + baseDay2decimal(baseDay(df$date, numeric=FALSE))
    }
    dtmp <- range(df$date, na.rm=FALSE)
    por  <- range(df$dyear, na.rm=FALSE)
    centerYear <- mean( por)
    df$cyear   <- df$dyear  - centerYear

# Remove missing values if requested ####
  if(remMiss) {
    df<- df[ !is.na(df[,dep]), ]
  }

# Return NA if no data in low censor year    #02Jul2018
    if (nrow(df[df$lowCensor, ]) == 0) return(NA)
    
# Re-set POR based on data set without NA in dependent variable
  dtmp <- range(df[df$lowCensor,"date"], na.rm=FALSE)
  por  <- range(df[df$lowCensor,"date"], na.rm=FALSE)

# Assign centerYear and POR to output with iSpec ####
  iSpec$centerYear  <- centerYear
  iSpec$dyearBegin  <- por[1]
  iSpec$dyearEnd    <- por[2]
  iSpec$dyearLength <- por[2] - por[1]
  iSpec$yearBegin   <- lubridate::year(por[1])
  iSpec$yearEnd     <- lubridate::year(por[2])
  iSpec$dateBegin   <- dtmp[1]
  iSpec$dateEnd     <- dtmp[2]

# return ####
  dfr <- list(df,iSpec)

  return(dfr)
}
