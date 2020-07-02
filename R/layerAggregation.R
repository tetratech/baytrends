#' Aggregate data layers
#'
#' This function aggregates data layers. Steps: 1) Perform first level error checking
#' to make sure that the data set contains 'layer' and valid aggregation option was
#' selected. 2) Perform second level error checking to make sure the aggregation option
#' selection makes sense (e.g. cannot aggregate "S" and "AP" if no "AP" data are in the
#' data set). 3) Average the data by taking the median or mean based on user input.
#' 
#' @param df data frame
#' @param avgTechnique method for aggregating data ("mean" [default], "median")
#' @param layerAggOption (0[default]: no aggregation; 1: combine "S" & "AP"
#'   ("SAP"); 2: combine "B" & "BP" ("BBP"); 3: opt 1 & 2 ("SAP", "BBP"); 4:
#'   combine all ("ALL")); 5: combine "S" and "B" ("SB")
#'
#'
#' @examples
#' \dontrun{
#' # retrieve all corrected chlorophyll-a concentrations for Station CB2.2.
#' # A warning is displayed indicating that data set has layers but user did
#' # not specify layer in retrieval. layerAggregation then aggregates per 
#' # specifications.
#' df1 <- selectData(dataCensored, "chla", "CB2.2")
#' df2 <- layerAggregation(df1[[1]], avgTechnique="mean", layerAggOption=4)
#' str(df2)
#' }
#' 
#' @return data frame with aggregated data
#' @keywords internal
#' @export
#' @importFrom survival Surv
#' 
.layerAggregation <- function(df, avgTechnique="mean", layerAggOption=3) {
  
  # -----< Change history >--------------------------------------------
  # 02Jul2020: JBH: migrated to Surv objects, fixed median calculation  
  # 17May2018: JBH: removed "exist" stmt 
  # 29Oct2016: JBH: migrated to helper function
  # 10Oct2016: JBH: updated to allow for aggregation of censored data  
  # 16Jun2016: JBH: added layerAggOption = 5; revised help file
  
  # 1) Perform first level error checking ####
  
  # error trap ... stop if the variable layer is not found in df
  if (!"layer" %in% names(df)) {
    warning("No variable layer in data frame -- no aggregation performed.")
    attr(df,"layerAggReturn") <- "No aggregation performed (layer not in data set)."
    return(df)
  }
  
  # determine layers and records in data set before aggregation
  layerList <- unique(df$layer)
  layerAggRecords <- nrow(df)
  
  # check to make sure the user picked either mean or median. If neither, then over ride and
  # take the median. Export a warning to the user.
  if (!avgTechnique %in% c("median", "mean")) {
    warning("Warning: Neither the median or mean were specified. Data will be averaged by mean.")
    attr(df,"layerAggAvgTechnique") <- paste0("User selected ",avgTechnique, " but set to mean.")
    avgTechnique <- "mean"
  }
  
  # error trap ... stop if layerAggOption = 0 or null
  if (layerAggOption == 0 | is.null(layerAggOption) ) {
    attr(df,"layerAggReturn") <- "No aggregation performed (layerAggOption=0)."
    return(df)
  }
  
  # error trap ... stop if layerAggOption != 1, 2, 3, 4, or 5
  if (!(layerAggOption %in% c(1, 2, 3, 4, 5))) {
    warning("Aggregation option, layerAggOption, is not a valid choice -- no aggregation performed.)")
    attr(df,"layerAggReturn") <- "No aggregation performed (invalid layerAggOption selected)."
    return(df)
  }
  
  # error trap ... stop if there is only one layer since you cannot aggregate
  # layers if there is only 1 layer
  if ( length(layerList) == 1 ) {
    warning("Only one layer identified -- no aggregation performed.")
    attr(df,"layerAggReturn") <- "No aggregation performed (only one layer in data set)."
    return(df)
  }
  
  # 2) Perform second level error checking to make sure the aggregation option #####
  #    selection makes sense (e.g. cannot aggregate "S" and "AP" if no "AP" data
  #    are in the data set).
  #
  # 2nd level error traps; relabel layer to SAP, BBP or ALL as appropriate
  
  # set aggregate flag to FALSE, if value is set to TRUE in following code
  # then an aggregation function will be performed
  iAggregate <- FALSE
  
  # evaluate (S & AP) aggregation viability which is available in
  # layerAggOption 1 or 3. This requires there to be
  # both S and AP in the df. If found the layer field is set to SAP
  if ((layerAggOption == 1 | layerAggOption == 3)
      &  "S" %in% layerList
      & "AP" %in% layerList) {
    df$layer[df$layer=="S" | df$layer=="AP"] <- "SAP"
    iAggregate=TRUE
  } else if (layerAggOption == 1 | layerAggOption == 3) {
    warning("Either 'S' or 'AP' layer not detected -- 'S'&'AP' aggregation not performed.")
  }
  
  # evaluate (B & BP) aggregation viability which is available in
  # layerAggOption 2 or 3. This requires there to be
  # both B and BP in the df.  If found the layer field is set to BBP
  if ((layerAggOption == 2 | layerAggOption == 3)
      &  "B" %in% layerList
      & "BP" %in% layerList) {
    df$layer[df$layer=="B" | df$layer=="BP"] <- "BBP"
    iAggregate=TRUE
  } else if (layerAggOption == 2 | layerAggOption == 3) {
    warning("Either 'B' or 'BP' layer not detected -- 'B'&'BP' aggregation not performed.")
  }
  
  # evaluate option 4 ("ALL") viability. This option requires there to be
  # more than one layer of data (layerlist>=1) in the df. If ok layer set to ALL
  if(layerAggOption == 4 & length(layerList) >= 1) {
    df$layer <- "ALL"
    iAggregate=TRUE
  } else if (layerAggOption == 4) {
    warning("Need more than one layer to perform aggregation -- no aggregation performed.")
  }
  
  # evaluate (S & B) aggregation viability which is available in
  # layerAggOption 5. This requires there to be
  # both S and B in the df.  If found the layer field is set to SB
  if ((layerAggOption == 5)
      &  "S" %in% layerList
      &  "B" %in% layerList) {
    df$layer[df$layer=="S" | df$layer=="B"] <- "SB"
    iAggregate=TRUE
  } else if (layerAggOption == 5) {
    warning("Either 'S' or 'B' layer not detected -- 'S'&'B' aggregation not performed.")
  }
  
  # by this point it is still conceivable that the user requested an aggregation,
  # but the data dont support it, e.g., layerAggOption = 1, but only S and B layers
  # are in the data set. So we now test to see if aggregation is still needed
  
  if (iAggregate==FALSE) {
    warning("No valid aggregation found -- no aggregation performed.")
    attr(df,"layerAggReturn") <- "No aggregation performed (no valid aggregation found)."
    return(df)
  }
  
  
  # 3) Average the data by taking the median or mean based on user input. #####
  #
  # At this point, we've got some data to aggregate. Unfortunately, I have not found a suitable
  # path to process non-Surv and Surv objects at one time ... argh :( ...
  
  # 3a) initial settings ####
  
  # identify which variables are numeric, Surv or other stuff
  i.numeric <- sapply(df, is.numeric)
  i.Surv    <- sapply(df, survival::is.Surv)
  
  varNum   <- names(df)[i.numeric & !i.Surv]   # numeric, not Surv
  varSurv  <- names(df)[i.numeric & i.Surv]    # Surv
  varID    <- names(df)[!i.numeric & !i.Surv]  # id variables 
  
  if (length(names(df)) != length(unique(c(varNum,varSurv,varID)))) {
    stop("Could not determine which variables to average")
  }
  
  # safe keeping df attributes
  df.original <- df[1 , ]
  
  # create data frame with unique variables at the station|date|layer level
  df2     <- df[!duplicated(df[,c("station","date","layer")]), varID]
  df2$key <- with(df2, paste(station, date, layer ,sep="|"))
  
  # 3b) Process numeric fields ####
  if (length(varNum) > 0) {
    
    # create temporary df if there is numeric data
    df0 <-  df[ , c(varID, varNum)]
    
    # convert data to long format 
    for (tmpVar in varNum) {
      conc <- df0[,c(varID,tmpVar)]
      names(conc) <- c(varID, "value")
      conc$parameter <- tmpVar
      if(tmpVar == varNum[1]) { 
        df1 <- conc
      } else {
        df1 <- rbind (df1 , conc)
      }
    }
    
    # create a primary key and number of observations  
    df1$key   <- with(df1, paste(station, date, layer, parameter ,sep="|"))
    df1$count <- as.numeric(with(df1, ave(key, key, FUN = length)))
    
    # split data set for processing based on number of observations
    df11 <- df1[ (df1$count==1) , ]
    df12 <- df1[ (df1$count>=2) , ]
    
    # df12 -- >=2 obs/average results #
    if(nrow(df12) >= 1) {
      
      if (avgTechnique == "median") {
        df12$value <- with(df12, ave(value, key, FUN = function(x) median(x, na.rm = TRUE)))
      } else {
        df12$value <- with(df12, ave(value, key, FUN = function(x) mean(x, na.rm = TRUE)))
      }
      
      # reduce data set
      df12 <- df12[!duplicated(df12$key), ]
    }
    
    # combine df11 and df12
    df.num <- rbind(df11, df12)
    
    # reshape to wide
    kVar <- c("station", "date", "layer", "parameter", "value")
    df.num <- df.num[, kVar]
    df.num <- reshape (df.num, v.names=c("value"), idvar=c("station", "date", "layer"),
                       timevar=c("parameter"), drop=c(""), direction = "wide")
    
    # drop "conc" from "conc.tdn"
    names(df.num) <- sub("^(.+[.])([^.]+)$", "\\2", names(df.num))
    df.num$key    <- with(df.num, paste(station, date, layer ,sep="|"))
    
    # merge outcome into 'keeping' df2, data frame    
    df2 <- merge(df2
                 , df.num[, !names(df.num) %in% c("station", "date","layer")]
                 , by = "key", all.x = TRUE)
    
    
  }
  
  # 3c) Process Surv fields ####
  if (length(varSurv) > 0) {
    
    # create temporary df if there is numeric data
    df0 <-  df[ , c(varID, varSurv)]
    
    # convert data to long format 
    for (tmpVar in varSurv) {
      conc <- df0[,c(varID,tmpVar)]
      names(conc) <- c(varID, "value")
      conc$parameter <- tmpVar
      if(tmpVar == varSurv[1]) { 
        df1 <- conc
      } else {
        df1 <- rbind (df1 , conc)
      }
    }
    
    # create a primary key and number of observations  
    df1$key   <- with(df1, paste(station, date, layer, parameter ,sep="|"))
    df1$count <- as.numeric(with(df1, ave(key, key, FUN = length)))
    
    # split data set for processing based on number of observations
    df11 <- df1[ (df1$count==1) , ]
    df12 <- df1[ (df1$count>=2) , ]
    
    # df12 -- >=2 obs/average results #
    if(nrow(df12) >= 1) {
      
      # calculate statistic
      if (avgTechnique == 'median') {
        df12$value <- survival::Surv(
          with(df12, ave(unSurv(df12$value)[,1], key, FUN = function(x) median(x, na.rm = TRUE)))
          , with(df12, ave(unSurv(df12$value)[,2], key, FUN = function(x) median(x, na.rm = TRUE)))
          , type = "interval2") 
      } else {
        df12$value <- survival::Surv(
          with(df12, ave(unSurv(df12$value)[,1], key, FUN = function(x) mean(x, na.rm = TRUE)))
          , with(df12, ave(unSurv(df12$value)[,2], key, FUN = function(x) mean(x, na.rm = TRUE)))
          , type = "interval2") 
      }
      
      # reduce data set
      df12 <- df12[!duplicated(df12$key), ]
    }
    
    # combine df11 and df12
    df.Surv <- rbind(df11, df12)
    
    # reshape to wide
    kVar <- c("station", "date", "layer", "parameter", "value")
    df.Surv <- df.Surv[, kVar]
    df.Surv <- reshape (df.Surv, v.names=c("value"), idvar=c("station", "date", "layer"),
                        timevar=c("parameter"), drop=c(""), direction = "wide")
    
    # drop "conc" from "conc.tdn"
    names(df.Surv) <- sub("^(.+[.])([^.]+)$", "\\2", names(df.Surv))
    df.Surv$key    <- with(df.Surv, paste(station, date, layer ,sep="|"))
    
    df2 <- merge(df2
                 , df.Surv[, !names(df.Surv) %in% c("station", "date","layer")]
                 , by = "key", all.x = TRUE)
    
  }
  
  # 4) Final clean up ####
  # just keep the names that came over in the passed data frame, df
  df2 <- df2[ , names(df)]
  
  # clean reshape attributes and re-apply incoming attributes
  attr(df2,'reshapeWide') <-NULL
  df2 <- .reAttDF(df2,df0)
  
  # apply attributes from layer aggregation
  attr(df2,"layerAggAvgTechnique")    <- avgTechnique
  attr(df2,"layerAggAggOption")       <- layerAggOption
  attr(df2,"layerAggLayersBefore")    <- layerList
  attr(df2,"layerAggLayersRecordsBef")<- layerAggRecords
  attr(df2,"layerAggLayersAfter")     <- unique(df$layer)
  attr(df2,"layerAggLayersRecordsAft")<- nrow(df2)
  
  return(df2)
}
