#' Aggregate data layers
#'
#' This function aggregates data layers. Steps: 1) Perform first level error checking
#' to make sure that the data set contains 'layer' and valid aggregation option was
#' selected. 2) Perform second level error checking to make sure the aggregation option
#' selection makes sense (e.g. cannot aggregate "S" and "AP" if no "AP" data are in the
#' data set). 3) Average the data by taking the median or mean based on user input.
#'
#' @param df data frame
#' @param avgTechnique method for aggregating data ("median"[default]/mean)
#' @param layerAggOption (0[default]: no aggregation; 1: combine "S" & "AP"
#'   ("SAP"); 2: combine "B" & "BP" ("BBP"); 3: opt 1 & 2 ("SAP", "BBP"); 4:
#'   combine all ("ALL")); 5: combine "S" and "B" ("SB")
#' @examples
#' # retrieve all corrected chlorophyll-a concentrations for Station CB2.2.
#' # A warning is displayed indicating that data set has layers but user did
#' # not specify layer in retrieval. layerAggregation then aggregates across
#' # all layers.
#' #df  <- dataMB
#' #df1 <- selectData(df, "chla", "CB2.2")
#' #df2 <- layerAggregation(df1[[1]], avgTechnique="median", layerAggOption=4)
#' #str(df2)
#' @return data frame with aggregated data
#'   
.layerAggregation <- function(df, avgTechnique="median", layerAggOption=0) {

# -----< Change history >--------------------------------------------
# 29Oct2016: JBH: migrated to helper function
# 10Oct2016: JBH: updated to allow for aggregation of censored data  
# 16Jun2016: JBH: added layerAggOption = 5; revised help file

# 1) Perform first level error checking ####

  # determine what column layer is in and assign that column number to iCol
  iCol <- grep("layer" , colnames(df))

  # error trap ... stop if the variable layer is not found in df
  if (length(iCol) == 0) {
    warning("No variable layer in data frame -- no aggregation performed.")
    attr(df,"layerAggReturn") <- "No aggregation performed (layer not in data set)."
    return(df)
  }

  # determine layers and records in data set before aggregation
  layerList <- unique(df$layer)
  layerAggRecords <- nrow(df)

  # check to make sure the user picked either mean or median. If neither, then over ride and
  # take the median. Export a warning to the user.
  if (avgTechnique != "median" & avgTechnique != "mean") {
    warning("Warning: Neither the median or mean were specified. Data will be averaged by median.")
    attr(df,"layerAggAvgTechnique") <- paste0("User selected ",avgTechnique, " but set to median.")
    avgTechnique <- "median"
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
  # more than one layer of data (layerlist>1) in the df. If ok layer set to ALL
  if(layerAggOption == 4 & length(layerList) > 1) {
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
  # At this point, we've got some data to aggregate. Unfortunately, I cannot find a function
  # to aggregate qw variables. So had to manually convert to long, average, then convert back to
  # wide.

  ### safe keeping df attributes
  df0 <- df[1 , ]

# convert data to long format ####
  tmpVarList <- c(attr(df,"depVar"), attr(df,"othVar"))
  for (tmpVar in tmpVarList) {
    conc <- as.data.frame(df[,tmpVar], expand = TRUE)[c(1,2,3,5,8,9)]
    names(conc) <- c("lower", "upper", "qualifier", "repLevel", "method", "lab")
    conc$parameter <- tmpVar
    conc <- cbind (df[, attr(df,"idVar")] , conc)
    if(!(exists("df1"))) {
      df1 <- conc
    } else {
      df1 <- rbind (df1 , conc)
    }
  }
  df<-df1[!(is.na(df1$lower)) | !(is.na(df1$upper)), ];   remove(df1, conc)

# average across layers ####
  # set up pseudo-PK
  df$key   <- with(df, paste(station, date, layer, parameter ,sep="/"))
  df$count <- as.numeric(with(df, ave(key, key, FUN = length)))

  # df11: 1 obs; df12: >=2 obs
  df11 <- df[ (df$count==1) , ]
  df12 <- df[ (df$count>=2) , ]

  # df11 -- 1 obs./do nothing #

  # df12 -- >=2 obs/average results #
  if(nrow(df12) >= 1) {
    #df12       <- df12[with(df12, order(key)), ]
    df12$lower <- with(df12, ave(lower, key, FUN = avgTechnique))
    df12$upper <- with(df12, ave(upper, key, FUN = avgTechnique))

    # reduce data set
    df12 <- df12[!duplicated(df12$key), ]

    # logic completeness check
    if(nrow(df12) == sum((( df12$lower==0 & df12$upper>=0)),
                         ((!df12$lower==0 & df12$lower==df12$upper)),
                         ((!df12$lower==0 & df12$lower <df12$upper)))) {
      #warning("Good news: df12 logic accounted for")
    } else {
      warning("df12 logic is incomplete--check for non '<' & '>' qualifier codes")
      table(df12$qualifier, useNA='always')
      table(df12$method,    useNA='always')
    }

    # 'less-thans'
    df12[df12$lower==0 & df12$upper>=0, "qualifier"] <- "<"
    # lower=upper
    df12[!(df12$lower==0) & df12$lower==df12$upper, "qualifier"] <- NA
    # lower<upper
    df12[!(df12$lower==0) & df12$lower <df12$upper, "qualifier"] <- "i"
  }

  # concatenate df11 and df12
  df <- rbind(df11, df12); remove(df11, df12)

  # clear "i" qualifiers
  df[!is.na(df$qualifier) & df$qualifier=="i", "qualifier"  ]  <- NA

# make qw object and reshape back to wide ####
  df$conc <- as.qw(values           = df$lower,
                   value2           = df$upper,
                   remark.codes     = df$qualifier,
                   value.codes      = "",
                   reporting.level  = NA_real_,
                   reporting.method = "",
                   reporting.units  = "",
                   analyte.method   = "",
                   analyte.name     = "",
                   unique.code      = "")
  df$conc@rounding <- c(3,4)

  # reshape to wide
  kVar <- c("station", "date", "layer", "parameter", "conc")
  df <- df[, kVar]
  df <- reshape (df, v.names=c("conc"), idvar=c("station", "date", "layer"),
                 timevar=c("parameter"), drop=c(""), direction = "wide")

  # drop "conc" from "conc.tdn"
  names(df) <- sub("^(.+[.])([^.]+)$", "\\2", names(df))

# 4) prep attributes for df and return ####

  # clean reshape attributes and re-apply incoming attributes
  attr(df,'reshapeWide') <-NULL
  df <- .reAttDF(df,df0)

  # apply attributes from layer aggregation
  attr(df,"layerAggAvgTechnique")    <- avgTechnique
  attr(df,"layerAggAggOption")       <- layerAggOption
  attr(df,"layerAggLayersBefore")    <- layerList
  attr(df,"layerAggLayersRecordsBef")<- layerAggRecords
  attr(df,"layerAggLayersAfter")     <- unique(df$layer)
  attr(df,"layerAggLayersRecordsAft")<- nrow(df)

  return(df)

}
