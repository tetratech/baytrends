# ####
#' Perform GAM analysis
#'
#' Perform GAM analysis. Relies on mgcv::gam to perform general additive model
#'
#' @param df data frame
#' @param dep dependent variable
#' @param stat station
#' @param layer layer (optional)
#' @param analySpec analytical specifications
#' @param gamTable gam table setting (set to FALSE to turn off table output)
#' @param gamPlot gam plot setting (set to FALSE to turn off plotting)
#' @param gamDiffModel GAM model(s) used for computing differences on sub-annual/multi-period basis
#'
#' @examples
#' # specify parameter and station to analyze
#' dep        <- 'secchi'
#' stat       <- 'CB5.4'
#' layer      <- 'S'
#'
#' #Using gamTest
#' dfr <- analysisOrganizeData (dataCensored)
#' df        <- dfr[[1]]
#' analySpec <- dfr[[2]]
#' gamResult <- gamTest(df, dep, stat, layer, analySpec=analySpec)
#'
#' @return Returns a list with results
#' @export
#' @import lubridate mgcv smwrQW
#' @importFrom graphics grid lines plot points abline axis.POSIXct
#' @importFrom graphics legend mtext par polygon title
#' @importFrom stats AIC anova as.formula coef coefficients
#' @importFrom stats model.frame pnorm predict pt qnorm qt reshape terms
#' @importFrom utils globalVariables
#'
# ####

gamTest <-function(df, dep, stat, layer=NA, analySpec, gamTable=TRUE, gamPlot=10, gamDiffModel=NA) {

# ----- Change history -------------------------------------------- ####
# 05Aug2017: JBH: add hydroTerms used for flow/salinity modeling; removed mn.doy;
#                 added error trap if flow.detrended or salinity.detrended are not loaded
# 04Aug2017: JBH: added flow terms to csv output tables
# 29Jul2017: JBH: inserted merging of flow and/or salinity
# 21Jul2017: JBH: removed gamK_CritSel extraction
# 19Jul2017: JBH: Modified Coefficient table to include comparison of interventions
#                 on an "A->B", "B->C", ... basis [see 19Jul2017 edit in .gamCoeff]
# 22Mar2017: JBH: modified to allow for more printed interventions
# 13Mar2017: JBH: added import and importFrom roxygen statements see above
# 04Feb2017: JBH: updated how 'select' term in gam function assigned
#                 added algorithm to compute number of knots in s(cyear) term
#                 added gamSelect and gamK to csv output
#                 change expectation maximization convergence threshold to argument passed by analySpec
#                 added F-stat evaluation and ANOVA table modification
# 30Nov2016: JBH: corrected yearRangeDropped assignment; update figRes assignment
# 08NOv2016: JBH: set gamPlot default to 10
# 29Oct2016: JBH: updates to allow for migrated to helper functions
# 24Oct2016: JBH: improved is.na check for no returned data to get rid of warning message
# 17Oct2016: JBH: added expectation maximization routines to allow for censored data; added
#                 porDiff to gamOutput list; expanded gamOutputs to allow for 0,...,6
# 08Jul2016: JBH: added select=TRUE to mgcv::GAM call
# 16Jun2016: JBH: minor code reformatting; updated code pick up/pass on returned list
#                 from .gamPlot; corrected code for how nltrnd.pv and seas.pv are assigned to
#                 stat.gam.res1; returned pdat with gamTest output; expanded to handle 4th
#                 gam model
# 09Jun2016: JBH: reduced number of variables associated with gamDiff returned to main program
# 05Jun2016: JBH: cleaned up if-else coding; added argument gamTable to function call
# 04Jun2016: JBH: Inserted argument gamPlot to function call; Added sub-annual/multi-period change
#                 analysis
# 03Jun2016: JBH: Transitioned GAM models to a list
# 18May2016: JBH: Added iSpec and data in list that is returned.
# 27Apr2016: JBH: Explicit use of "::" for non-base functions added.

# Initialization #####
  {
    # dont use scientific notation in figures
    options(scipen=5)

    #set up plot resolution
    if(gamPlot==TRUE) {
      figRes=1
    } else if(gamPlot %in% c(1:30)) {
      figRes=gamPlot
      gamPlot=TRUE
    } else if(gamPlot >30) {  #30Nov2016: set figRes to 30
      figRes=30
      gamPlot=TRUE
    } else {
      gamPlot<-FALSE
    }

    step.pt='none'

    #unpack
    depVarList  <- analySpec$depVarList
    stationList <- analySpec$stationList
    layerList   <- analySpec$layerList
    obsMin      <- analySpec$obsMin
    alpha       <- analySpec$gamAlpha
    seasons     <- analySpec$gamLegend[analySpec$gamLegend$season, "legend"]
    seasons     <- lubridate::mdy (paste0(seasons ,"/2000"))
    q.doy       <- as.numeric(smwrBase::baseDay(seasons))
    gamPenalty            <- analySpec$gamPenalty               #04Feb2017
    gamPenaltyCrit        <- analySpec$gamPenaltyCrit           #04Feb2017
    gamCoeffDeltaMaxCrit  <- analySpec$gamCoeffDeltaMaxCrit     #04Feb2017
    # gamK_CritSel          <- analySpec$gamK_CritSel             #04Feb2017 #21Jul2017

    #set transform to TRUE/FALSE based on what the dependent variable is
    transform <- depVarList[depVarList$deps==dep,"logTrans"]

    #get the data
    #  remMiss=TRUE
    dfr <-  selectData(df, dep, stat, layer, transform=transform, analySpec = analySpec)

    # return if no data (19,24Oct2016)
    if (is.na(dfr[1]))   return(NA)

    # unpack the returned data; update dep to "ln"+dep if log trans is desired.
    ct0 <- dfr[[1]]
    ct1 <- ct0[ct0$lowCensor,]
    iSpec <- dfr[[2]]
    dep   <- iSpec$dep
    yearBegin <- iSpec$yearBegin
    yearEnd   <- iSpec$yearEnd

    # error trap for minimum observations
    if ( !(dep %in% names(ct1)) )  {
      warning(paste0("Minimum obs. req. not met: ",dep, ", Station: ", stat, ", Layer: ", layer, " not evaluated."))
      .P(" ")
      return(NA)
    } else if (sum(!is.na(ct1[ct1$lowCensor,names(ct1)==dep])) < obsMin) {
      warning(paste0("Minimum obs. req. not met: ",dep, ", Station: ", stat, ", Layer: ", layer, " not evaluated."))
      .P(" ")
      return(NA)
    }

    # set mgcv:gam select option based on gamPenalty setting and          #04Feb2017
    # level of censoring (iSpec$censorFracSum$fracUnc)
    if(!is.na(gamPenalty) & gamPenalty %in% c(TRUE,FALSE)) {
      selectSetting <- gamPenalty
    } else {
      if (iSpec$censorFracSum$fracUnc <1) {
        selectSetting <- FALSE
      } else {
        selectSetting <- TRUE
      }
    }

    # # set mgcv:gam s(cyear) knots terms based on gamK_CritSel and         #04Feb2017  #22Jul2017
    # # length of record stored in iSpec
    # gamK = max(gamK_CritSel[1], ceiling(gamK_CritSel[2] * (iSpec$yearEnd - iSpec$yearBegin + 1)))

    # count number of models to evaluate.
    numModels <- nrow(t(sapply(analySpec[['gamModels']],c)))
  }

# GAM loop: BEGIN #####
  for (iRow in 1:numModels) {
    # iRow<-1

    # GAM loop: model initialization ####
    {
      gamModel.option = analySpec[["gamModels"]][[iRow]]$option
      gamModel.name   = analySpec[["gamModels"]][[iRow]]$name
      gamModel.model  = analySpec[["gamModels"]][[iRow]]$model
      gamModel.deriv  = analySpec[["gamModels"]][[iRow]]$deriv
      gamModel.gamK1  = analySpec[["gamModels"]][[iRow]]$gamK1 #22Jul2017
      gamModel.gamK2  = analySpec[["gamModels"]][[iRow]]$gamK2 #22Jul2017

      # set whether model includes intervention, flw_sal, gamK1 and/or gamK2 term  #22Jul2017
      intervention <- ifelse (length(grep('intervention',gamModel.model)) == 0, FALSE, TRUE)
      has.gamK1    <- ifelse (length(grep('gamK1',gamModel.model)) == 0, FALSE, TRUE)   #22Jul2017
      has.gamK2    <- ifelse (length(grep('gamK2',gamModel.model)) == 0, FALSE, TRUE)   #22Jul2017
      has.flw_sal  <- ifelse (length(grep('flw_sal',gamModel.model)) == 0, FALSE, TRUE) #22Jul2017

      # compute gamK1 if 'has.gamK1' is TRUE
      gamK1 = ifelse (has.gamK1, max(gamModel.gamK1[1],
                                     ceiling(gamModel.gamK1[2] * (iSpec$yearEnd - iSpec$yearBegin + 1))) , NA)

      # compute gamK2 if 'has.gamK2' is TRUE
      gamK2 = ifelse (has.gamK2, max(gamModel.gamK2[1],
                                     ceiling(gamModel.gamK2[2] * (iSpec$yearEnd - iSpec$yearBegin + 1))) , NA)

      # error trap for gamModel.option
      if (!gamModel.option %in% c(0:6) )  {
        stop("GAM model option not valid value from 0-6.")
        return(NA)
      }

      # Error trap for intervention term in model if there aren't at least two levels
      # of intervention in the data (21Oct2016)
      if (intervention & iSpec$intervenNum<2) next
    }

    # integrate flow and/or salinity data into data set (ct1)      ####
    # added 29Jul2017
    {
      if(has.flw_sal) {
        # process for flow
        if(iSpec$hydroTermSel=='flow' & !'flw_sal' %in% names(ct1)) {
          if(!exists("flow.detrended")) next  #05Aug2017
          # format gage ID and list of averaging windows
          gageID     <- paste0('q',iSpec$usgsGageID)
          hydro.var  <- paste0("d",iSpec$flwAvgWin)
          # evaluate correlation and merge best variable back with the data
          ct1.list   <- .mergeFlow(ct1=ct1, iSpec=iSpec, gageID=gageID, hydro.var=hydro.var)
        }
        # process for salinity
        if(iSpec$hydroTermSel=='salinity' & !'flw_sal' %in% names(ct1)) {
          if(!exists("salinity.detrended")) next  #05Aug2017
          # evaluate correlation and merge best variable back with the data
          ct1.list   <- .mergeSalinity(ct1=ct1, iSpec=iSpec)
        }
        # keep rows in ct1, where flw_sal is available
        ct1        <- ct1.list[["ct1"]]
        iSpec      <- ct1.list[["iSpec"]]
        ct1 <- ct1[ !is.na(ct1$flw_sal) , ]
      }
    }

    # ####no edits past here yet 22Jul2017 ########################


# GAM loop: initialize storage for gam results in 1 model ####
    {
      stat.gam.res1 <-  data.frame(
        station = stat,
        dep = depVarList[depVarList$depsGAM==dep, "deps"],
        layer = layer,
        latitude   = stationList[stationList$stations==stat, "latitude"],
        longitude  = stationList[stationList$stations==stat, "longitude"],
        cbSeg92    = stationList[stationList$stations==stat, "cbSeg92"],
        state      = stationList[stationList$stations==stat, "state"],
        stationGrpName = stationList[stationList$stations==stat, "stationGrpName"],
        parmName   = paste0(depVarList[depVarList$depsGAM==dep, "parmName"],
                            " [",depVarList[depVarList$depsGAM==dep, "parmUnits"], "]"),
        numObservations = iSpec$numObservations,
        yearRng    = paste0(yearBegin ,"-",  yearEnd),
        yearBegin  = yearBegin,
        yearEnd    = yearEnd,
        numYrs     = yearEnd - yearBegin + 1,
        yearRangeDropped = paste0(iSpec$yearRangeDropped[1] ,"-", iSpec$yearRangeDropped[2]), #30Nov2017
        fracLT           = iSpec$censorFracSum$fracLT,        #03Nov2017
        fracUnc          = iSpec$censorFracSum$fracUnc,       #03Nov2017
        fracInt          = iSpec$censorFracSum$fracInt,       #03Nov2017
        fracRecen        = iSpec$censorFracSum$fracRecen,     #03Nov2017
        recensor         = iSpec$recensor,                    #03Nov2017
        depGAM     = dep,
        logTrans   = depVarList[depVarList$depsGAM==dep,"logTrans"],
        gamOption  = gamModel.option,
        gamName    = gamModel.name,
        gamSelect  = selectSetting,   #04Feb2017
        gamK1       = gamK1,
        gamK2       = gamK2, stringsAsFactors = FALSE,             #04Feb2017 #22Jul2017
        hydroTermSel     = iSpec$hydroTermSel,                     #05Aug2017
        hydroTermSel.var = iSpec$hydroTermSel.var)                 #05Aug2017
    }

# GAM loop: Run GAM w/ Expectation Maximization for Censored Data #####

    # create gam formula
    gamForm  <- as.formula(paste(iSpec$dep, gamModel.model))
    iSpec$gamForm <- paste(iSpec$dep, gamModel.model)
    .H4(paste(depVarList[depVarList$depsGAM==dep, "parmName"], "-", gamModel.name))

    # impute plausible first guess. (impute in normal space, then convert)
    ct2 <- ct1
    ct2[,iSpec$depOrig] <- .impute(ct1[,iSpec$depOrig] )
    if(transform) ct2[,iSpec$dep] <- suppressWarnings(log(ct2[,iSpec$depOrig]))

    # run GAM on impute plausible first guess.
    #gamRslt     <- mgcv::gam(gamForm, knots=list(doy=c(1,366)),data=ct2, select=TRUE)         #04Feb2017
    gamRslt     <- mgcv::gam(gamForm, knots=list(doy=c(1,366)),data=ct2, select=selectSetting) #04Feb2017

    # extract statistics from first guess
    gamRsltSum  <- summary(gamRslt)
    gam1        <- gamRslt
    mu          <- predict(gam1)
    sigma       <- sqrt(gam1$sig2)
    gamCoeff1   <- coefficients(gam1)

    # skip expectation maximization and simply use 1st guess if all data are uncensored
    {
      if (!iSpec$censorFracSum$fracUnc==1) {

        # # set convergence criteria     #04Feb2017
        # gamCoeffDeltaMaxCrit <- 1e-6   #04Feb2017

        for (expConvIter in 1:50) {
          # get conditional expectation. Both .ExpLNmCens and .ExpNmCens use the 1st
          # and 2nd argument to point to the censored data in normal space so that's why
          # we use ct1 and iSpec$depOrig. mu and sigma are computed from mgcv::gam
          # so are either in log or normal space depending on type of variable. The returned
          # variable ect, is in normal space (note that ect$l == etc$u), so ect is either
          # log transformed [or not] and substituted into ct2[, iSpec$dep]
          if(transform) {
            ect <- .ExpLNmCens(ct1, iSpec$depOrig, mu, sigma)
            ct2[, iSpec$dep] <- log(ect$l)
          } else {
            ect <- .ExpNmCens (ct1, iSpec$depOrig, mu, sigma)
            ct2[, iSpec$dep] <- ect$l
          }

          # Run gam on refitted values
          #gamRslt0     <- mgcv::gam(gamForm, knots=list(doy=c(1,366)),data=ct2, select=TRUE)          #04Feb2017
          gamRslt0     <- mgcv::gam(gamForm, knots=list(doy=c(1,366)),data=ct2, select=selectSetting)  #04Feb2017

          # error trap. make sure gamRslt0$sp has results to use
          if ((Inf %in% gamRslt0$sp) | (NaN %in% gamRslt0$sp)) {
            warning(paste0(stat,"/",dep,"/",layer, ': expectation max conv warning'))
            break
          } else {
            gamRslt <- gamRslt0
          }

          # extract statistics
          gamRsltSum  <- summary(gamRslt)
          gam1        <- gamRslt
          mu          <- predict(gam1)
          sigma       <- sqrt(gam1$sig2)
          gamCoeff2   <- coefficients(gam1)

          # compute root-mean-squared-difference in coefficients between iteration,
          # but include an error trap to skip comparison on the iteration
          # if the number of coefficients changes in the evaluation
          if(length(gamCoeff1)==length(gamCoeff2)) gamCoeffDeltaRMSE <- sqrt(mean((gamCoeff1-gamCoeff2)^2))

          # store updated coefficients
          gamCoeff1        <- gamCoeff2

          # break out of loop if convergence criteria is met
          if(gamCoeffDeltaRMSE < gamCoeffDeltaMaxCrit) break

        } # end expected maximization convergence
      } # end if statement to skip expectation maximization
    }


# GAM loop: Compute POR difference and Create Plot #####

    # compute por difference (use full period of record and all seasons) #11Aug2017
    # base.yr.set=NA; test.yr.set=NA; doy.set=NA; alpha=alpha
    por.diff <- gamDiff(gamRslt, iSpec, analySpec, base.yr.set=NA, test.yr.set=NA, doy.set=NA, alpha=alpha)

    # Turn t.deriv to TRUE/FALSE based on which model is being evaluated
    t.deriv <- gamModel.deriv

    # if gamPlot == TRUE, output a plot
    if (gamPlot)   {
      #  pgam <- gamRslt;  tsdat<-ct1;  dayStep=figRes
      gamPlotList <- .gamPlotCalc(dep,ct1,gamRslt,iSpec, analySpec, t.deriv=t.deriv,alpha = alpha,
                              dayStep=figRes, step.pt=step.pt, q.doy=q.doy)
      pdat       <- gamPlotList[["pdat"]]
      sa.sig.inc <- gamPlotList[["sa.sig.inc"]]
      sa.sig.dec <- gamPlotList[["sa.sig.dec"]]
      # mn.doy     <- gamPlotList[["mn.doy"]]        #05Aug2017

      # compile temporary list of the gamOutput for this gam model loop (to pass as input to next line)
      gamOutput.tmp <- list(gamOption  = gamModel.option,
                        gamRslt=gamRslt, gamRsltSum=NA, gamANOVAtbl=NA,
                        gamDiagnostics=NA, gamCoefftbl=NA, perChange=NA,
                        porDiff.regular=por.diff[[1]], porDiff.adjusted=por.diff[[2]],
                        predictions = pdat )

      # compile temporary "gamResult" list placing above temp. list into the model 0 slot
      stat.gam.tmp <- list(stat.gam.result = NA,  chng.gam.result = NA, data= ct1, data.all=ct0,
                           iSpec = iSpec, gamOutput0 = gamOutput.tmp ,
                           gamOutput1 = NA , gamOutput2 = NA , gamOutput3 = NA ,
                           gamOutput4 = NA , gamOutput5 = NA , gamOutput6 = NA )

      # set flags for plots significant trends and seasonal components to TRUE/FALSE
      seasAvgSigPlotSel <- ifelse (t.deriv,  TRUE, FALSE)
      seasModelPlotSel  <- ifelse (regexpr('ti',iSpec$gamForm) > 0, TRUE, FALSE)
      diffTypeSel       <- ifelse (intervention, 'both', 'regular')

      # output gam figure
      # gamResult=stat.gam.tmp; analySpec=analySpec; fullModel=0; seasAvgModel=0; seasonalModel=0;
      # obserPlot=TRUE; interventionPlot=TRUE; seasAvgPlot=TRUE; seasAvgConfIntPlot=TRUE;diffType = 'both'
      # seasAvgSigPlot=seasAvgSigPlotSel; fullModelPlot=TRUE;
      # seasModelPlot=seasModelPlotSel; BaseCurrentMeanPlot=TRUE; adjustedPlot=TRUE
      gamPlotDisp(gamResult=stat.gam.tmp, analySpec=analySpec,
                  fullModel=0, seasAvgModel=0, seasonalModel=0, diffType = diffTypeSel,
                  obserPlot=TRUE, interventionPlot=TRUE,
                  seasAvgPlot=TRUE, seasAvgConfIntPlot=TRUE, seasAvgSigPlot=seasAvgSigPlotSel,
                  fullModelPlot=TRUE, seasModelPlot=seasModelPlotSel, BaseCurrentMeanPlot=TRUE,
                  adjustedPlot=FALSE)

    } else {
      pdat       <- NA
      sa.sig.inc <- NA
      sa.sig.dec <- NA
      # mn.doy     <- NA      #05Aug2017
    }

# GAM loop: Table output #####

    gamANOVAtbl <- .gamANOVA(gamRslt)
    gamCoefftbl <- .gamCoeff(gamRslt)
    aic1 <- AIC(gamRslt)
    gamDiagnosticstbl <- data.frame(AIC = round(aic1,2),
                                    RMSE = round(sqrt(gamRsltSum$scale),4),
                                    AdjRsquare = round(gamRsltSum$r.sq,4) )
    perChangetbl <- .gamDiffPORtbl(por.diff,iSpec)

    #identify min edf in ANOVA table
    edfMinIndex  <- which.min(gamANOVAtbl$df)
    edfMin       <- gamANOVAtbl$df[edfMinIndex]
    edfMinSource <- gamANOVAtbl$source[edfMinIndex]

    # evaluate F-stat in ANOVA table     #04Feb2017
    FstatFlag <- ""
    if(selectSetting==FALSE &
       (min(gamANOVAtbl$df) < gamPenaltyCrit[1] |
        max(gamANOVAtbl$F) > gamPenaltyCrit[2])) {
      gamANOVAtbl$Note <- '-'
      gamANOVAtbl[gamANOVAtbl$df < gamPenaltyCrit[1] |
                    gamANOVAtbl$F > gamPenaltyCrit[2],"Note"] <- "F-stat maybe unreliable"
      FstatFlag <- "***"
    }

    # Output GAM ANOVA table          #04Feb2017
    if(gamTable) {

      if(selectSetting==FALSE &
         (min(gamANOVAtbl$df) < gamPenaltyCrit[1] |
          max(gamANOVAtbl$F) > gamPenaltyCrit[2])) {
        .T("GAM Analysis of Variance.")
        print(knitr::kable(gamANOVAtbl[,],
                           col.names = c("Type","Source","edf","F-stat","p-value","Note"),
                           align=c("l","r","r","r","r","l")))
      } else {
        .T("GAM Analysis of Variance.")
        print(knitr::kable(gamANOVAtbl[,],
                           col.names = c("Type","Source","edf","F-stat","p-value"),
                           align=c("l","r","r","r","r")))
      }

    # Output GAM coefficient table  #19Jul2017
      .T("GAM Parameter Coefficients.")
      if(length(gamCoefftbl) ==5) {
      print(knitr::kable(gamCoefftbl,
                         col.names = c('Parameter','Estimate','Std. Err.','t value','p-value'),
                         align=c("l","r","r","r","r")  ))
      } else if (length(gamCoefftbl) ==9) {
        print(knitr::kable(gamCoefftbl[,1:7],
                           col.names = c('Parameter','Estimate','Std. Err.','t value','p-value',
                                         'Int Chg p-val','Int Chg est'),
                           align=c("l","r","r","r","r","r","r")  ))

      } else {
        print(' ... print fail ...')
      }

    # print Regression Diagnostics table
      .T("GAM Diagnostics.")
      print(knitr::kable(gamDiagnosticstbl,
                         col.names =c("AIC","RMSE","Adj. R-squared")))

    # print period of record percent change table
      .T(paste0("Estimates of Change from ", iSpec$yearBegin, "-",iSpec$yearEnd,"."))
      if(intervention) {
        print(knitr::kable(perChangetbl[1:7,], align=c("l","c","c"),
                           col.names =c("Calculation","Estimate","Adj. Estimate")))
      } else {
        print(knitr::kable(perChangetbl[1:7,1:2], align=c("l","c","c"),
                           col.names =c("Calculation","Estimate")))
      }

    }

# GAM loop: stat.gam.res1 gathering #####

    # extract parameter coefficients and corresponding p-values (03Nov)
    stat.gam.res1$cyear.coeff        <- if(length(gamRsltSum$p.coeff[names(gamRsltSum$p.coeff)=="cyear"])==0) NA else
      gamRsltSum$p.coeff[names(gamRsltSum$p.coeff)=="cyear"]
    stat.gam.res1$cyear.pv     <- if(length(gamRsltSum$p.pv[names(gamRsltSum$p.pv)=="cyear"])==0) NA else
      gamRsltSum$p.pv[names(gamRsltSum$p.pv)=="cyear"]

    # extract interaction terms # with focus on"A->B, B->C, ..." types of changes 19Jul2017
    stat.gam.res1$interB.label <- if(!("B" %in% iSpec$intervenList$intervention)) NA else
      iSpec$intervenList[which(iSpec$intervenList$intervention=="B"),'label']
    stat.gam.res1$interB.chgEst        <- if("interventionB" %in% gamCoefftbl$source)
      gamCoefftbl[which(gamCoefftbl$source == "interventionB"),"intChg.est.actual"] else NA
    stat.gam.res1$interB.chgEst.pv     <- if("interventionB" %in% gamCoefftbl$source)
      gamCoefftbl[which(gamCoefftbl$source == "interventionB"),"intChg.p.value.actual"] else NA

    stat.gam.res1$interC.label <- if(!("C" %in% iSpec$intervenList$intervention)) NA else
      iSpec$intervenList[which(iSpec$intervenList$intervention=="C"),'label']
    stat.gam.res1$interC.chgEst        <- if("interventionC" %in% gamCoefftbl$source)
      gamCoefftbl[which(gamCoefftbl$source == "interventionC"),"intChg.est.actual"] else NA
    stat.gam.res1$interC.chgEst.pv     <- if("interventionC" %in% gamCoefftbl$source)
      gamCoefftbl[which(gamCoefftbl$source == "interventionC"),"intChg.p.value.actual"] else NA

    #22Mar2017 added more intervention output terms
    stat.gam.res1$interD.label <- if(!("D" %in% iSpec$intervenList$intervention)) NA else
      iSpec$intervenList[which(iSpec$intervenList$intervention=="D"),'label']
    stat.gam.res1$interD.chgEst        <- if("interventionD" %in% gamCoefftbl$source)
      gamCoefftbl[which(gamCoefftbl$source == "interventionD"),"intChg.est.actual"] else NA
    stat.gam.res1$interD.chgEst.pv     <- if("interventionD" %in% gamCoefftbl$source)
      gamCoefftbl[which(gamCoefftbl$source == "interventionD"),"intChg.p.value.actual"] else NA

    stat.gam.res1$interE.label <- if(!("E" %in% iSpec$intervenList$intervention)) NA else
      iSpec$intervenList[which(iSpec$intervenList$intervention=="E"),'label']
    stat.gam.res1$interE.chgEst        <- if("interventionE" %in% gamCoefftbl$source)
      gamCoefftbl[which(gamCoefftbl$source == "interventionE"),"intChg.est.actual"] else NA
    stat.gam.res1$interE.chgEst.pv     <- if("interventionE" %in% gamCoefftbl$source)
      gamCoefftbl[which(gamCoefftbl$source == "interventionE"),"intChg.p.value.actual"] else NA

    # extract p-values from ANOVA table (03Nov2016)
    stat.gam.res1$p.cyear.pv   <- if(length(which(rownames(gamRsltSum$pTerms.table)=="cyear"))==0) NA else
      gamRsltSum$pTerms.table[which(rownames(gamRsltSum$pTerms.table)=="cyear"),"p-value"]
    stat.gam.res1$p.inter.pv   <- if(length(which(rownames(gamRsltSum$pTerms.table)=="intervention"))==0) NA else
      gamRsltSum$pTerms.table[which(rownames(gamRsltSum$pTerms.table)=="intervention"),"p-value"]

    stat.gam.res1$s.cyear.pv   <- if(length(which(rownames(gamRsltSum$s.table)=="s(cyear)"))==0) NA else
      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="s(cyear)"),"p-value"]
    stat.gam.res1$s.doy.pv     <- if(length(which(rownames(gamRsltSum$s.table)=="s(doy)"))==0) NA else
      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="s(doy)"),"p-value"]
    stat.gam.res1$ti.cyear.doy.pv        <- if(length(which(rownames(gamRsltSum$s.table)=="ti(cyear,doy)"))==0) NA else
      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="ti(cyear,doy)"),"p-value"]

    # extract p values for flow terms #04Aug2017
    stat.gam.res1$s.flw_sal.pv           <- if(length(which(rownames(gamRsltSum$s.table)=="s(flw_sal)"))==0) NA else
      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="s(flw_sal)"),"p-value"]
    stat.gam.res1$ti.flw_sal.doy         <- if(length(which(rownames(gamRsltSum$s.table)=="ti(flw_sal,doy)"))==0) NA else

      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="ti(flw_sal,doy)"),"p-value"]
    stat.gam.res1$ti.flw_sal.cyear       <- if(length(which(rownames(gamRsltSum$s.table)=="ti(flw_sal,cyear)"))==0) NA else
      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="ti(flw_sal,cyear)"),"p-value"]
    stat.gam.res1$ti.flw_sal.doy.cyear   <- if(length(which(rownames(gamRsltSum$s.table)=="ti(flw_sal,doy,cyear)"))==0) NA else
      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="ti(flw_sal,doy,cyear)"),"p-value"]

    # extract p value for ti intervention terms
    stat.gam.res1$ti.interA.pv <- if(length(which(rownames(gamRsltSum$s.table)=="ti(cyear,doy):interventionA"))==0) NA else
      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="ti(cyear,doy):interventionA"),"p-value"]
    stat.gam.res1$ti.interB.pv <- if(length(which(rownames(gamRsltSum$s.table)=="ti(cyear,doy):interventionB"))==0) NA else
      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="ti(cyear,doy):interventionB"),"p-value"]
    stat.gam.res1$ti.interC.pv <- if(length(which(rownames(gamRsltSum$s.table)=="ti(cyear,doy):interventionC"))==0) NA else
      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="ti(cyear,doy):interventionC"),"p-value"]
    #22Mar2017 added more intervention output terms
    stat.gam.res1$ti.interD.pv <- if(length(which(rownames(gamRsltSum$s.table)=="ti(cyear,doy):interventionD"))==0) NA else
      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="ti(cyear,doy):interventionD"),"p-value"]
    stat.gam.res1$ti.interE.pv <- if(length(which(rownames(gamRsltSum$s.table)=="ti(cyear,doy):interventionE"))==0) NA else
      gamRsltSum$s.table[which(rownames(gamRsltSum$s.table)=="ti(cyear,doy):interventionE"),"p-value"]

    #
    stat.gam.res1$edfMin       <- edfMin
    stat.gam.res1$edfMinSource <- edfMinSource
    stat.gam.res1$FstatFlag    <- FstatFlag           #04Feb2017
    # stat.gam.res1$mn.doy       <- mn.doy            #05Aug2017
    stat.gam.res1$sa.sig.inc   <- sa.sig.inc
    stat.gam.res1$sa.sig.dec   <- sa.sig.dec

    #04Nov -- update
    stat.gam.res1$por.diffType    <- ifelse(intervention,"adjusted",'regular')
    por.diff.tmp                  <- if(intervention) {por.diff[[2]]} else {por.diff[[1]]}
    stat.gam.res1$por.bl.mn       <- por.diff.tmp$per.mn[1]
    stat.gam.res1$por.cr.mn       <- por.diff.tmp$per.mn[2]
    stat.gam.res1$por.bl.mn.obs   <- por.diff.tmp$per.mn.obs[1]
    stat.gam.res1$por.cr.mn.obs   <- por.diff.tmp$per.mn.obs[2]
    stat.gam.res1$por.abs.chg     <- por.diff.tmp$diff.est
    stat.gam.res1$por.abs.chg.obs <- por.diff.tmp$diff.est.obs
    stat.gam.res1$por.pct.chg     <- por.diff.tmp$pct.chg
    stat.gam.res1$por.chg.pv      <- por.diff.tmp$diff.pval

    # 03Nov left over from before
    stat.gam.res1$aic        <- gamDiagnosticstbl$AIC
    stat.gam.res1$rmse       <- gamDiagnosticstbl$RMSE
    stat.gam.res1$adjR2      <- gamDiagnosticstbl$AdjRsquare

    # gather results for ith gam model with other results
    if(!exists("stat.gam.result")) stat.gam.result <- stat.gam.res1 else
                                   stat.gam.result <- rbind(stat.gam.result,stat.gam.res1)

# GAM loop: gam model gathering #####
    gamOutput.tmp <- list(gamOption  = gamModel.option,
                          gamRslt=gamRslt, gamRsltSum=gamRsltSum, gamANOVAtbl=gamANOVAtbl,
                          gamDiagnostics=gamDiagnosticstbl, gamCoefftbl=gamCoefftbl,
                          perChange=perChangetbl, porDiff.regular=por.diff[[1]],
                          porDiff.adjusted=por.diff[[2]], predictions = pdat )

    if (gamModel.option==0) {
      gamOutput0 <- gamOutput.tmp
    } else if (gamModel.option==1) {
      gamOutput1 <- gamOutput.tmp
    } else if (gamModel.option==2) {
      gamOutput2 <- gamOutput.tmp
    } else if (gamModel.option==3) {
      gamOutput3 <- gamOutput.tmp
    } else if (gamModel.option==4) {
      gamOutput4 <- gamOutput.tmp
    } else if (gamModel.option==5) {
      gamOutput5 <- gamOutput.tmp
    } else if (gamModel.option==6) {
      gamOutput6 <- gamOutput.tmp
    }

# GAM loop: Sub-annual/multi-period change #####
    if(!is.na(gamDiffModel[1]) & (gamModel.option %in% gamDiffModel)) {

    # loop through all period and season combinations
      for (i1 in 1: nrow(t(sapply(analySpec[['gamDiffPeriods']],c)))) {
        for (i2 in 1: nrow(t(sapply(analySpec[['gamDiffSeasons']],c)))) {

          # i1=1
          # i2=1
          # extract base year(s), test years(s), and month(s) from the list
          base.yr.set <- analySpec[["gamDiffPeriods"]][[i1]]$periodStart
          test.yr.set <- analySpec[["gamDiffPeriods"]][[i1]]$periodEnd
          months  <- analySpec[["gamDiffSeasons"]][[i2]]$seasonMonths

          # set base and/or test years to NA if request is outside range of data (if NA is used
          # then gamDiff will default to using 1st 2 years and/or last 2 years of data)
          if(!is.na(base.yr.set[1])) base.yr.set <- base.yr.set[base.yr.set %in% c(yearBegin:yearEnd)]
          if(!is.na(test.yr.set[1])) test.yr.set <- test.yr.set[test.yr.set %in% c(yearBegin:yearEnd)]

          # do range check on months, if no months provided, assume Jan-Dec;
          # then compute doys for passing off to gamDiff
          if(!is.na(months[1]))      months <- months[months %in% c(1:12)]
          if(is.na(months[1]))       months <-  c(1:12)
          doy.set <- smwrBase::baseDay(as.POSIXct(paste(2000,months,15,sep='-')))

          # Calculate gamDiff  30Sep2017: added analySpec to function call
          sub.gamDiff <- gamDiff(gamRslt, iSpec, analySpec, base.yr.set=base.yr.set,
                                 test.yr.set=test.yr.set, doy.set=doy.set,
                                 alpha=alpha)

          #which sub.gamDiff to output
          sub.gamDiff.tmp                  <- if(intervention) {sub.gamDiff[[2]]} else {sub.gamDiff[[1]]}

          # package up results into df
          sub.gamDiff.df1 <- data.frame(
            periodName          = analySpec[["gamDiffPeriods"]][[i1]]$periodName,
            seasonName          = analySpec[["gamDiffSeasons"]][[i2]]$seasonName,
            periodStart         = paste(sub.gamDiff.tmp$base.yr,collapse=" "),
            periodEnd           = paste(sub.gamDiff.tmp$test.yr,collapse=" "),
            seasonMonths        = paste(months,collapse=" "),
            gamDiff.diffType    = ifelse(intervention,"adjusted",'regular'),
            gamDiff.bl.mn       = sub.gamDiff.tmp$per.mn[1] ,
            gamDiff.cr.mn       = sub.gamDiff.tmp$per.mn[2] ,
            gamDiff.bl.mn.obs   = sub.gamDiff.tmp$per.mn.obs[1] ,
            gamDiff.cr.mn.obs   = sub.gamDiff.tmp$per.mn.obs[2] ,
            gamDiff.abs.chg     = sub.gamDiff.tmp$diff.est   ,
            gamDiff.abs.chg.obs = sub.gamDiff.tmp$diff.est.obs   ,
            gamDiff.pct.chg     = sub.gamDiff.tmp$pct.chg ,
            gamDiff.chg.pval    = sub.gamDiff.tmp$diff.pval )

          if(i1==1 & i2==1) sub.gamDiff.df <- sub.gamDiff.df1 else
                            sub.gamDiff.df <- rbind(sub.gamDiff.df, sub.gamDiff.df1)

        }  # end gamDiffSeasons loop
      } # end gamDiffPeriods loop

      # GAM loop: Sub-annual/multi-period change: gather results
      #merge base gam results for left-half of table with right half calculated here
      chng.gam.result1 <- merge(stat.gam.res1,sub.gamDiff.df)

      # gather results for ith gam model with other results
      #chng.gam.result <-  rbind(chng.gam.result,  chng.gam.result1)

      if(!exists("chng.gam.result"))  chng.gam.result <- chng.gam.result1 else
                                      chng.gam.result <- rbind(chng.gam.result,chng.gam.result1)

    } # end sub-annual and multi-period trend analysis

# GAM loop: end #####
  } # end LOOP through each gam model

# Pack up list #####
  if(!exists("chng.gam.result")) chng.gam.result<-data.frame(NA)
  if(!exists("gamOutput0"))      gamOutput0<-data.frame(NA)
  if(!exists("gamOutput1"))      gamOutput1<-data.frame(NA)
  if(!exists("gamOutput2"))      gamOutput2<-data.frame(NA)
  if(!exists("gamOutput3"))      gamOutput3<-data.frame(NA)
  if(!exists("gamOutput4"))      gamOutput4<-data.frame(NA)
  if(!exists("gamOutput5"))      gamOutput5<-data.frame(NA)
  if(!exists("gamOutput6"))      gamOutput6<-data.frame(NA)

  # 21Oct2016 - added check for existence of stat.gam.result
  if(exists("stat.gam.result")) {
   stat.gam.result <- list(stat.gam.result = stat.gam.result,
                          chng.gam.result = chng.gam.result,
                          data            = ct1,
                          data.all        = ct0,
                          iSpec           = iSpec,
                          gamOutput0      = gamOutput0 ,
                          gamOutput1      = gamOutput1 ,
                          gamOutput2      = gamOutput2 ,
                          gamOutput3      = gamOutput3 ,
                          gamOutput4      = gamOutput4 ,
                          gamOutput5      = gamOutput5 ,
                          gamOutput6      = gamOutput6 )
  } else {
    stat.gam.result <- list(stat.gam.result = NA,
                            chng.gam.result = NA,
                            data            = NA,
                            data.all        = NA,
                            iSpec           = NA,
                            gamOutput0      = NA,
                            gamOutput1      = NA,
                            gamOutput2      = NA,
                            gamOutput3      = NA,
                            gamOutput4      = NA,
                            gamOutput5      = NA,
                            gamOutput6      = NA )
  }



# Return #####
  return(stat.gam.result)

}
