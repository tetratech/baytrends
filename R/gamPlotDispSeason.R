# ####
#' Plot censored gam fits vs. time
#'
#' @param gamResult output from procedure gamTest
#' @param analySpec analytical specifications
#' @param fullModel GAM # for displaying full GAM (e.g., 0, 1, 2)
#' @param seasAvgModel GAM # for displaying seasonally average GAM
#' @param seasonalModel GAM # for displaying seasonal GAM
#' @param diffType plot predicted baseline mean ('regular') or adjusted baseline mean ('adjusted')
#' @param obserPlot logical field indicating whether to plot observations
#' @param interventionPlot logical field indicating whether to plot interventions (e.g., method changes)
#' @param seasAvgPlot logical field indicating whether to plot seasonal average GAM
#' @param seasAvgConfIntPlot logical field indicating whether to plot confidence interval for seasonal average GAM
#' @param seasAvgSigPlot logical field indicating whether to plot significant increasing and decreasing trends for seasonal average GAM
#' @param fullModelPlot logical field indicating whether to plot full GAM
#' @param seasModelPlot logical field indicating whether to plot seasonal GAM
#' @param BaseCurrentMeanPlot logical field indicating whether to plot baseline and current mean
#' @param adjustedPlot logical field indicating whether to plot adjusted model
#' @param gamSeasonFocus logical field indicating whether to plot focus on season mean 
#'
#' @examples
#' # Specify parameter and station to analyze
#' dep        <- 'do'
#' stat       <- 'CB5.4'
#' layer      <- 'B'
#'
#' # Prepare data and set up specifications for analysis
#' dfr <- analysisOrganizeData (dataCensored)
#' df        <- dfr[[1]]
#' analySpec <- dfr[[2]]
#' 
#' # Apply gamTest 
#' gamResult <- gamTest(df, dep, stat, layer, analySpec=analySpec)
#' gamPlotDisp(gamResult = gamResult, analySpec = analySpec,
#'             fullModel = 2, seasAvgModel = 2, seasonalModel = 2,
#'             diffType = "regular", obserPlot = TRUE, interventionPlot = TRUE,
#'             seasAvgPlot = TRUE, seasAvgConfIntPlot = FALSE,
#'             seasAvgSigPlot = FALSE, fullModelPlot = TRUE, seasModelPlot = TRUE,
#'             BaseCurrentMeanPlot = FALSE, adjustedPlot = FALSE)
#' 
#' # Apply gamTestSeason
#' gamResult2 <- gamTestSeason(df, dep, stat, layer, analySpec=analySpec,
#'                             gamSeasonPlot = c("7/15-8/15", "purple", "range"))
#' gamPlotDispSeason(gamResult = gamResult2, analySpec = analySpec,
#'                   fullModel = 2, seasAvgModel = 2, seasonalModel = 2,
#'                   diffType = "regular", obserPlot = TRUE, interventionPlot = TRUE,
#'                   seasAvgPlot = TRUE, seasAvgConfIntPlot = FALSE,
#'                   seasAvgSigPlot = FALSE, fullModelPlot = FALSE, seasModelPlot = FALSE,
#'                   BaseCurrentMeanPlot = TRUE, adjustedPlot = FALSE, gamSeasonFocus = TRUE)
#'       
#' @export
#' 
#' @importFrom stats quantile
#'
# ####
gamPlotDispSeason <- function(gamResult=gamResult, analySpec=analySpec, 
                              fullModel=2, seasAvgModel=2, seasonalModel=2, diffType='regular',
                              obserPlot=TRUE, interventionPlot=TRUE,
                              seasAvgPlot=TRUE, seasAvgConfIntPlot=TRUE, seasAvgSigPlot=TRUE,
                              fullModelPlot=TRUE, seasModelPlot=TRUE, BaseCurrentMeanPlot=TRUE,
                              adjustedPlot=FALSE, gamSeasonFocus=TRUE) {
  
# ----- Change history -------------------------------------------- ####
# 24May2018: JBH: copy gamPlotDisp -> gamPlotDispSeason and implement modifications for 
#                 seasonal analysis
# 12Jul2018: JBH: add magenta line when additional independent variables are in model
# 05Aug2017: JBH: added some place holder code for flw_sal bar & fitted model
# 14Mar2017: JBH: updated off scale y settings
# 05Jan2017: JBH: modified ConfInt shading to allow for missing values in pdat
#                 modified Derivative line to allow for "0's" in pdat$sa.pred1.sig
# 04Nov2016: JBH: modified to print gamDiff regular and gamDiff adjusted baseline and
#                 current means.
# 25Oct2016: JBH: added statement to reset par settings at end of function
# 20Oct2016: JBH: made adjustment to lower offscale setting calculation, yOffScale
# 17Oct2016: JBH: separated gamPlot to gamPlotCalc (calculations) and gamPlotDisp
#                 (display). This enables gamPlotDisp have a cleaner strategy for
#                 customizing plots.
# 16Jun2016: JBH: re-activated derivative code; updated pdat code to always compute
#                 predicted values for seasons and return pdat to calling function;
#                 added code to tally range of dates for significant increases/decreases
# 04Jun2016: JBH: added argument dayStep to thin plot density and reduce processing time
# 03Jun2016: JBH: Added unpack of stat and layer from iSpec
# 27Apr2016: JBH: Explicit use of "::" for non-base functions added.
# 04Feb2016: JBH: added horizontal grid lines to plots

# fullModel=2; seasAvgModel=2; seasonalModel=2; diffType='regular'
# obserPlot=TRUE; interventionPlot=TRUE; seasAvgPlot=TRUE
# seasAvgConfIntPlot=TRUE; seasAvgSigPlot=TRUE; fullModelPlot=TRUE
# seasModelPlot=TRUE; BaseCurrentMeanPlot=TRUE; adjustedPlot=FALSE
# gamSeasonFocus=TRUE
  
# Unpack & initialization #####
  tsdat     <- gamResult$data
  tsdat.all <- gamResult$data.all
  iSpec     <- gamResult$iSpec
  date.range <- c(iSpec$dateBegin, iSpec$dateEnd)
  centerYear <- iSpec$centerYear
  formula    <- iSpec$gamForm
  transform  <- iSpec$transform
  stat       <- iSpec$stat
  layer      <- iSpec$layer
  dep        <- iSpec$dep
  stat.layer <- paste(stat,layer,sep="-")
  analySpec$gamLegend$On <-FALSE
  showGamNumOnPlot <- analySpec$showGamNumOnPlot
  
  gridCol <- 'gray80'; gridlty =6
  censorlty <- 3; censorlwd <- 1
  seasAvgSiglwd <- 4
  
  # 24May2019 - modify gamLegend to address season plot
  #   ** this workaround is to allow gamPlotDispSeason to be called independently **
  {
    if (!"seasMean min/max" %in% analySpec$gamLegend$descrip) {
      
      gamSeasonPlot <- iSpec$gamSeasonPlot
      
      # unpack gamLegend from analySpec
      gamLegend <- analySpec$gamLegend
      
      # find and modify "seasMean" legend entry
      rowT = which(gamLegend$descrip == "seasMean")
      gamLegend[rowT,c("legend","colSel","colLegend")] <-
        c(gamSeasonPlot[1], gamSeasonPlot[2], gamSeasonPlot[2])
      gamLegend[rowT,c("lwdLegend")] <- 2
      
      # add legend row for min/max
      gamLegend[nrow(gamLegend)+1,] <- gamLegend[rowT,]
      gamLegend[nrow(gamLegend),c("legend","colSel","colLegend","descrip")] <-
        c(paste(gamSeasonPlot[1],'range'), gamSeasonPlot[2], gamSeasonPlot[2], "seasMean min/max")
      gamLegend[nrow(gamLegend),c("lwdLegend","ltyLegend")] <- c(1, 2)
      
      # pack gamLegend pack into analySpec
      analySpec$gamLegend     <- gamLegend 
      analySpec$gamSeasonPlot <- gamSeasonPlot
    }
  } # 24May2019 - modify gamLegend to address season plot -- end
  
  analySpec$gamLegend$On <-FALSE
  
# Review data points and set up x- and y-axis ranges ####
  conc          <- data.frame(tsdat.all$date,
                              tsdat.all$recensor,
                              as.data.frame(tsdat.all[,dep], expand = TRUE)[c(1,2)],
                              tsdat.all$doy) 
  names(conc)   <- c("date", "recensor", "lower", "upper", "doy" )
  conc$point    <- "?"
  
  yRange <- range(c(conc[is.finite(conc$lower),"lower"],
                    conc[is.finite(conc$upper),"upper"]))
  xRange <- range(conc$date)
  
  #20Oct2016: switch lower vale from "yRange[1] - abs(yRange[1])"
  #           to "yRange[1] - abs(yRange[2])"
  yOffScale <- c(yRange[1] - 1e3*abs(yRange[2]) , yRange[2] + 1e3*abs(yRange[2]))
  
  conc[conc$recensor,"point"]  <- "Recen"
  conc[!conc$recensor & conc$lower==conc$upper,"point"]    <- "Uncen"
  conc[!conc$recensor & is.infinite(conc$lower) & is.finite(conc$upper),"point"]     <- "LT"
  conc[!conc$recensor & is.finite(conc$lower)   & is.infinite(conc$upper),"point"]   <- "GT"
  conc[!conc$recensor & is.finite(conc$lower)   & is.finite(conc$upper) & conc$lower!=conc$upper,"point"]    <- "Int"
  
  conc$lower2   <- conc$lower
  conc$upper2   <- conc$upper
  conc[is.infinite(conc$lower2),"lower2"] <- yOffScale[1]
  conc[is.infinite(conc$upper2),"upper2"] <- yOffScale[2]
  
  # downselect observations to specific season if gamSeasonFocus 24May2019
  if (gamSeasonFocus) {
    seasMean <- unlist(strsplit(analySpec$gamSeasonPlot[1], "-"))
    q2.range <- as.numeric(baytrends::baseDay(lubridate::mdy (paste0(seasMean ,"/2000"))))
    if (!grepl('-',analySpec$gamSeasonPlot[1])) {
      q2.range[1] <- q2.range[1] - 15
      q2.range[2] <- q2.range[1] + 30 
    }   
    conc <- conc[conc$doy >= q2.range[1] & conc$doy <= q2.range[2], ]
  }
  # downselect observations to specific season if gamSeasonFocus -- end 
  
# Set up plot, ####
  #opar <- par()
  #par(oma = c(1.5, 0,0,0))
  #par(fig = c(0, 1, 0, 1), oma = c(1.5, 0,0,0), mar = c(5.1, 4.1, 4.1, 2.1), new = FALSE)
  par(fig = c(0, 1, 0, 1), oma = c(1.5, 0,0,0), mar = c(3.5, 4.1, 2.7, 2.1), new = FALSE)
  
  if(transform) {
    plot(y=exp(yRange[1]), x=xRange[1], log='y',  type='n', ylim=exp(yRange), xlim=xRange,
         cex=.6 , cex.axis=.8 ,ann=FALSE  , par(mar=c(3,3.8,3,1)))
  } else {
    plot(y=(yRange[1]), x=xRange[1], log='',  type='n', ylim=(yRange), xlim=xRange,
         cex=.6 , cex.axis=.8 ,ann=FALSE  , par(mar=c(3,3.8,3,1)))
  }
  
# add flow or salinity indicators #05Aug2017 ####
  if('flw_sal' %in% names(tsdat)) {
    myPCH = 124; myCEX=1.0; HiFlw_LoSal = "springgreen"; LoFlw_HiSal = "darkblue"
    y_hydro <- ifelse(transform, exp(yRange[2]), yRange[2])
    prob_hydro <- 0.80
    if(iSpec$hydroTermSel == 'flow') {
      tmp =  data.frame(x=tsdat[tsdat$flw_sal > quantile(tsdat$flw_sal,probs=prob_hydro), "date"],
                        y=y_hydro)
      points(y=tmp$y , x=tmp$x, col=HiFlw_LoSal, pch=myPCH, cex=myCEX)
      tmp =  data.frame(x=tsdat[tsdat$flw_sal < quantile(tsdat$flw_sal,probs=(1-prob_hydro)), "date"],
                        y=y_hydro)
      points(y=tmp$y , x=tmp$x, col=LoFlw_HiSal, pch=myPCH, cex=myCEX)
      
    } else if(iSpec$hydroTermSel == 'salinity') {
      tmp =  data.frame(x=tsdat[tsdat$flw_sal < quantile(tsdat$flw_sal,probs=(1-prob_hydro)), "date"],
                        y=y_hydro)
      points(y=tmp$y , x=tmp$x, col=HiFlw_LoSal, pch=myPCH, cex=myCEX)
      tmp =  data.frame(x=tsdat[tsdat$flw_sal > quantile(tsdat$flw_sal,probs=prob_hydro), "date"],
                        y=y_hydro)
      points(y=tmp$y , x=tmp$x, col=LoFlw_HiSal, pch=myPCH, cex=myCEX)
    }
  }
  
# add axes, grid lines, and labels  ####
  grid (NA, NULL, lty = gridlty, col = gridCol)
  abline(v=axis.POSIXct(1, x=pretty(tsdat$date), labels = FALSE),lty = gridlty, col = gridCol)
  mtext(side=2,text=paste0(iSpec$parmName," [",iSpec$parmUnits,"]"),line=2.2,cex=.9)
  title(paste0(iSpec$parmName,"-",iSpec$layerName," at ", iSpec$stat),cex.main=.9)
  if (showGamNumOnPlot) mtext(side=3,text=fullModel,line=0.5,cex=.8,adj=1, col='black')   #2019Jun24
  
# fit.GAM #05Aug2017 ; 01Oct2017 ; 24Nov2017####
  # gamOutput4 <-gamResult$gamOutput4$gamRslt$fitted.values
  # fittedValues <- TRUE
  fit.GAM <- ifelse (length(grep('flw_sal',
                                 gamResult[[paste0("gamOutput",fullModel)]]$gamRslt$formula)) == 0, FALSE, TRUE)
  
  if (!fit.GAM) {
    # identify additional independent variables #12Jul2018
    # excluding dependent variable and "gamK*" 
    indVar <- setdiff (all.vars(gamResult[[paste0("gamOutput",fullModel)]]$gamRslt$formula)
                       , c(iSpec$dep, "cyear", "doy", "intervention", "flw_sal"))
    indVar <- indVar[-grep("^gamK", indVar)]
    fit.GAM <- length(indVar) > 0
  }
  
  if(fit.GAM && fullModelPlot) {
    pdat <- data.frame(date = gamResult$data$date,
                       pred1 = gamResult[[paste0("gamOutput",fullModel)]]$gamRslt$fitted.values)
    colSelA  <- 'magenta'
    lwdSelA  <- 1
    ltySelA  <- 3
    
    if(transform) {
      lines(exp(pdat$pred1)~pdat$date,col=colSelA, lwd=lwdSelA,lty=ltySelA)
    } else {
      lines((pdat$pred1)~pdat$date,col=colSelA, lwd=lwdSelA,lty=ltySelA)
    }
  }
  
# shading for CI ####
  if(seasAvgConfIntPlot) {
    pdat <- gamResult[[paste0("gamOutput",seasAvgModel)]]$predictions
    
    # 05Jan2017: remove records in prediction data set with NAs
    pdat <- pdat[!is.na(pdat$cilb) & !is.na(pdat$ciub),]
    
    colSelA <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip =="seasConfInt")]
    
    analySpec$gamLegend$On[which(analySpec$gamLegend$descrip =="seasConfInt")] <- TRUE
    
    ciub <- as.matrix(pdat[, c('ndate','ciub')])
    cilb <- as.matrix(pdat[, c('ndate','cilb')])
    cilb <- cilb[order(cilb[,1],decreasing=TRUE),]
    ci.polygon <- rbind(ciub,cilb)
    
    # polys <- unique(pdat$intervention)
    # table(pdat$intervention)
    
    if(transform) {
      polygon(ci.polygon[,1], exp(ci.polygon[,2]),col=colSelA,border=NA)
    } else {
      polygon(ci.polygon[,1], (ci.polygon[,2]),col=colSelA,border=NA)
    }
  }
  
# add intervention dates ####
  if(interventionPlot & nrow(iSpec$intervenList)>1) {
    
    colSelA <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip =="interventionchange")]
    ltySelA <- analySpec$gamLegend$ltyLegend[which(analySpec$gamLegend$descrip =="interventionchange")]
    lwdSelA <- analySpec$gamLegend$lwdLegend[which(analySpec$gamLegend$descrip =="interventionchange")]
    
    analySpec$gamLegend$On[which(analySpec$gamLegend$descrip =="interventionchange")] <- TRUE
    
    abline(v=iSpec$intervenList[2:nrow(iSpec$intervenList),"beginDate"], col=colSelA, lty=ltySelA, lwd=lwdSelA)
  }
  
# plot points ####
  if(obserPlot) {
    
    #Recensored points
    colSelA <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip =="recensored")]
    pchSelA <- analySpec$gamLegend$pchLegend[which(analySpec$gamLegend$descrip =="recensored")]
    
    if(nrow(conc[conc$point=="Recen",])>0) {
      analySpec$gamLegend$On[which(analySpec$gamLegend$descrip =="recensored")] <- TRUE
    }
    
    if(transform) {
      points(conc[conc$point=="Recen","date"], exp(conc[conc$point=="Recen","upper"]), pch=pchSelA, col=colSelA, cex=0.6)
    } else {
      points(conc[conc$point=="Recen","date"],    (conc[conc$point=="Recen","upper"]), pch=pchSelA, col=colSelA, cex=0.6)
    }
    
    #Select and graph censored data
    colSelA <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip =="censored")]
    pchSelA <- analySpec$gamLegend$pchLegend[which(analySpec$gamLegend$descrip =="censored")]
    
    # Subset censored data to tmp data frame
    tmp <- conc [conc$point=="LT" | conc$point=="GT" | conc$point=="Int",]
    
    if(nrow(tmp)>0) {
      analySpec$gamLegend$On[which(analySpec$gamLegend$descrip =="censored")] <- TRUE
    }
    
    # do plot function if at least one record in tmp
    if (nrow(tmp)>0) {
      # loop thru each point and plot short line
      for (iRow in 1:nrow(tmp)) {
        x  <- tmp[iRow,"date"]
        y1 <- tmp[iRow,"lower2"]
        y2 <- tmp[iRow,"upper2"]
        if(transform) {
          lines(c(x,x), c(exp(y1),exp(y2)), type="l", lwd=censorlwd, col=colSelA, lty=censorlty)
        } else {
          lines(c(x,x), c(   (y1),   (y2)), type="l", lwd=censorlwd, col=colSelA, lty=censorlty)
        }
      }
      # plot symbols at lower and upper limit
      if(transform) {
        lines(tmp[,"date"],     exp(tmp[,"lower2"])         , type="p", pch=pchSelA,  col=colSelA, cex=0.6)
        lines(tmp[,"date"],     exp(tmp[,"upper2"])         , type="p", pch=pchSelA,  col=colSelA, cex=0.6)
      } else {
        lines(tmp[,"date"],        (tmp[,"lower2"])         , type="p", pch=pchSelA,  col=colSelA, cex=0.6)
        lines(tmp[,"date"],        (tmp[,"upper2"])         , type="p", pch=pchSelA,  col=colSelA, cex=0.6)
      }
    }
    
    # Uncensored
    colSelA <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip =="uncensored")]
    pchSelA <- analySpec$gamLegend$pchLegend[which(analySpec$gamLegend$descrip =="uncensored")]
    
    if(nrow(conc[conc$point=="Uncen",])>0) {
      analySpec$gamLegend$On[which(analySpec$gamLegend$descrip =="uncensored")] <- TRUE
    }
    
    if(transform) {
      points(conc[conc$point=="Uncen","date"], exp(conc[conc$point=="Uncen","upper"]), pch=pchSelA, col=colSelA, cex=0.6)
    } else {
      points(conc[conc$point=="Uncen","date"],    (conc[conc$point=="Uncen","upper"]), pch=pchSelA, col=colSelA, cex=0.6)
    }
  }
  
# Derivative processing #####
  if(seasAvgSigPlot) {
    pdat <- gamResult[[paste0("gamOutput",seasAvgModel)]]$predictions
    colSelA <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip =="seasSignif")]
    
    analySpec$gamLegend$On[which(analySpec$gamLegend$descrip =="seasSignif")] <- TRUE
    
    # 05Jan2017:
    pdat[!is.na(pdat$sa.pred1.sig) & pdat$sa.pred1.sig==0, "sa.pred1.sig"] <- NA
    
    # derivative plotting
    if(transform) {
      lines(pdat[pdat$sa.pred1.sig==-1,"date"], exp(pdat[pdat$sa.pred1.sig==-1,"sa.pred1"]),  lwd = seasAvgSiglwd, col = colSelA)
      lines(pdat[pdat$sa.pred1.sig== 1,"date"], exp(pdat[pdat$sa.pred1.sig== 1,"sa.pred1"]),  lwd = seasAvgSiglwd, col = colSelA)
    } else {
      lines(pdat[pdat$sa.pred1.sig==-1,"date"],    (pdat[pdat$sa.pred1.sig==-1,"sa.pred1"]),  lwd = seasAvgSiglwd, col = colSelA)
      lines(pdat[pdat$sa.pred1.sig== 1,"date"],    (pdat[pdat$sa.pred1.sig== 1,"sa.pred1"]),  lwd = seasAvgSiglwd, col = colSelA)
    }
  }
  
# fitted GAM for seasonally-adjusted GAM  ####
  if(seasAvgPlot) {
    pdat <- gamResult[[paste0("gamOutput",seasAvgModel)]]$predictions
    colSelA  <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip =="seasAverage")]
    lwdSelA  <- analySpec$gamLegend$lwdLegend[which(analySpec$gamLegend$descrip =="seasAverage")]
    ltySelA  <- analySpec$gamLegend$ltyLegend[which(analySpec$gamLegend$descrip =="seasAverage")]
    
    analySpec$gamLegend$On[which(analySpec$gamLegend$descrip =="seasAverage")] <- TRUE
    
    if(transform) {
      lines(exp(pdat$sa.pred1)~pdat$date,col=colSelA, lwd=lwdSelA, lty=ltySelA)
      if(adjustedPlot & "sa.pred1.adjusted" %in% names(pdat)) {
        lines(exp(pdat$sa.pred1.adjusted)~pdat$date,col=colSelA, lwd=lwdSelA, lty=ltySelA)
      }
    } else {
      lines((pdat$sa.pred1)~pdat$date,col=colSelA, lwd=lwdSelA, lty=ltySelA)
      if(adjustedPlot & "sa.pred1.adjusted" %in% names(pdat)) {
        lines((pdat$sa.pred1.adjusted)~pdat$date,col=colSelA, lwd=lwdSelA, lty=ltySelA)
      }
    }
  }
  
# fitted GAM for full model GAM  ####
  if(fullModelPlot) {
    pdat <- gamResult[[paste0("gamOutput",fullModel)]]$predictions
    colSelA  <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip =="fullGAM")]
    lwdSelA  <- analySpec$gamLegend$lwdLegend[which(analySpec$gamLegend$descrip =="fullGAM")]
    ltySelA  <- analySpec$gamLegend$ltyLegend[which(analySpec$gamLegend$descrip =="fullGAM")]
    
    analySpec$gamLegend$On[which(analySpec$gamLegend$descrip =="fullGAM")] <- TRUE
    
    if(transform) {
      lines(exp(pdat$pred1)~pdat$date,col=colSelA, lwd=lwdSelA,lty=ltySelA)
      if(adjustedPlot & "pred1.adjusted" %in% names(pdat)) {
        lines(exp(pdat$pred1.adjusted)~pdat$date,col=colSelA, lwd=lwdSelA,lty=ltySelA)
      }
    } else {
      lines((pdat$pred1)~pdat$date,col=colSelA, lwd=lwdSelA,lty=ltySelA)
      if(adjustedPlot & "pred1.adjusted" %in% names(pdat)) {
        lines((pdat$pred1.adjusted)~pdat$date,col=colSelA, lwd=lwdSelA,lty=ltySelA)
      }
    }
  }  
  
# fitted GAM for seasonal models  ####
  if(seasModelPlot) {
    pdat <- gamResult[[paste0("gamOutput",seasonalModel)]]$predictions
    
    for (i in 1:length(analySpec$gamLegend[analySpec$gamLegend$season, "legend"])) {
      descrip = paste0("season",i)
      colSelA  <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip ==descrip)]
      lwdSelA  <- analySpec$gamLegend$lwdLegend[which(analySpec$gamLegend$descrip ==descrip)]
      ltySelA  <- analySpec$gamLegend$ltyLegend[which(analySpec$gamLegend$descrip ==descrip)]
      
      analySpec$gamLegend$On[which(analySpec$gamLegend$descrip ==descrip)] <- TRUE
      
      if(transform) {
        lines(exp(pdat[, paste0("seas.",i)])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        if(adjustedPlot & paste0("seas.",i,".adjusted") %in% names(pdat)) {
          lines(exp(pdat[, paste0("seas.",i,".adjusted")])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        }
      } else {
        lines(   (pdat[, paste0("seas.",i)])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        if(adjustedPlot & paste0("seas.",i,".adjusted") %in% names(pdat)) {
          lines(   (pdat[, paste0("seas.",i,".adjusted")])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        }
      }
    }
  }

# gamSeasonFocus: plot seasonal model 28May2019 ####
  
  if(gamSeasonFocus) {
    pdat <- gamResult[[paste0("gamOutput",seasonalModel)]]$predictions
    
    colSelA  <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip == "seasMean")]
    lwdSelA  <- analySpec$gamLegend$lwdLegend[which(analySpec$gamLegend$descrip == "seasMean")]
    ltySelA  <- analySpec$gamLegend$ltyLegend[which(analySpec$gamLegend$descrip == "seasMean")]
    
    analySpec$gamLegend$On[which(analySpec$gamLegend$descrip =="seasMean")] <- TRUE
    
    if(transform) {
      lines(exp(pdat[ ,"seasMeanMean"])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
      if(adjustedPlot & paste0("seasMeanMean",".adjusted") %in% names(pdat)) {
        lines(exp(pdat[, paste0("seasMeanMean",".adjusted")])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
      }
    } else {
      lines(   (pdat[, "seasMeanMean"])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
      if(adjustedPlot & paste0("seasMeanMean",".adjusted") %in% names(pdat)) {
        lines(   (pdat[, paste0("seasMeanMean",".adjusted")])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
      }
    }
    
    # add min/max
    # check to make sure it makes sense to add range (gamSeasonPlot[1] is in "range" format,
    # gamSeasonPlot[3] exists and it has value of 'range')
    if (grepl('-',analySpec$gamSeasonPlot[1]) && exists(analySpec$gamSeasonPlot[3]) && analySpec$gamSeasonPlot[3]=='range') {

      myDescription <- "seasMean min/max"
      
      analySpec$gamLegend$On[which(analySpec$gamLegend$descrip == myDescription)] <- TRUE
            colSelA  <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip == myDescription)]
      lwdSelA  <- analySpec$gamLegend$lwdLegend[which(analySpec$gamLegend$descrip == myDescription)]
      ltySelA  <- analySpec$gamLegend$ltyLegend[which(analySpec$gamLegend$descrip == myDescription)]
      
      
      myDescription <- "seasMeanMin"
      
      if(transform) {
        lines(exp(pdat[ ,myDescription])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        if(adjustedPlot & paste0(myDescription,".adjusted") %in% names(pdat)) {
          lines(exp(pdat[, paste0(myDescription,".adjusted")])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        }
      } else {
        lines(   (pdat[, myDescription])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        if(adjustedPlot & paste0(myDescription,".adjusted") %in% names(pdat)) {
          lines(   (pdat[, paste0(myDescription,".adjusted")])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        }
      }
      
      myDescription <- "seasMeanMax"
      
      if(transform) {
        lines(exp(pdat[ ,myDescription])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        if(adjustedPlot & paste0(myDescription,".adjusted") %in% names(pdat)) {
          lines(exp(pdat[, paste0(myDescription,".adjusted")])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        }
      } else {
        lines(   (pdat[, myDescription])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        if(adjustedPlot & paste0(myDescription,".adjusted") %in% names(pdat)) {
          lines(   (pdat[, paste0(myDescription,".adjusted")])~pdat$date,col=colSelA,lty=ltySelA,lwd=lwdSelA)
        }
      }
      
    }
  } # gamSeasonFocus: plot seasonal model -- end
    
# Baseline and current mean plot ####
  if(BaseCurrentMeanPlot) {
    
    if(diffType=='regular' | diffType=='both') {
      porDiff <- gamResult[[paste0("gamOutput",fullModel)]]$porDiff.regular
      colSelA <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip =="BaseCurrentMean")]
      pchSelA <- analySpec$gamLegend$pchLegend[which(analySpec$gamLegend$descrip =="BaseCurrentMean")]
      
      analySpec$gamLegend$On[which(analySpec$gamLegend$descrip =="BaseCurrentMean")] <- TRUE
      
      por <-  date.range + c(365*24*60*60,-365*24*60*60)
      if(transform) {
        points(exp(c(porDiff$per.mn))~por,pch=pchSelA,col=colSelA,cex=1.5)
      } else {
        points(   (c(porDiff$per.mn))~por,pch=pchSelA,col=colSelA,cex=1.5)
      }
    }
    
    if(diffType=='adjusted' | diffType=='both') {
      porDiff <- gamResult[[paste0("gamOutput",fullModel)]]$porDiff.adjusted
      colSelA <- analySpec$gamLegend$colSel[which(analySpec$gamLegend$descrip =="BaseCurrentMeanAdj")]
      pchSelA <- analySpec$gamLegend$pchLegend[which(analySpec$gamLegend$descrip =="BaseCurrentMeanAdj")]
      
      analySpec$gamLegend$On[which(analySpec$gamLegend$descrip =="BaseCurrentMeanAdj")] <- TRUE
      
      por <-  date.range + c(365*24*60*60,-365*24*60*60)
      if(transform) {
        points(exp(c(porDiff$per.mn))~por,pch=pchSelA,col=colSelA,cex=1.0)
      } else {
        points(   (c(porDiff$per.mn))~por,pch=pchSelA,col=colSelA,cex=1.0)
      }
    }
    
  }
  
# Legend ####
  
  myLegend <- analySpec$gamLegend[analySpec$gamLegend$On, ]
  
  # update legend for flow or salinity #05Aug2017
  if('flw_sal' %in% names(tsdat)) {
    if(iSpec$hydroTermSel == 'flow') {
      myLegend <- rbind(myLegend,
                        data.frame(legend     = 'Hi Flw',      colSel       = HiFlw_LoSal,    colLegend = HiFlw_LoSal,
                                   lwdLegend  = NA_real_,      ltyLegend    = NA_real_,      pchLegend = 15,
                                   fillLegend = NA_character_, borderLegend = NA_character_, season    = FALSE,
                                   descrip    = "hydro1",      On           = TRUE),
                        data.frame(legend     = 'Lo Flw',      colSel       = LoFlw_HiSal,    colLegend = LoFlw_HiSal,
                                   lwdLegend  = NA_real_,      ltyLegend    = NA_real_,      pchLegend = 15,
                                   fillLegend = NA_character_, borderLegend = NA_character_, season    = FALSE,
                                   descrip    = "hydro2",      On           = TRUE))
      
      
    } else if(iSpec$hydroTermSel == 'salinity') {
      myLegend <- rbind(myLegend,
                        data.frame(legend     = 'Lo Sal',      colSel       = HiFlw_LoSal,    colLegend = HiFlw_LoSal,
                                   lwdLegend  = NA_real_,      ltyLegend    = NA_real_,      pchLegend = 15,
                                   fillLegend = NA_character_, borderLegend = NA_character_, season    = FALSE,
                                   descrip    = "hydro1",      On           = TRUE),
                        data.frame(legend     = 'Hi Sal',      colSel       = LoFlw_HiSal,    colLegend = LoFlw_HiSal,
                                   lwdLegend  = NA_real_,      ltyLegend    = NA_real_,      pchLegend = 15,
                                   fillLegend = NA_character_, borderLegend = NA_character_, season    = FALSE,
                                   descrip    = "hydro2",      On           = TRUE))
    }
  }
  
  #update legend for fitted model
  if(fit.GAM && fullModelPlot) {
    myLegend <- rbind(myLegend,
                      data.frame(legend     = 'Fit.GAM',     colSel       = 'magenta',    colLegend = 'magenta',
                                 lwdLegend  = 1 ,            ltyLegend    = 3 ,      pchLegend = NA_real_  ,
                                 fillLegend = NA_character_, borderLegend = NA_character_, season    = FALSE,
                                 descrip    = "fit.gam",      On           = TRUE))
  } else {
    myLegend[myLegend$legend=="Adj.GAM", "legend"] <- "Fit.GAM"
  }
  
  ncolSel=1
  ncolSel <- ifelse(nrow(myLegend)>7, ceiling(nrow(myLegend)/2) , nrow(myLegend))
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(.5, 3, 0, 2), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend(x="bottom",
         legend  = myLegend$legend,
         col     = myLegend$colLegend ,
         lwd     = myLegend$lwdLegend ,
         lty     = myLegend$ltyLegend ,
         pch     = myLegend$pchLegend ,
         fill    = myLegend$fillLegend ,
         border  = myLegend$borderLegend , ncol=ncolSel,
         cex=.65, xpd = TRUE, horiz = FALSE,inset = c(0,0), x.intersp=0.4)
  
# reset graphical parameter settings ####
  
  # 25Oct2016
  par(fig = c(0, 1, 0, 1), oma= c(0, 0, 0, 0), mar=c(5.1, 4.1, 4.1, 2.1))
  
  
}
