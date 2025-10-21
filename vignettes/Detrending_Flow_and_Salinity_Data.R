## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----initialize,echo=TRUE-----------------------------------------------------
library(baytrends)

## ----usgsgages, echo=FALSE, results='asis'------------------------------------
.T("Chesapeake Bay River Input Monitoring Stations",1,'e')
knitr::kable(usgsGages)

## ----flow, eval=FALSE, results='asis', fig.height=6.5, fig.width=6.5----------
# library(baytrends)
# # Define Function Inputs
# usgsGageID    <- usgsGages$usgsGageID # all RIM stations
# siteName      <- usgsGages$siteName   # all RIM stations
# yearStart     <- 1983
# yearEnd       <- 2017
# dvAvgWinSel   <- c(1, 5, 10, 15, 20, 30, 40, 50, 60, 90, 120, 150, 180, 210)
# dvAvgWgtSel   <- "uniform"
# dvAvgSidesSel <- 1
# lowess.f      <- 0.2
# 
# # Run detrended.flow function
# flow.detrended <- detrended.flow(usgsGageID, siteName, yearStart, yearEnd
#                                  , dvAvgWinSel, dvAvgWgtSel, dvAvgSidesSel
#                                  , lowess.f)
# 
# # Save list to data file separate use
# save(flow.detrended, file='mySeasonallyDetrendedFlow.rda')

