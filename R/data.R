#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Censored Data Example
#' 
#' A dataset with example censored data.
#' 
#' @format A data frame with 7719 rows and 16 variables:
#' \describe{
#'    \item{station}{Station code}
#'    \item{date}{}
#'    \item{layer}{}
#'    \item{secchi}{}
#'    \item{chla}{}
#'    \item{do}{}
#'    \item{tn}{}
#'    \item{tp}{}
#'    \item{po4f}{}
#'    \item{pp}{}
#'    \item{tdp}{}
#'    \item{no23f}{}
#'    \item{nh4f}{}
#'    \item{tdn}{}
#'    \item{pn}{}
#'    \item{tss}{}
#' }
#' @source Chesapeake Bay data
"dataCensored"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' Layer Lookup
# #' 
# #' Long Description
# #' 
# #' @format A data frame with 10 rows and 3 variables:
# #' \describe{
# #'    \item{layers}{layer abbreviation}
# #'    \item{order}{sort order of layers}
# #'    \item{names}{layer name}
# #' }
# "layerLukUp"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parameter List
#' 
#' Long Description
#' 
#' @format A data frame with 81 rows and 13 variables:
#' \describe{
#'    \item{parm}{}
#'    \item{parmSource}{}
#'    \item{parmCat}{}
#'    \item{parmName}{}
#'    \item{parmCalc}{}
#'    \item{parmNamelc}{}
#'    \item{parmUnits}{}
#'    \item{parmSource}{}
#'    \item{parmRecensor}{}
#'    \item{parmRO1}{}
#'    \item{parmRO2}{}
#'    \item{parmTrend}{}
#'    \item{logTrans}{}
#'    \item{order}{}
#'    \item{trendIncrease}{}
#' }
"parameterList"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Master Stations List
#' 
#' Long Description
#' 
#' @format A data frame with 145 rows and 19 variables:
#' \describe{
#'    \item{station}{}
#'    \item{state}{}
#'    \item{locationType}{}
#'    \item{waterbody}{}
#'    \item{latitude}{}
#'    \item{longitude}{}
#'    \item{cbSeg92}{}
#'    \item{usgsGageID}{}
#'    \item{usgsGageMatch}{}
#'    \item{stationRO1}{}
#'    \item{stationRO2}{}
#'    \item{stationGrpName}{}
#'    \item{stationMethodGroup}{}
#'    \item{hydroTerm}{}
#'    \item{flwAvgWin}{}
#'    \item{flwParms}{}
#'    \item{salParms}{}
#'    \item{notes}{}
#' }
"stationMasterList"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' USGS Gages
#' 
#' Long Description
#' 
#' @format A data frame with 9 rows and 2 variables:
#' \describe{
#'    \item{usgsGageID}{USGS Gage ID}
#'    \item{siteName}{USGS Site Name}
#' }
"usgsGages"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
