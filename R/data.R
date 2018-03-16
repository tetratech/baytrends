#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Censored Data Example
#' 
#' @description A dataset with example censored data.
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
#' @title Layer Lookup
#' 
#' @description Layer Lookup
#' 
#' @format A data frame with 10 rows and 3 variables:
#' \describe{
#'    \item{layers}{layer abbreviation}
#'    \item{order}{sort order of layers}
#'    \item{name}{layer name}
#' }
"layerLukup"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parameter List
#' 
#' @description  Parameter List
#' 
#' @format A data frame with 81 rows and 13 variables:
#' \describe{
#'    \item{parm}{Parameter}
#'    \item{parmSource}{Original Parameter}
#'    \item{parmCat}{Category}
#'    \item{parmName}{Name}
#'    \item{parmCalc}{Calculated?}
#'    \item{parmNamelc}{}
#'    \item{parmUnits}{Units}
#'    \item{parmRecensor}{}
#'    \item{parmRO1}{}
#'    \item{parmRO2}{}
#'    \item{parmTrend}{}
#'    \item{logTrans}{}
#'    \item{trendIncrease}{}
#' }
"parameterList"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Master Stations List
#' 
#' @description Master Stations List
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
#'    \item{usgsGageName}{}
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
#' @title USGS Gages
#' 
#' @description USGS Gages
#' 
#' @format A data frame with 9 rows and 2 variables:
#' \describe{
#'    \item{usgsGageID}{USGS Gage ID}
#'    \item{siteName}{USGS Site Name}
#' }
"usgsGages"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Detrended Flow Data
#' 
#' @description Detrended Flow Data
#' 
#' @format A list of 8:
#' \describe{
#'    \item{retreiveDate}{Data retrieval date}
#'    \item{gages}{data frame with 2 rows and 2 variables: usgsGageID and siteName}
#'    \item{yearStart}{Start year}
#'    \item{yearEnd}{End year}
#'    \item{dvAvgWinSel}{}
#'    \item{dvAvgWgtSel}{}
#'    \item{dvAvgSidesSel}{}
#'    \item{lowess.f}{}
#' }
"flow.detrended"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
