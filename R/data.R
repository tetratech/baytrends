#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dataCensored ####
#' @title Chesapeake Bay Program Monitoring Data, 1985-2016
#' 
#' @description Selected 1985-2016 data for eight (8) stations from the
#'   Chesapeake Bay Monitoring Program. Water quality variables are stored as
#'   class qw that allows for left- and interval-censored data.
#' 
#' @format A data frame with 13,062 rows and 17 variables:
#' \describe{
#'   \item{station}{station identifier}
#'   \item{date}{sample date}
#'   \item{layer}{sample layer}
#'   \item{secchi}{Secchi depth [m]}
#'   \item{salinity}{Salinity [ppt]}
#'   \item{do}{Dissolved Oxygen [mg/L]}
#'   \item{wtemp}{Water Temperature [deg C]}
#'   \item{chla}{Corrected Chlorophyll-a [ug/L]}
#'   \item{tn}{Total Nitrogen [mg/L]}
#'   \item{tp}{Total Phosphorus [mg/L]}
#'   \item{tss}{Total Suspended Solids [mg/L]}
#'   \item{din}{Dissolved Inorganic Nitrogen [mg/L]}
#'   \item{po4}{Orthophosphorus [mg/L]}
#'   \item{tdn}{Total Dissolved Nitrogen [mg/L]}
#'   \item{tdp}{Total Dissolved Phosphorus [mg/L]}
#'   \item{nh4}{Ammonium [mg/L]}
#'   \item{no23}{Nitrite + Nitrate [mg/L]}
#' }
"dataCensored"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# layerLukup ####
#' @title Layer List
#' 
#' @description A lookup table of layer abbreviations and the corresponding
#'   names.
#' 
#' @format A data frame with 10 rows and 3 variables:
#' \describe{
#'    \item{layers}{Layer codes}
#'    \item{order}{Analysis order}
#'    \item{name}{Layer name}
#' }
"layerLukup"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# parameterList ####
#' @title Parameter List
#' 
#' @description A lookup table of water quality parameters 
#' 
#' @format A data frame with 81 rows and 13 variables:
#' \describe{
#'    \item{parm}{"preferred" parameter ID}
#'    \item{parmSource}{parmamter field as may be found in sample data sets}
#'    \item{parmCat}{Parameter Group Header}
#'    \item{parmName}{Full parameter name}
#'    \item{parmCalc}{indicator if parameter is calculated}
#'    \item{parmNamelc}{parameter name in lower case}
#'    \item{parmUnits}{units}
#'    \item{parmRecensor}{numerical value used in log plots if data are reported
#'    as <= 0}
#'    \item{parmRO1}{Parameter sort order level 1}
#'    \item{parmRO2}{Parameter sort order level 2}
#'    \item{parmTrend}{TRUE/FALSE for whether parameter should be analyzed for
#'    trend}
#'    \item{logTrans}{TRUE/FALSE for whether parameter should be analyzed in log
#'    transposed}
#'    \item{trendIncrease}{Should an increase in conccentration be interpreted
#'    as 'improving', 'degrading', 'increasing', or 'decreasing'}
#' }
"parameterList"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# stationMasterList ####
#' @title Chesapeake Bay Program long-term tidal monitoring stations
#' 
#' @description Chesapeake Bay Program long-term tidal monitoring stations
#' 
#' @format A data frame with 145 rows and 19 variables:
#' \describe{
#'    \item{station}{Water quality station code}
#'    \item{state}{State location}
#'    \item{locationType}{As identified in the state trend reports, whether this
#'    station is in the mainstem or tributary monitoring}
#'    \item{waterbody}{Location as identified in the CBP CIMS database}
#'    \item{latitude}{From the CBP CIMS database, and verified in a GIS map}
#'    \item{longitude}{From the CBP CIMS database, and verified in a GIS map}
#'    \item{cbSeg92}{Match to CB 92 Segmentation scheme (for 303d list)}
#'    \item{usgsGageName}{Manual match to a USGS fall-line monitoring station
#'    (See usgsGages$siteName)}
#'    \item{usgsGageID}{USGS station code (See usgsGages$siteNumber)}
#'    \item{usgsGageMatch}{Identifies how the USGS-to-TribStation match was
#'    made. Direct: If the station falls within a tributary with a USGS station,
#'    or in the mainstem. Indirect: If the station is in a small sub-tributary
#'    of one of the major tributaries. The USGS station may not be that
#'    representative, but it is better than matching to the Susquehanna.
#'    SusDefault: If there was no clear match, the tidal station was matched to
#'    the Susquehanna River.}
#'    \item{stationRO1}{Station sort order level 1}
#'    \item{stationRO2}{Station sort order level 2}
#'    \item{stationGrpName}{Station group header}
#'    \item{stationMethodGroup}{Foreign key to methodsList table}
#'    \item{hydroTerm}{"flow" or "salinity" to indicate whether to model flow or
#'    salinity effects (if missing, "flow" assumed)}
#'    \item{flwAvgWin}{list of averaging windows if flow is selected in
#'    hydroTerm (options are: 1, 5, 10, 15, 20, 30, 40, 50, 60, 90, 120, 150,
#'    180, 210)}
#'    \item{flwParms}{list of parameters to model with flow (regardless of hydroTerm
#'    value), each parameter separated by a space}
#'    \item{salParms}{list of parameters to model with salinity (regardless of
#'    hydroTerm value), each parameter separated by a space}
#'    \item{notes}{Optional note to track updates}
#' }
"stationMasterList"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# usgsGages ####
#' @title USGS Gages
#' 
#' @description List of core USGS gages for CBP trend analyses
#' 
#' @format A data frame with 9 rows and 2 variables:
#' \describe{
#'    \item{usgsGageID}{USGS Gage ID}
#'    \item{siteName}{USGS Site Name}
#' }
"usgsGages"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sal ####
#' @title Salinity data
#' 
#' @description Salinity data, 1984 to 2016, for 8 stations.
#' 
#' @format A data frame with 51,092 rows and 4 variables:
#' \describe{
#'    \item{station}{CBP Station ID}
#'    \item{date}{date, YYYY-MM-DD}
#'    \item{layer}{sample layer}
#'    \item{salinity}{Measured salinity}
#' }
"sal"
