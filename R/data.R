#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dataCensored ####
#' @title Chesapeake Bay Program Monitoring Data, 1985-2016
#' 
#' @description Selected 1985-2016 data for eight (8) stations from the
#'   Chesapeake Bay Monitoring Program. Water quality variables are stored as
#'   either class 'num' for variables with no censoring or class 'Surv' 
#'   (see survival::Surv) that allows for left- and interval-censored data.
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
#' @format A data frame with 82 rows and 13 variables:
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Q05078470 ####
#' @title Daily Streamflow
#' 
#' @description Daily mean flow for Judicial Ditch 64 near Mentor, Minnesota (SW4), USGS
#' station number 05078470 for calendar year 2003.
#' 
#' From smwrData.
#' 
#' @name Q05078470
#' @docType data
#' @usage Q05078470
#' @format Data frame with 365 rows and 2 columns\cr
#' \tabular{lll}{ 
#' Name \tab  Type \tab Description\cr\cr
#' DATES \tab Date \tab Day\cr
#' FLOW \tab numeric \tab Daily mean streamflow\cr
#' } Note: the daily mean streamflow rates are in
#' cubic feet per second.
#' @source Data retrieved from NWISWeb
#' (\url{https://nwis.waterdata.usgs.gov/mn/nwis/sw}) on 2005-01-05.
#' @keywords datasets
#' @examples
#' data(Q05078470)
#' # Plot the data for station 05078470
#' with(Q05078470, plot(DATES, FLOW, type='l'))
"Q05078470"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QWstacked
#' @title Stream Water Quality
#' 
#' @description Selected water-quality data for Judicial Ditch 64 near Mentor, Minnesota
#' (SW4), USGS station number 05078470 for calendar year 2003.
#' 
#' From smwrData.
#' 
#' @name QWstacked
#' @docType data
#' @usage QWstacked
#' @format Data frame with 46 rows and 22 columns\cr
#' \tabular{lll}{ 
#' Name \tab Type \tab Description\cr\cr
#' agency_cd \tab character \tab Agency code\cr
#' site_no \tab character \tab USGS station number\cr
#' sample_dt \tab Date \tab Sample date\cr
#' sample_tm \tab character \tab Sample time\cr
#' sample_end_dt \tab Date \tab Sample end date\cr
#' sample_end_tm \tab character \tab Sample end time\cr
#' sample_start_time_datum_cd \tab character \tab Time zone datum\cr
#' tm_datum_rlbty_cd \tab character \tab Time datum reliability code\cr
#' coll_ent_cd \tab character \tab Collecting agency code\cr
#' medium_cd \tab character \tab Sample medium code\cr
#' tu_id \tab character \tab Taxonomic unit code\cr
#' body_part_id \tab character \tab Body part code\cr
#' parm_cd \tab character \tab Analyte parameter code\cr
#' remark_cd \tab character \tab Remark code for the result\cr
#' result_va \tab character \tab Numeric value of the result\cr
#' val_qual_tx \tab character \tab Result value qualifier code\cr
#' meth_cd \tab character \tab Lab method code\cr
#' dqi_cd \tab character \tab Data-quality indicator code\cr
#' rpt_lev_va \tab character \tab Reporting level\cr
#' rpt_lev_cd \tab character \tab Reporting level type\cr
#' lab_std_va \tab character \tab Lab standard deviation\cr
#' anl_ent_cd \tab character \tab Analyzing agency code\cr
#' } Note: all concentrations in the
#' column result_va are in milligrams per liter as phosphorus.\cr
#'
#' @source Data retrieved from NWISWeb
#' (\url{https://nwis.waterdata.usgs.gov/mn/nwis/qwdata}) on 2012-04-06. The
#' format of water-quality data is often retrieved by result--each row
#' represents the data for a single analyte. Note that many columns are part of
#' the generic data retrieval and do not pertain specifically to these data.
#' @keywords datasets
#' @examples
#' 
#' data(QWstacked)
#' # Plot the data for whole-water phosphorus
#' with(subset(QWstacked, parm_cd == "00665"), plot(sample_dt, result_va))
#' # Overlay the data for dissolved phosphorus (should be smaller values)
#' with(subset(QWstacked, parm_cd == "00666"), points(sample_dt, result_va, col='red'))
"QWstacked"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pcodeColData
#' @title Selected USGS parameter Codes
#'
#' @description Selected U.S. Gelogical Survey (USGS) parameter codes to create 
#' column names and data type conversion when importing data from NWISweb.
#' 
#' From swmrQW
#' 
#' @name pcodeColData
#' @docType data
#' @usage pcodeColData
#' @format Data frame with 1178 rows and 3 columns\cr
#'\tabular{lll}{
#'Name \tab Type \tab Description\cr
#'parm_cd \tab character \tab The 5-digit parameter code\cr
#'col_name \tab character \tab The column name to use when importing data from NWISweb\cr
#'data_type \tab character \tab The data type to convert the imported data\cr
#'}
#' @note This data set can be copied and modifed by the user to add or change column names.
#'The \code{data_type} must be either "qw" or "numeric" any other value will be ignored.
#' @references
#'Lorenz, D.L., 2016, smwrQW---An R Package for Managing and Analyzing 
#'Water-Quality Data, Version 0.7.3
#' @keywords datasets
#' @examples
#' data(pcodeColData)
#' # just print some rows
#' head(pcodeColData)
#' tail(pcodeColData)
"pcodeColData"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QW05078470
#' @title Stream Water Quality
#' 
#' @description Selected water-quality data for Judicial Ditch 64 near Mentor, Minnesota
#' (SW4), USGS station number 05078470 for calendar year 2003.
#' 
#' 
#' @name QW05078470
#' @docType data
#' @usage QW05078470
#' @format Data frame with 11 rows and 4 columns\cr
#' \tabular{lll}{ 
#' Name \tab Type \tab Description\cr\cr
#' DATES \tab Date \tab Sample date\cr
#' TIMES \tab character \tab Sample time\cr
#' R00665 \tab character \tab Remark code for total phosphorus concentration\cr
#' P00665 \tab numeric \tab Total phosphorus concentration\cr
#' } Note: all concentrations are in milligrams per liter as
#' phosphorus.\cr
#'
#' @source Data retrieved from NWISWeb
#' (\url{https://nwis.waterdata.usgs.gov/mn/nwis/qw}) on 2005-01-05.
#' @keywords datasets
#' @examples
#' 
#' data(QW05078470)
#' # Plot the data
#' with(QW05078470, plot(DATES, P00665, log='y'))
#' 
"QW05078470"
