\name{stationMasterList}
\alias{stationMasterList}
\docType{data}
\title{
Table: Chesapeake Bay Program long-term monitoring stations
}
\description{
Chesapeake Bay Program long-term monitoring stations
}
\usage{data("stationMasterList")}
\format{
  A data frame with the following 14 variables.
  \describe{
    \item{\code{station}}{Water quality station code}
    \item{\code{state}}{State location}
    \item{\code{locationType}}{As identified in the state trend reports, whether this station is in the mainstem or tributary monitoring}
    \item{\code{waterbody}}{Location as identified in the CBP CIMS database}
    \item{\code{latitude}}{From the CBP CIMS database, and verified in a GIS map}
    \item{\code{longitude}}{From the CBP CIMS database, and verified in a GIS map}
    \item{\code{cbSeg92}}{Match to CB 92 Segmentation scheme (for 303d list)}
    \item{\code{usgsGageName}}{Manual match to a USGS fall-line monitoring station (See usgsGages$siteName)}
    \item{\code{usgsGageID}}{USGS station code (See usgsGages$siteNumber)}
    \item{\code{usgsGageMatch}}{Identifies how the USGS-to-TribStation match was made. station is in a small tributary }
    \item{\code{stationRO1}}{Station sort order level 1}
    \item{\code{stationRO2}}{Station sort order level 2}
    \item{\code{stationGrpName}}{Station group header}
    \item{\code{stationMethodGroup}}{Station method group}
    \item{\code{hydroTerm}}{For gam models with flw_sal term, should model use "flow" or "salinity" in the model as the default}
    \item{\code{flwAvgWin}}{For gam models where "flow" is used for flw_sal term, what are the averaging periods to use}
    \item{\code{flwParms}}{For gam models with flw_sal term, which parameters should use "flow" for hydroTerm despite the the value of hydroTerm}
    \item{\code{salParms}}{For gam models with flw_sal term, which parameters should use "salinity" for hydroTerm despite the the value of hydroTerm}
    \item{\code{notes}}{notes about when row was updated}
  }
}
\examples{
str(stationMasterList)
}
\keyword{datasets}
