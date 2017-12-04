\name{dataCensored}
\alias{dataCensored}
\docType{data}
\title{
Table: Sample Chesapeake Bay Program Data, 1985-2015
}
\description{
Selected data from the CBP. Water quality variables are stored as class qw (see package ?smwrQW)
}
\usage{data("dataCensored")}
\format{
  A data frame with the following 16 variables.
  \describe{
    \item{\code{station}}{station}
    \item{\code{date}}{sample date}
    \item{\code{layer}}{sample layer}
    \item{\code{secchi}}{Secchi depth [m]}
    \item{\code{chla}}{Corrected Chlorophyll-a [ug/L]}
    \item{\code{do}}{Dissolved Oxygen [mg/L]}
    \item{\code{tn}}{Total Nitrogen [mg/L]}
    \item{\code{tp}}{Total Phosphorus [mg/L]}
    \item{\code{po4f}}{Orthophosphorus [mg/L]}
    \item{\code{pp}}{Particulate Phosphorus [mg/L]}
    \item{\code{tdp}}{Total Dissolved Phosphorus [mg/L]}
    \item{\code{no23f}}{Nitrite + Nitrate (Filtered) [mg/L]}
    \item{\code{nh4f}}{Ammonium (Filtered) [mg/L]}
    \item{\code{tdn}}{Total Dissolved Nitrogen [mg/L]}
    \item{\code{pn}}{Particulate Organic Nitrogen and Particulate Nitrogen [mg/L]}
    \item{\code{tss}}{Total Suspended Solids [mg/L]}
  }
}
\examples{
data(dataCensored)
}
\keyword{datasets}
