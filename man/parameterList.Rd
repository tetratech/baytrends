\name{parameterList}
\alias{parameterList}
\docType{data}
\title{
Table: List of valid parameters for CBP trend analysis
}
\description{
List of valid parameters for CBP trend analysis
}
\usage{data("parameterList")}
\format{
  A data frame with the following 12 variables.
  \describe{
    \item{\code{parm}}{valid parameter code}
    \item{\code{parmSource}}{alternative parameter code}
    \item{\code{parmCat}}{Parameter category, e.g., "Core" includes dissolved oxygen, Secchi depth, corrected chlorophyll-a, total nitrogen, and total phosphorus}
    \item{\code{parmName}}{Parameter description (useful for graph labels)}
    \item{\code{parmCalc}}{Has the parameter been calculated}
    \item{\code{parmNamelc}}{Parameter description (lower case)}
    \item{\code{parmUnits}}{Parameter units}
    \item{\code{parmRO1}}{internal report order level 1}
    \item{\code{parmRO2}}{internal report order level 2}
    \item{\code{parmRecensor}}{value used for re-coding values <= 0}
    \item{\code{parmTrend}}{analyze variable for trends}
    \item{\code{logTrans}}{analyze variable using log transformation for GAM analysis}
    \item{\code{trendIncrease}}{Interpretation of an increasing concentration}
  }
}
\examples{
str(parameterList)
}
\keyword{datasets}
