#'Impute Detection Limits
#'
#'Imputes values for missing detection limits for left-censored data.  From smwrQW package.
#'
#'
#'@param values the numeric values.
#'@param censor.codes logical or character, \code{TRUE} or "<" indicates
#'left-censored at value and \code{FALSE}, " " or "" indicates a quantified
#'value. Character codes "E" and "J" are censored at the current detection
#'limit. Other characters are ignored and generate a warning.
#'@param default the default detection limit value to assign to an uncensored
#'value that is less than the currently imputed detection limit value.
#'@return A vector of detection limits matched with each of the input values.
#'@note This function is based on the DLIMIT function in LOADEST (Runkel and
#'others, 2004). The general assumption for imputing detection limits is that
#'the data are sorted by time and the detection limits changes in a consistent
#'pattern over time.
#'@section Warning: Tim Cohn, written communication, 5 Nov 2002 states that
#'there are several problems with this approach:\cr
#'
#'1) for large data sets, it may be difficult for the user to group the
#'observations into subsets as in the example above.  If the observations are
#'not grouped into subsets or the subsets do not have the censored observations
#'as the first records, incorrect detection limits may be applied to the
#'uncensored data.\cr
#'
#'2) for the case of multiple constituents, it may be impossible to place the
#'censored observation within each subset at the top of the subset.  Consider
#'constituents Y and Z, each analyzed at Lab B, and each with one censored
#'observation.  The censored observations occur on different dates.  In this
#'case its not possible to have both observations as the first line in the
#'subset from Lab B. (Note: the user could work around this problem by doing
#'seperate runs for each constituent.)\cr
#'
#'3) When a subset of observations is completely uncensored, the detection
#'limit from another subset will be applied.  Consider the example above, but
#'with the 10 observations from Lab B being completely uncensored.  In this
#'case, the detection limit from Lab A will be used for all 20 observations.\cr
#'
#'4) When all of the observations are uncensored (no '<' signs for a given
#'constituent), there is no way for the user to specify the detection limit(s).
#'In this case, a default value of 1.E-25 is used for all observations
#'(consistent with ESTIMATOR).\cr
#'
#'Despite these problems, the approach is satisfactory for most applications:
#'"the estimates are not very sensitive to the precise value of the censoring
#'threshold for the above-threshold values."
#'@seealso \code{\link{readNWQLdl}}
#'@references Runkel, R.L., Crawford, C.G., and Cohn, T.A., 2004, Load
#'estimator (LOADEST) a FORTRAN program for estiamting constituent laods in
#'streams and rivers: U.S. Gelogical Survey Techniques and Methods 4-A5, 69.
#'Available online as \url{https://pubs.er.usgs.gov/publication/tm4A5}.
#'@keywords internal manip
#'@examples
#'
#'## The actual detection limits are 2,2,2,1,1,1.
#'dlimit(c(2,2,3,1,1,2), c(" ", "<", " ", " ", "<", " "))
#'
#'@export
dlimit <- function(values, censor.codes, default=1.e-25) {
  ## Coding history:
  ##    2012Sep17 DLLorenz original Coding
  ##    2013Jan01 DLLorenz Roxygenized
  ##    2013Jan01          This version
  ##
  if(is.logical(censor.codes)) {
    ## Based on the DLIMIT function in LOADEST:
    ## 1. If there are no censored values, then the detection limit is 1x10^-25.
    ## 2. If there are censored values, then the data are assumed to be in
    ##    sequential order and detection limit is set as the value of the
    ##    most recent censored value, backfilling from the first censored
    ##    observation to the first observation in the dataset. If any uncensored
    ##    concentration is less than the detection limit of the most recent
    ##    censored value, then the detection limit is set to 1x10^-25.
    ##
    if(any(censor.codes)) {
      dl <- values[which(censor.codes)[1]] # this is the first in the sequence
      detlim <- rep(dl, length(values))
      for(i in seq(along=values)) {
        if(is.na(values[i]))
          detlim[i] <- NA
        else if(censor.codes[i]) { # assume that never NA if get to here
          dl <- values[i]
          detlim[i] <- dl
        }
        else # uncensored
          detlim[i] <- if(dl <= values[i]) dl else default
      }
    }
    else
      detlim <- rep(default, length(values))
  }
  else {
    ## Based on the DLIMIT function in LOADEST:
    ## See censor.codes="logical" for explanation.
    ## Except that censor.codes == "E" or "J" get censored at dl rather than
    ## changing the DL
    cc <- unique(censor.codes)
    bad <- which(!(cc %in% c("<", "", " ", "E", "J")))
    if(any(bad))
      warning(paste("censor.codes ignored:", cc[bad], collapse=" "))
    if(any(censor.codes == "<")) {
      dl <- values[which(censor.codes=="<")[1]] # this is the first in the seq
      detlim <- rep(dl, length(values))
      for(i in seq(along=values)) {
        if(is.na(values[i]))
          detlim[i] <- NA
        else if(censor.codes[i] == "<") { # assume that never NA if get to here
          dl <- values[i]
          detlim[i] <- dl
        }
        else if(censor.codes[i] %in% c("E", "J")) {
          if(values[i] < dl) { # Assume "estimated" value below DL
            values[i] <- dl
            detlim[i] <- dl
            censor.codes[i] <- "<"
          }
          else # Assume "estimated" value above DL
            detlim[i] <- dl
        }
        else # not censored
          detlim[i] <- if(dl <= values[i]) dl else default
      }
    }
    else
      detlim <- rep(default, length(values))
  }
  return(detlim)
}
