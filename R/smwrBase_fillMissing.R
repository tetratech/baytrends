#'Fill Missing Values
#'
#'Replace missing values in time-series data by interpolation.
#'
#'Missing values at the beginning and end of \code{x} will not be replaced. \cr
#'
#'The argument \code{span} is used to help set the range of values used to
#'construct the \code{StructTS} model. If \code{span} is set small, then the
#'variance of epsilon dominates and the estimates are not smooth. If
#'\code{span} is large, then the variance of level dominates and the estimates
#'are linear interpolations. The variances of level and epsilon are components
#'of the state-space model used to interpolate values, see \code{\link{StructTS}} for details. 
#'See \bold{Note} for more information about the method.\cr
#'
#'If \code{span} is set larger than 99, then the entire time series is used to
#'estimate all missing values.  This approach may be useful if there are many
#'periods of missing values. If \code{span} is set to any number less than 4,
#'then simple linear interpolation will be used to replace missing values.
#'
#' Added from smwrBase.
#'
#' @param x the sequence of observations. Missing values are permitted and will
#'be replaced.
#' @param span the maximum number of observations on each side of each range of
#'missing values to use in constructing the time-series model. See
#'\bold{Details}.
#' @param Dates an optional vector of dates/times associated with each value 
#'in \code{x}. Useful if there are gaps in dates/times.
#' @param max.fill the maximum gap to fill.
#' @return The observations in \code{x} with missing values replaced by
#'interpolation.
#' @note The method used to interpolate missing values is based on
#'\code{tsSmooth} constructed using \code{StructTS} on \code{x} with
#'\code{type} set to "trend." The smoothing method basically uses the
#'information (slope) from two values previous to missing values and the two
#'values following missing values to smoothly interpolate values accounting for
#'any change in slope. Beauchamp (1989) used time-series methods for synthesizing
#'missing streamflow records. The group that is used to define the statistics that
#'control the interpolation is very simply defined by \code{span} rather than
#'the more in-depth measures described in Elshorbagy and others (2000).
#'
#'If the data have gaps rather than missing values, then fillMissing will return
#'a vector longer than \code{x} if \code{Dates} is given and the return data
#'cannot be inserted into the original data frame. If \code{Dates} is not given,
#'then the gap will be recognized and not be filled. The function
#'\code{insertMissing} can be used to create a data frame with the complete
#'sequence of dates.
#' @seealso
#Flip for production/manual
#'\code{\link[stats]{tsSmooth}}, \code{\link[stats]{StructTS}}
#, \code{\link{insertMissing}}
#\code{tsSmooth} (in stats package), \code{StructTS} (in stats package), \code{\link{insertMissing}}
#' @references Beauchamp, J.J., 1989, Comparison of regression and time-series
#'methods for synthesizing missing streamflow records: Water Resources
#'Bulletin, v. 25, no. 5, p. 961--975.\cr
#'
#'Elshorbagy, A.A., Panu, U.S., and Simonovic, S.P., 2000, Group-based estimation
#'of missing hydrological data, I. Approach and general methodology:
#'Hydrological Sciences Journal, v. 45, no. 6, p. 849--866.
#' @export
#' @keywords internal manip
#' @examples
#'\dontrun{
#'#library(smwrData)
#'data(Q05078470)
#'# Create missing values in flow, the first sequence is a peak and the second is a recession
#'Q05078470$FlowMiss <- Q05078470$FLOW
#'Q05078470$FlowMiss[c(109:111, 198:201)] <- NA
#'# Interpolate the missing values
#'Q05078470$FlowFill <- fillMissing(Q05078470$FlowMiss)
#'# How did we do (line is actual, points are filled values)?
#'par(mfrow=c(2,1), mar=c(5.1, 4.1, 1.1, 1.1))
#'with(Q05078470[100:120, ], plot(DATES, FLOW, type="l"))
#'with(Q05078470[109:111, ], points(DATES, FlowFill))
#'with(Q05078470[190:210, ], plot(DATES, FLOW, type="l"))
#'with(Q05078470[198:201, ], points(DATES, FlowFill))
#'}
fillMissing <- function(x, span=10, Dates=NULL, max.fill=10) {
  ## Coding history:
  ##    2012May31 DLLorenz Initial version, based on tsSmooth
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2012Aug23 DLLorenz Break up into small time frames for better interp
  ##    2012Dec21 DLLorenz Added simple interpolation if span < 1
  ##    2013Jan10 DLLorenz Bug fix for short spans of nonmissing values
  ##    2013Feb02 DLLorenz Prep for gitHiub
  ##
  x <- as.numeric(x) # force to double
  ck <- sum(is.na(x))
  if(ck == 0L || ck == length(x))
    return(x) # Nothing to do
  ## Check on sequence gaps
  if(is.null(Dates))
    ckDt <- rep(0, length(x))
  else
    ckDt <- c(0, diff(as.numeric(Dates), differences=2), 0)
  ## Check on max fill
  fill.evnt <- eventNum(is.na(x), reset=TRUE)
  fill.len <- eventLen(fill.evnt)
  dont.fill <- fill.len > max.fill
  ## Add to max fill those missing values were there are gaps
  gap.evnt <- unique(fill.evnt[ckDt !=0 & fill.evnt != 0L])
  if(length(gap.evnt))
    dont.fill[fill.evnt %in% gap.evnt] <- TRUE
  if(span < 4) { # simple linear interpolation
    xseq <- seq(along=x)
    ## Set output to NA where dont.fill
    xseq[dont.fill] <- NA
    notna <- !is.na(x)
    return(approx(xseq[notna], x[notna], xout=xseq)$y)
  }
  ## Need to filter leading and trailing NA
  xlead <- double(0L)
  while(is.na(x[1L])) {
    xlead <- c(xlead, NA)
    x <- x[-1L]
  }
  xtail <- double(0L)
  while(is.na(x[length(x)])) {
    xtail <- c(xtail, NA)
    x <- x[-length(x)]
  }
  sel <- is.na(x)
  ## Rather than checking each fill period, we'll just reset the output
  ## Probably less effecient, but less disruptive to code
  if(any(sel)) {
    if(span < 99) {
      ## Break up each time period of missing values so that there
      ## is no more than span observations on each side of the missing
      ## period
      ToDo <- which(sel)
      GrpDo <- eventNum(sel, reset=TRUE)[ToDo]
      PkMn <- 1L
      N <- length(x)
      for(i in unique(GrpDo)) {
        Picks <- ToDo[GrpDo == i]
        PkMn <- max(PkMn, min(Picks) - span) # Original set from above
        PkMx <- min(max(Picks)+span, ToDo[GrpDo == (i+1)]-1L, N)
        xsub <- x[seq(PkMn, PkMx)]
        ## A sequence of at least 3 nonmissing values is required. The values
        ## must also be non constant.
        ## The first is a simple test that guarantees the minimim sequence,
        ## but will miss the combination of 3,1 or 1,3. The second captures
        ## that, but is much slower!
        if(diff(range(xsub, na.rm = TRUE)) > 0 &&
           (sum(!is.na(xsub)) > 4 ||
            max(rle(is.na(xsub))$lengths[c(1,3)])) > 2) {
          TS <- StructTS(xsub, type="trend")
          fill <- tsSmooth(TS)
          x[Picks] <- fill[Picks - PkMn + 1L, 1L]
        }
        else { # use linear interpolation
          xseq <- seq(along=xsub)
          notna <- !is.na(xsub)
          x[Picks] <- approx(xseq[notna], xsub[notna], xout=xseq[!notna])$y
        }
        PkMn <- max(ToDo[GrpDo == i]) + 1L
      }
    } else {
      fill <- tsSmooth(StructTS(x, type="trend"))
      x[sel] <- fill[sel, 1L]
    }
  }
  retval <- c(xlead, x, xtail)
  retval[dont.fill] <- NA
  return(retval)
}
