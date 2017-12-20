#'Summarize Data
#'
#'Provide a summary of censored or water-quality data.
#'
#'
#'@rdname summary
#'@aliases summary.lcens summary.mcens summary.qw
#'@param object the object to summarize.
#'@param details provide details on multiple entries?
#'@param \dots further arguments passed to other methods.
#'@return For \code{details} = \code{FALSE}, a named character vector describe
#'some characteristics of \code{object}. For \code{details} = \code{TRUE}, a
#'named list containing vectors with all unique values.
#'@note This function is used when \code{summary} is called on a data frame.
#'Setting \code{details} to \code{TRUE} can be useful for individual columns in
#'a data frame.
#'@keywords misc
#'@examples
#'
#'\dontrun{
#'# Summrize these data
#'summary(importNWISqw("05330000", "00608"))
#'}
#'
#' @export
#'@method summary lcens
summary.lcens <- function(object, ...) {
  ## Arguments:
  ##  object - an object of class "lcens"
  ##  ... dots
  ##
  ## Return a named vector of summary information
  Len <- length(object)
  Cen <- censoring(object)
  Mss <- sum(is.na(object))
  return(c(Length=Len, Nobs=Len-Mss, Class="lcens", Censor=Cen))
}

#'@rdname summary
#' @export
#'@method summary mcens
summary.mcens <- function(object, ...) {
  ## Arguments:
  ##  object - an object of class "mcens"
  ##  ... dots
  ##
  ## Return a named vector of summary information
  Len <- length(object)
  Cen <- censoring(object)
  Mss <- sum(is.na(object))
  return(c(Length=Len, Nobs=Len-Mss, Class="mcens", Censor=Cen))
}

#'@rdname summary
#' @export
#'@method summary qw
summary.qw <- function(object, details=FALSE, ...) {
  ## Arguments:
  ##  object - an object of class "qw"
  ##  details, return a detailed list instead of named vector
  ##  ... dots
  ##
  Len <- length(object)
  Cen <- censoring(object)
  Mss <- sum(is.na(object))
  SRS <- unique(object@analyte.name)
  SRS <- SRS[!is.na(SRS)]
  if(length(SRS) > 1 && !details)
    SRS <- "many"
  Unt <- unique(object@reporting.units)
  Unt <- Unt[!is.na(Unt)]
  if(length(Unt) > 1 && !details)
    Unt <- "mixed"
  Mth <- unique(object@analyte.method)
  Mth <- Mth[!is.na(Mth)]
  if(length(Mth) > 1 && !details)
    Mth <- "many"
  if(details) {
    Rls <- unique(object@reporting.method)
    Rls <- Rls[!is.na(Rls)]
    return(list(Length=Len, Nobs=Len-Mss, Analyte=SRS, Censor=Cen,
                Units=Unt, Method=Mth, RL.method=Rls))
  }
  return(c(Length=Len, Nobs=Len-Mss, Analyte=SRS, Censor=Cen,
                Units=Unt, Method=Mth))
}
