#' @title Water-quality Data Conversion
#'
#' @description Converts data and meta data to a water-quality data (\code{qw}) object:
#'various methods.
#'From smwrQW package.
#'
#' @details Two methods for constructing water-quality data are defined in the current
#'version. The first method works well for un-, left-, and right-censored data where the 
#'remark code and value can completely define the numeric values. The second method works
#'for any censored data, including interval-censored data, where an upper and lower
#'range of values are required. For the second method, \code{values} is the lower limit
#'of the range and for left-censored data, may be either \code{-Inf}, \code{NA}, or \code{0}.
#'If \code{-Inf}, then the value is treated as strictly left-censored, otherwise, the 
#'lower limit is set to \code{0}, which corresponds to the traditional interpretation of
#'left-censored water-quality data. For the second method, \code{value2} is the upper
#'limit and for right-censored data will always be set to \code{Inf}.
#'
#' @name as.qw
#' @rdname as.qw
#' @include a1_smwrQW_qw-class.R
#' @param values the uncensored values and optionally the censored values.
#' @param remark.codes the remarks codes: " " or "" indicated uncensored, "<"
#'indicates left-censoring, ">" indicates right censoring. The special remark
#'code "M" is created when the value is rounded to 0 by NWIS. In this case, the value 
#'is set to less than the reporting level if the reporting level is not the missing
#'value \code{NA}, otherwise the value is set to missing \code{NA} and "M" is
#'retained as the remark code. Other codes have a
#'special meaning that can be modified by \code{value.codes}.
#' @param value.codes special value qualifier codes. See Lorenz and others, 2014
#'for a description of these codes and the special \code{remark.codes}.
#' @param reporting.level the reporting level associated with each value. Note that
#'if the reporting level is not known, then \code{NA_real_} should be used instead
#'of using any arbitrary value.
#' @param reporting.method the method used to compute the reporting level. These
#'can be arbitrary codes, known only to the user.
#' @param reporting.units the reporting units. A few defined units such as
#'"mg/l" are recognized and used by the software.
#' @param analyte.method the method used to compute the concentration. These can
#'be arbitrary codes, known only to the user.
#' @param analyte.name the name of the analyte. A few names are recongnized such
#'as "chloride" and used by the software.
#' @param unique.code an arbitrary code identifying unique instances of data.
#'For data retreived from the US Geological Survey, this should be the 5-digit
#'parameter code.
#' @param value2 for interval censored data, the upper value.
#' @return An object of class "qw" that contains the numeric data in a
#'two-column matrix and the meta data as additional slot information.
#' @note The class "qw" is intended to be a flexible and user-friendly
#'way to store and manage water-quality data. The meta data that are stored
#'with the numeric and remark data are essential for unbiased statistical
#'analysis and for reducing other errors in interpretation or computation.\cr
#'
#' Objects of class "qw" also have a slot named "rounding" that controls how the 
#'data are roudend when printed. That slot is an integer vector of length 2, the
#'first element is the maximum number of significant figures and the second is the
#'maximum number of decimal places to show. The default value used by \code{as.qw}
#'is c(2L,3L). It can be changed by specifically modifying the slot value.
#'
# @seealso \code{\link{importNWISqw}}
#' @references Lorenz, D.L., in preparation
#' @exportMethod as.qw
#' @keywords internal methods manip
setGeneric("as.qw", function(values, remark.codes, value.codes, reporting.level,
                             reporting.method, reporting.units, analyte.method,
                             analyte.name, unique.code, value2)
  standardGeneric("as.qw")
  ## Arguments:
  ##  values (numeric vector) the numeric data
  ##  remark.codes (character vector) the remark code
  ##  value.codes (character vector) the value qualifier codes
  ##  reporting.level (numeric vector)the reporting level associated with each value
  ##  reporting.method (character vector) the reporting level type
  ##  reporting.units (character vector) the units of the values
  ##  analyte.method (character vector) the method used to compute the value
  ##  analyte.name (character vector) the name of the analyte
  ##  unique.code (character vector) any code (like parameter code) for identifying a
  ##    unique group of data
  ##  value2 (numeric vector) the numeric values associated with interval censored data
)

#' @rdname as.qw
#' @keywords internal 
setMethod("as.qw", signature(values="numeric", remark.codes="character",
                             value.codes="character",
                             reporting.level="numeric", reporting.method="character",
                             reporting.units="character", analyte.method="character",
                             analyte.name="character", unique.code="character", value2="missing"),
          function(values, remark.codes, value.codes, reporting.level, reporting.method,
                   reporting.units,analyte.method, analyte.name, unique.code, value2) {
            ## Length of remark.codes must match length of values
            N <- length(values)
            if(length(remark.codes) != N)
              stop("lengths of values and remark.codes must match")
            ## In practical terms, it is conceivable that single values could be
            ##  supplied by the rest
            value.codes <- rep(value.codes, length.out=N)
            reporting.level <- rep(reporting.level, length.out=N)
            reporting.method <- rep(reporting.method, length.out=N)
            reporting.units <- rep(reporting.units, length.out=N)
            analyte.method <- rep(analyte.method, length.out=N)
            analyte.name <- rep(analyte.name, length.out=N)
            unique.code <- rep(unique.code, length.out=N)
            value2 <- values
            ## Logic check on remarks--only "<", ">", and " " make sense
            remark.codes[is.na(remark.codes)] <- " "
            remark.codes[remark.codes == ""] <- " "
            ## Special check on remark code "M"
            if(any(pickM <- remark.codes == "M")) { # recode to < RL if possible
              picks <- !is.na(reporting.level) & pickM
              if(any(picks)) {
                remark.codes[picks] <- "<"
                value2[picks] <- reporting.level[picks]
                values[picks] <- 0
                warning("Special remark code M converted to less-than value")
              }
            }
            remarks.uniq <- unique(remark.codes)
            remarks.ignore <- !(remarks.uniq %in% c("<", ">", " "))
            if(any(remarks.ignore))
              warning("Special remark.codes: ", paste(remarks.uniq[remarks.ignore], collapse=', '),
                      " retained, but may require interpretation by the user.")
            ## Modify value2 for > and values for <
            value2[remark.codes == ">"] <- Inf
            values[remark.codes == "<"] <- 0
            ## Change lower limit if logged--will this ever happen?
            LogVal <- grepl("^log\\(", reporting.units)
            if(any(LogVal))
              values[remark.codes == "<" & LogVal] <- 0
            ## Pack it up and ship it off
            mat <- cbind(values=values, value2=value2)
            retval <- new("qw", mat, remark.codes=remark.codes,
                          value.codes=value.codes,
                          reporting.level=reporting.level,
                          reporting.method=reporting.method,
                          reporting.units=reporting.units,
                          analyte.method=analyte.method, analyte.name=analyte.name,
                          unique.code=unique.code, rounding=c(2L,3L),
                          names=as.character(seq(N)))
            # Not sure why necessary
            retval@names <- as.character(seq(N))
            return(retval)
          })

#' @rdname as.qw
#' @keywords internal 
setMethod("as.qw", signature(values="numeric", remark.codes="character",
                             value.codes="character",
                             reporting.level="numeric", reporting.method="character",
                             reporting.units="character", analyte.method="character",
                             analyte.name="character", unique.code="character", 
                             value2="numeric"),
          function(values, remark.codes, value.codes, reporting.level, reporting.method,
                   reporting.units,analyte.method, analyte.name, unique.code, value2) {
            ## Length of remark.codes must match length of values
            N <- length(values)
            if(length(remark.codes) != N)
              stop("lengths of values and remark.codes must match")
            if(length(value2) != N)
              stop("lengths of values and value2 must match")
            ## In practical terms, it is conceivable that single values could be
            ##  supplied by the rest
            value.codes <- rep(value.codes, length.out=N)
            reporting.level <- rep(reporting.level, length.out=N)
            reporting.method <- rep(reporting.method, length.out=N)
            reporting.units <- rep(reporting.units, length.out=N)
            analyte.method <- rep(analyte.method, length.out=N)
            analyte.name <- rep(analyte.name, length.out=N)
            unique.code <- rep(unique.code, length.out=N)
            ## Logic check on remarks--only "<", ">", and " " make sense
            remark.codes[is.na(remark.codes)] <- " "
            remark.codes[remark.codes == ""] <- " "
            remarks.uniq <- unique(remark.codes)
            remarks.ignore <- !(remarks.uniq %in% c("<", ">", " "))
            if(any(remarks.ignore))
              warning("Special remark.codes: ", paste(remarks.uniq[remarks.ignore], collapse=', '),
                      " retained, but may require interpretation by the user.")
            ## Modify value2 for > and values for <
            value2[remark.codes == ">"] <- Inf
            values[remark.codes == "<" & !is.finite(values)] <- 0
            ## Change lower limit if logged--will this ever happen?
            LogVal <- grepl("^log\\(", reporting.units)
            if(any(LogVal))
              values[remark.codes == "<" & LogVal] <- 0
            ## Pack it up and ship it off
            mat <- cbind(values=values, value2=value2)
            retval <- new("qw", mat, remark.codes=remark.codes,
                          value.codes=value.codes,
                          reporting.level=reporting.level,
                          reporting.method=reporting.method,
                          reporting.units=reporting.units,
                          analyte.method=analyte.method, analyte.name=analyte.name,
                          unique.code=unique.code, rounding=c(2L,3L),
                          names=as.character(seq(N)))
            # Not sure why necessary
            retval@names <- as.character(seq(N))
            return(retval)
          })
