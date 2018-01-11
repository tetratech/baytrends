#' @title Add Water-Quality Data
#'
#' @description Adds or subtracts water-quality data while trying to maintain the 
#'integrity of the data.
#'
#' @details The \dots arguments must not be named in the call. Only objects of class 
#'"qw" or "numeric" are allowed. To perform a subtraction, simply preceed the argument
#'with the unary - sign in the call. The first argument must be a positive valued 
#'object of class "qw."
#'
#' @include a1_smwrQW_qw-class.R
#' @param \dots any number of water-quality (class "qw") or numeric vectors. The length
#'of the water-quality data must all agree, but numeric values are replicated to the 
#'length of the water-quality data. See \bold{Details}.
#' @param analyte the name of the analyte, if not supplied, generated from \dots.
#' @param pcode the parameter code of the analyte or any othe unique identifier the user chooses.
#' @param gt0 logical, if \code{TRUE}, then force values to be strictly non-negative on 
#'subtraction, otherwise negative values are permitted.
#' @return An object of class "qw" that is the result of the requested operations.
#' @keywords internal 
#' @export add
add <- function(..., analyte, pcode="", gt0=TRUE) {
  ## Coding history:
  ##    2013May17 DLLorenz Original coding
  ##
  dots <- list(...)
  dotnames <- names(dots)
  if(is.null(dotnames)) {
    call <- as.list(match.call())[-1]
    call <- as.character(call)
    dotnames <- call[seq(length(dots))]
    dotsigns <- ifelse(substring(dotnames, 1L, 1L) == "-", "-", "+")
    dotnames[dotsigns == "-"] <- substring(dotnames[dotsigns == "-"], 2L)
  }
  else
    stop("The water-quality data must be unnamed")
  ## Start processing, use the first argument as the core object to be returned
  retval <- dots[[1L]]
  Len <- length(retval)
  if(class(retval)[1L] != "qw")
    stop("The first argument must be of class \"qw\"")
  if(dotsigns[1L] == "-")
    stop("The first argument must be treated as positive")
  dotsigns[1L] <- "" # For constructing analyte if necessary
  for(i in seq(2L, length(dotnames))) {
    toadd <- dots[[i]]
    if(class(toadd)[1L] == "numeric") {
      toadd <- rep(toadd, length.out=Len)
      ## Numerics are properly converted if negative on call
      LTs <- retval@remark.codes == "<"
      retval@.Data[!LTs, 1L] <- retval@.Data[!LTs, 1L] + toadd[!LTs]
      retval@.Data[, 2L] <- retval@.Data[, 2L] + toadd
      if(gt0) {
        retval@.Data[, 1L] <- pmax(retval@.Data[, 1L], 0)
        retval@.Data[, 2L] <- pmax(retval@.Data[, 2L], 0)
      }
      # should not change the RL: retval@reporting.level <- retval@reporting.level  + toadd
    }
    else if(class(toadd)[1L] == "qw") {
      if(dotsigns[i] == "+") {
        retval@.Data[, 1L] <- retval@.Data[, 1L] + toadd@.Data[, 1L]
        retval@.Data[, 2L] <- retval@.Data[, 2L] + toadd@.Data[, 2L]
        retval@reporting.level <- retval@reporting.level  + toadd@reporting.level
        ## Fix censor codes
        retval@remark.codes[retval@remark.codes == "<" & toadd@remark.codes != "<"] <- ""
        retval@remark.codes[toadd@remark.codes == ">"] <- ">"
      } else { # do subtraction--@.Data are negated  at call
        retval@.Data[, 1L] <- retval@.Data[, 1L] + toadd@.Data[, 2L]
        retval@.Data[, 2L] <- retval@.Data[, 2L] + toadd@.Data[, 1L]
        if(gt0) {
          retval@.Data[, 1L] <- pmax(retval@.Data[, 1L], 0)
          retval@.Data[, 2L] <- pmax(retval@.Data[, 2L], 0)
        }
        ## Fix censor codes
        retval@remark.codes[toadd@remark.codes == ">"] <- "<"
        retval@remark.codes[retval@.Data[,1L] == 0] <- "<"
        retval@remark.codes[retval@.Data[,1L] > 0 & (retval@.Data[,2L] - retval@.Data[,1L] > 1e-9)] <- "I"
      }
    }
    else
      stop("All arguments must be of class \"qw\" or \"numeric.\"")
  }
  if(missing(analyte)) # Construct analyte name
    analyte <- paste(paste(dotsigns, dotnames, sep=""), collapse="")
  retval@analyte.name <- rep(analyte, length.out=Len)
  retval@unique.code <- rep(pcode, length.out=Len)
  return(retval)
}
