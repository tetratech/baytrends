#'Character Representations
#'
#'Creates character representations of censored and water-quality data: 
#'methods for "lcens," "mcens," and "qw" data.  
#'Removed ATimport smwrBase (after keywords and before export)
#'
#'@rdname as.character.lcens
#'@aliases as.character.lcens as.character.mcens as.character.qw
#'@param x any R object to coerce to character
#'@param \dots further arguments passed to or from other methods.
#'@return Creates a character representation of \code{x}.
#'@keywords internal manip
#'@export
#'@method as.character lcens
as.character.lcens <- function(x, ...) {
  vals <- as.character(x@.Data[, 1])
  rmks <- ifelse(x@censor.codes, "<", " ")
  rmks <- ifelse(is.na(rmks), " ", rmks) # Fix NANA in output
  rmks <- ifelse(is.na(vals), " ", rmks) # Fix <NA in output
  return(paste(rmks, vals, sep=''))
}

#'@export
#'@method as.character mcens
#'@rdname as.character.lcens
as.character.mcens <- function(x, ...) {
  ## These are the possibilities in this order
  ## NA results in as.character(NA)
  ## min = max results in min value
  ## min = -Inf and max = inf results in "undefined"
  ## min = -Inf results in < max
  ## max = Inf results in > min
  ## min < max results in min - max
  ##
  retval <- paste(x@.Data[, 1L], " - ", x@.Data[, 2L], sep='')
  sel <- which(x@censor.codes == 0L & !x@interval) # Only change uncensored
  retval[sel] <- as.character(x@.Data[sel, 1L])
  sel <- which(x@censor.codes == -1L)
  retval[sel] <- paste("<", as.character(x@.Data[sel, 2L]), sep='')
  sel <- which(x@censor.codes == 1L)
  retval[sel] <- paste(">", as.character(x@.Data[sel, 1L]), sep='')
  if(any(sel <- (x@.Data[, 1L] == -Inf) & (x@.Data[, 2L] == Inf), na.rm=TRUE))
    retval[which(sel)] <- "undefined"
  if(any(sel <- is.na(x)))
    retval[which(sel)] <- NA
  return(retval)
}

#'@export
#'@method as.character qw
#'@rdname as.character.lcens
as.character.qw <- function(x, ...) {
  rnd <- x@rounding
  xval <- round(signif(x@.Data,rnd[1]), rnd[2])
  xrmk <- x@remark.codes
  ## Interval censoring
  xsho <- ifelse(xrmk == "I", xval[, 2L], xval[, 1L])
  xrmk <- ifelse(xrmk == "I", as.character(xval[, 1L]), xrmk)
  ## Left censoring
  xsho <- ifelse(xrmk == "<", xval[, 2L], xsho)
  ## Show indicator of outside range
  xstr <- ifelse(xrmk == "<" & x@reporting.level < xsho, "!", " ")
  xstr <- ifelse(is.na(x@reporting.level), "*", xstr)
  xstr <- na2miss(xstr, " ")
  xvc <- x@value.codes
  # strip additional info if xsho is NA
  xrmk[is.na(xsho)] <- " "
  xstr[is.na(xsho)] <- ""
  xvc[is.na(xsho)] <- ""
  return(paste(xrmk, xsho, xstr, xvc, sep=' '))
}
