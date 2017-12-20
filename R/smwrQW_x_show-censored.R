#' @title Show Methods for \code{WSqw} objects
#'
#' @description Prints the object.
#'
#' @include a3_smwrQW_mcens-class.R a2_smwrQW_lcens-class.R a1_smwrQW_qw-class.R
#' @name show-censored
#' @param object the object to show.
#' @return The object is retruned invisibly.
#' @keywords methods manip
#' @exportMethod show

#' @rdname show-censored
#' @aliases show,lcens-method
setMethod("show",  "lcens", function(object) {
  if(length(object)) {
    vals <- as.character(signif(object@.Data[, 1], 4))
    rmks <- ifelse(object@censor.codes, "<", " ")
    rmks <- ifelse(is.na(rmks), " ", rmks) # Fix NANA in output
    dl <- as.character(signif(object@.Data[, 2], 4))
    xtemp <- cbind(Values=paste(rmks, vals, sep=""), Limits=dl)
    ## make rownames that look like they would work for extraction
    rownames(xtemp) <- paste("[", seq(along=dl), "]", sep="")
    print(xtemp, quote=FALSE)
  } else
    cat("lcens(0)\n")
  invisible(object) }
)

#' @rdname show-censored
#' @aliases show,mcens-method
setMethod("show",  "mcens", function(object) {
  if(length(object)) {
    Lower <- as.character(signif(object@.Data[, 1L], 4))
    Upper <- as.character(signif(object@.Data[, 2L], 4))
    xtemp <- cbind(Lower=Lower, Upper=Upper)
    ## make rownames that look like they would work for extraction
    rownames(xtemp) <- paste("[", seq(along=Lower), "]", sep="")
    print(xtemp, quote=FALSE)
  } else
    cat("mcens(0)\n")
  invisible(object) }
)

#' @rdname show-censored
#' @aliases show,qw-method
setMethod("show", "qw", function(object) {
  if(length(object)) {
    xval <- object@.Data
    xrmk <- object@remark.codes
    xval[, 1L] <- ifelse(xrmk == "<", 0, xval[, 1L])
    xval <- matrix(as.character(signif(xval, 4L)), ncol=2L) # need to force back to matrix
    xval[, 2L] <- ifelse(xrmk == ">", "+", xval[, 2L])
    xval[, 1L] <- ifelse(xval[, 1L] == xval[, 2L], " ", xval[, 1L]) # make it easy to read
    xval <- format(xval)
    ## Make rownames that look like they would work for extraction
    rownames(xval) <- paste("[", seq(nrow(xval)), "]", sep="")
    colnames(xval) <- c("Lower", "Upper")
    ## Append values codes if not all blank
    xvc <- object@value.codes
    if(!all(xvc %in% c("", " ")))
      xval <- cbind(xval, Codes=xvc)
    xnm <- object@analyte.name
    if(length(unique(xnm)) > 1L)
      xval <- cbind(xval, Analyte=xnm)
    print(xval, quote=FALSE)
  } else
    cat("qw(0)\n")
  invisible(object) }
)
