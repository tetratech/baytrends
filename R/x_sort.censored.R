#'Sorting Vectors
#'
#'Sorts a vector into ascending or descending order: methods for "lcens" and "mcens"
#'data. This function sorts left-censored values immediately below the equivalent 
#'uncensored values and right-censored values immediately above the equivalent 
#'uncensored values. 
#'
#'@aliases sort.lcens sort.mcens
#'@param x the data to be sorted.
#'@param decreasing sort the data in descending order?
#'@param na.last for controlling the treatment of \code{NA}s. If \code{TRUE},
#'then missing values in the data are put last; if \code{FALSE}, then they are
#'put first; if \code{NA}, then they are removed.
#'@param \dots arguments to be passed to or from methods.
#'@return An object like \code{x}, but with the data sorted. Left-censored
#'values are placed between smaller numeric values and the corresponding
#'uncensored values.
#'@keywords manip
#'@examples
#'set.seed(5422)
#'XR <- rnorm(10)
#'sort(as.lcens(XR, rep(c(0, -1), each=5))) # censors at 0 and -1
#'
#' @rdname sort.censored
#' @export
#' @method sort lcens
sort.lcens <- function(x, decreasing=FALSE, na.last=NA, ...) {
  ## Coding history:
  ##    2012Mar18 DLLorenz Original coding for separate sort function, from censpp
  ##    2013Jan07 DLLorenz Roxygenized, completely rewritten and added .mcens
  ##    2013Jan07          This version
  ## 
  cens <- x@censor.codes
  nums <- x@.Data[, 1L]
  ord <- order(nums, -cens, na.last=na.last, decreasing=decreasing)
  return(x[ord])
}

#' @rdname sort.censored
#' @export
#' @method sort mcens
sort.mcens <- function(x, decreasing=FALSE, na.last=TRUE, ...) {
  nums1 <- x@.Data[, 1L]
  nums2 <- x@.Data[, 2L]
  cens <- x@censor.codes
  nums <- ifelse(x@interval, (nums1 + nums2)/2,
                 ifelse(cens == 1L, nums1, nums2))
  ord <- order(nums, cens, na.last=na.last, decreasing=decreasing)
  return(x[ord])
}
