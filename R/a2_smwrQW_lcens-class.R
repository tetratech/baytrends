#' @title Left-censored Data
#'
#' @description Class "lcens" describes left-censored data.
#' From smwrQW package.
#'
#' @name lcens-class
#' @rdname lcens-class
#' @slot .Data a 2-column matrix that contains the value and the detection limit
#'of the left-censored data. Missing values are permitted.
#' @slot censor.codes a logcial value, \code{TRUE} indicates a censored value, 
#'\code{FALSE} indates an uncensored value. Missing values are permitted only for
#'missing values in \code{.Data}.
#' @slot names a character vector of unique names for each element in \code{.Data}.
#' @section Objects from the Class: Objects can be created by calls to
#'\code{as.lcens}.
#' @references Lorenz, D.L., in preparation
#' @keywords classes
#' @exportClass lcens
#' @examples
#'
#'showClass("lcens")
#'
setClass("lcens", slots=list(censor.codes="logical", names="character"),
         contains="matrix")
