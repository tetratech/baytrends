#' @title Mutliply-censored Data
#'
#' @description Class "mcens" describes right- or multiply-censored data.
#' From smwrQW package.
#'
#' @name mcens-class
#' @rdname mcens-class
#' @slot .Data a 2-column matrix that contains the lower and upper value for the data.
#'Missing values are permitted.
#' @slot censor.codes an integer vector that indicates censoring; -1 indicates 
#'left-censored, 0 indicates uncensored, and 1 indicates rigth-censored.
#'Missing values are permitted only for elements of \code{.Data} that are missing.
#' @slot interval a logical value; \code{TRUE} indicates that the value is 
#'interval censored and \code{FALSE} indicates otherwise.
#'Missing values are permitted only for elements of \code{.Data} that are missing.
#' @slot names a character vector of unique names for each element in \code{.Data}.
#' @note Note that \code{censor.codes} and \code{interval} are not necessary, but
#'included to make some manipulations easier.
#' @section Objects from the Class: Objects can be created by calls to
#'\code{as.mcens}.
#' @references Lorenz, D.L., in preparation
#' @keywords internal classes
#' @exportClass mcens
#' @examples
#'
#'showClass("mcens")
#'
setClass("mcens", slots=list(censor.codes="integer", interval="logical",
                                 names="character"),
         contains="matrix")
