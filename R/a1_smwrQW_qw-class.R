#' @title Water-quality Data
#'
#' @description Class "qw" describes water-quality data.
#' From smwrQW package.
#'
#' @name qw-class
#' @rdname qw-class
#' @slot .Data a 2-column matrix that contains the lower and upper limit of the value
#'of the water-quality data. Missing values are permitted.
#' @slot remark.codes a character vector of remark codes. Only "<" and ">" are meaningful
#'for the class; other values will be ignored, but may generate warnings.
#'Missing values are permitted only for corresponding missing values in \code{.Data}.
#' @slot value.codes a character vector for user-defined code that may help characterize
#'the value of the water-quality data.
#'Missing values are permitted only for corresponding missing values in \code{.Data}.
#' @slot reporting.level a numeric vector describing the reporting level of the
#'water-quality data. Missing values are permitted if the reporting level is not known.
#' @slot reporting.method a character vector for user-defined code or description of
#'the \code{reporting.level}. Generally used to distinguish between detection limits and
#'quantitation levels.
#' @slot reporting.units a character vector describing the units. Some values, like "mg/L"
#'are recognized by some applications.
#' @slot analyte.method a character vector to describe the analytical method.
#' @slot analyte.name a character vector of the analyte name. Some values may be recognized
#'by some applications.
#' @slot rounding a vector of length 2 that describes how to round the data for printing.
#'The first value is the number of significant digits, the second is the maximum number of 
#'decimal places, default is c(2,3).
#' @slot unique.code a character vector that defines a distinct collection of data. Typically 
#'used to define columns is datasets organized by sample.
#' @slot names a character vector of unique names for each element in \code{.Data}.
#' @section Objects from the Class: Objects can be created by calls to
#'\code{as.qw}.
#' @references Lorenz, D.L., in preparation
#' @keywords internal classes
#' @exportClass qw
#' @examples
#'
#'showClass("qw")
#'
setClass("qw", slots=list(remark.codes="character", value.codes="character",
                              reporting.level="numeric", reporting.method="character",
                              reporting.units="character", analyte.method="character",
                              analyte.name="character", rounding="numeric",
                              unique.code="character", names="character"),
         contains="matrix")
