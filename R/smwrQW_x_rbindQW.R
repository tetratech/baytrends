#' Combine Data by Rows
#'
#' Combines a sequence of data frame arguments and combine by rows. This is a 
#'specialized version of rbind that works for data frames that contain columns 
#'of class "qw."
#'
#' @param \dots any number of data frames with identical columns. The missing value \code{NA}
#'is permitted as a special case to allow the addition of missing values.
#' @return A data frame with all columns combined in the order specified in \dots.
#' @keywords internal data
#' @seealso \code{\link{rbind}}
#'
#' @export
rbindQW <- function(...) {
  dots <- list(...)
  dots <- lapply(dots, as.data.frame)
  ## Expand columns of class qw
  dots <- lapply(dots, function(df) {
    lapply(names(df), function(col) {
      if(class(df[[col]])[[1L]] == "qw")
        as.data.frame(df[[col]], expand=TRUE, nm=col)
      else
        df[, col, drop=FALSE]
     } )
   } )
   dots <- lapply(dots, as.data.frame)
   ## Check for a single value appended (only NA)
   ckdots <- sapply(dots, length)
   if(any(ckdots == 1L)) {
     target <- dots[[1L]][1L,]
     for(i in which(ckdots == 1L))
       dots[[i]] <- as.data.frame(lapply(target, function(x) NA))
   }
   ## pack everything together and convert back to qw
   dots <- do.call(rbind, dots)
   return(convert2qw(dots, scheme="qw"))
}
