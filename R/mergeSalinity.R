############################
#' merge salinity into analysis data frame and update iSpec with variable name
#'
#' @param ct1 analysis data frame
#' @param iSpec iSpec
#'
#' @export
#' @importFrom stats cor
#'
.mergeSalinity <- function(ct1=ct1, iSpec=iSpec) {
# -----< Change history >--------------------------------------------
# 29Jul2017: JBH: first release

  # grab the salinity data
  tmp <- salinity.detrended[[iSpec$stat]]
  # pick which salinity series to use (BBP or SAP)
  iSpec$hydroTermSel.var <- ifelse(iSpec$layer %in% c("B","BP","BBP"),"BBP","SAP")
  # merge data and rename variable to 'flw_sal'
  tmp <- merge(ct1, tmp[ ,c("date",iSpec$hydroTermSel.var )], by="date", all.x=TRUE)
  names(tmp)[names(tmp) == iSpec$hydroTermSel.var] <- 'flw_sal'

  # packup
  ct1.list <- list(ct1=tmp,iSpec=iSpec)
  return(ct1.list)

}
