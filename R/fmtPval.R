############################
#' Format pvalues
#'
#' @param pval pvalue to format
#' @export
#'
.fmtPval <- function(pval) {
  
# -----< Change history >--------------------------------------------
# 05May2016: JBH: changed is.na value formatting to filling in with '-'
  
  pval <- ifelse((pval<0.0001),"<0.0001",sprintf('%6.4f',pval))
  pval[is.na(pval)] <- "-"
  fmtPval <- pval
}