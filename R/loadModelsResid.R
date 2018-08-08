# ####
#' Load Built-in GAM formulas for calculating residuals
#'
#' Load Built-in GAM formulas for calculating residuals
#' 
#' @param gamSelect character vector of models (Current options include 'doy',
#'   'doy_flw_sal', 'doy_flw_sal_int')
#'
#' @return Returns a list with GAM formulas 
#' @export
#' 
loadModelsResid <- function(gamSelect='doy_flw_sal') {
  
  gamModels <- list()
  
  for (gams in gamSelect) {
    if (gams == 'doy') {
      gamModels0   <- list(
        list(option=0, name= "Seasonality",
             model= paste0("~ s(doy,bs='cc')"), 
             deriv=TRUE, gamK1=c(NA,NA), gamK2=c(NA,NA))) 
    } else if (gams == 'doy_flw_sal') {
      gamModels0   <- list(
        list(option=4, name= "Seasonality & Hydro Adj",
             model= paste0("~ s(doy,bs='cc')",  
                           " + s(flw_sal,k=gamK2)",  
                           " + ti(flw_sal,doy,bs=c('tp','cc'))"),
             deriv=TRUE, gamK1=c(10,1/3), gamK2=c(10,2/3)) )
    } else if (gams == 'doy_flw_sal_int') {
      gamModels0   <- list(
        list(option=5, name= "Seasonality & Hydro Adj w/ Interv.",
             model= paste0("~ intervention", 
                           " + s(doy,bs='cc')", 
                           " + s(flw_sal,k=gamK2)", 
                           " + ti(flw_sal,doy,bs=c('tp','cc'))"), 
             deriv=TRUE, gamK1=c(10,1/3), gamK2=c(10,2/3)))
      
    }
    gamModels <- c(gamModels, gamModels0)
  }
  
  return(gamModels)
  
}


