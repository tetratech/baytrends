# ####
#' Load Built-in GAM formulas 
#'
#' Returns built-in GAM formulas
#' 
#' @param gamSelect character vector of models (Current options include gam0,
#'   gam1, gam2, gam3, gam4, gam5)
#'
#' @return Returns a list with GAM formulas 
#' @export
#' 
loadModels <- function(gamSelect='gam4') {
  
  gamModels <- list()
  
  for (gams in gamSelect) {
    if (gams == 'gam0') {
      gamModels0   <- list(
        list(option=0, name= "Linear Trend with Seasonality",
             model= paste0("~ cyear", 
                           "+ s(doy,bs='cc')"), 
             deriv=TRUE, gamK1=c(NA,NA), gamK2=c(NA,NA)))
    } else if (gams == 'gam1') {
      gamModels0   <- list(
        list(option=1, name= "Non-linear Trend with Seasonality",
             model= paste0("~ cyear", 
                           " + s(cyear, k=gamK1)",  
                           " + s(doy,bs='cc')"), 
             deriv=TRUE, gamK1=c(10,2/3), gamK2=c(NA,NA)))
    }else if (gams == 'gam2') {
      gamModels0   <- list(
        list(option=2, name= "Non-linear trend with Seas+Int",
             model= paste0("~ cyear",  
                           " + s(cyear, k=gamK1)",  
                           " + s(doy,bs='cc')", 
                           " + ti(cyear,doy,bs=c('tp','cc'))"),
             deriv=TRUE, gamK1=c(10,2/3), gamK2=c(NA,NA)))
    } else if (gams == 'gam3') {
      gamModels0   <- list(
        list(option=3, name= "Non-linear trend with Seas+Int. & Intervention",
             model= paste0("~ intervention",  
                           " + cyear",  
                           " + s(cyear, k=gamK1)",  
                           " + s(doy,bs='cc')",  
                           " + ti(cyear,doy,bs=c('tp','cc'))"),
             deriv=TRUE, gamK1=c(10,2/3), gamK2=c(NA,NA)))
    } else if (gams == 'gam4') {
      gamModels0   <- list(
        list(option=4, name= "Non-linear trend with Seas+Int. & Hydro Adj",
             model= paste0("~ cyear", 
                           " + s(cyear, k=gamK1)",
                           " + s(doy,bs='cc')",  
                           " + ti(cyear,doy,bs=c('tp','cc'))",  
                           " + s(flw_sal,k=gamK2)",  
                           " + ti(flw_sal,doy,bs=c('tp','cc'))",  
                           " + ti(flw_sal, cyear,bs=c('tp' ,'tp'))",  
                           " + ti(flw_sal,doy,cyear, bs=c('tp','cc','tp'))"),
             deriv=TRUE, gamK1=c(10,1/3), gamK2=c(10,2/3)) )
    } else if (gams == 'gam5') {
      gamModels0   <- list(
        list(option=5, name= "Non-linear trend with Seas+Int. & Inter/Hydro Adj",
             model= paste0("~ intervention", 
                           " + cyear", 
                           " + s(cyear, k=gamK1)", 
                           " + s(doy,bs='cc')", 
                           " + ti(cyear,doy,bs=c('tp','cc'))", 
                           " + s(flw_sal,k=gamK2)", 
                           " + ti(flw_sal,doy,bs=c('tp','cc'))", 
                           " + ti(flw_sal, cyear,bs=c('tp' ,'tp'))", 
                           " + ti(flw_sal,doy,cyear, bs=c('tp','cc','tp'))"),
             deriv=TRUE, gamK1=c(10,1/3), gamK2=c(10,2/3)))
    }
    gamModels <- c(gamModels, gamModels0)
  }
  
  return(gamModels)
  
}


