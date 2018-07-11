#' Prepare ANOVA table for GAM analysis
#'
#' @param gamo output from gam model
#' @export
#'
.gamANOVA <- function(gamo) {
# -----< Change history >--------------------------------------------
# 11Jul2018: JBH: updated to allow for situations where gam formula
#                 does not have a parametric term
# 16Jun2016: JBH: added some notes to code

  # gamo <- gamRslt
  anov.gamo     <- anova(gamo)
  if (length(anov.gamo$pTerms.table)>0) {
    agamp <- data.frame(anov.gamo$pTerms.table)
  } else {
    agamp <- data.frame(df=NA_real_,F=NA_real_,p.value=NA_real_)
    rownames(agamp) <- "NA"
  }
  agamp$type    <- '   "      " '
  agamp$type[1] <- 'parametric terms'
  agams         <- data.frame(anov.gamo$s.table)
  agams$type    <- '   "      " '
  agams$type[1] <- 'smoothed terms'
  names(agams)[names(agams)=='edf'] <- 'df' # rename edf
  agams         <- agams[,names(agams)!="Ref.df"]   # drop ref.df
  agamo         <- rbind(agamp,agams)
  agamo$source  <- rownames(agamo)
  agamo[,1]     <- round(agamo[,1],2)  # round edf
  agamo[,2]     <- round(agamo[,2],4)  # round F-stat
  agamo[,3]     <- .fmtPval(agamo[,3]) # format p-value
  rownames(agamo) <- NULL
  agamo <- agamo[,c(c(4,5,1,2,3))]
  return(agamo)
}


############################
#' Prepare table of coefficients for GAM analysis
#'
#' @param lmo output from gam model
#' @export
#'
.gamCoeff <- function(lmo, iSpec) { 
#  lmo <- gamRslt

# -----< Change history >--------------------------------------------
# 01May2018: JBH: added iSpec to list of arguments, merge intervention label
# 19Jul2017: JBH: Expanded table to include comparison of interventions
#                 on an "A->B", "B->C", ... basis; changed lm.coeff
#                 to not use factors

  lm.sum <- summary(lmo)
  p.se <- lm.sum$se[1:length(lm.sum$p.coeff)]
  lm.coeff <- data.frame(
    source    = names(lm.sum$p.coeff),
    estimate  = round(lm.sum$p.coeff,6),
    std.error = round(p.se,6),
    t.value   = round(lm.sum$p.t,4),
    p.value   = .fmtPval(lm.sum$p.pv), stringsAsFactors = FALSE)
  rownames(lm.coeff) <- NULL

  # extract list of intervention terms from coefficient table
  # (use exact beginning match to miss all "ti" terms
  interven1 <- lm.coeff[grep("^intervention", lm.coeff$source) ,"source"]

  # proccess interventions if in coefficient table
  if(length(interven1) > 0) {

    # add columns to coefficient table
    lm.coeff$intChg.p.value <- '-'               # for formatted results
    lm.coeff$intChg.est     <- NA_real_          # for rounded results
    lm.coeff$intChg.p.value.actual <- NA_real_   # unrounded results
    lm.coeff$intChg.est.actual     <- NA_real_   # unrounded results

    ### Analyze A -> B change
    lm.coeff[which(lm.coeff$source=="interventionB"),"intChg.p.value"] <-
      as.character(lm.coeff[which(lm.coeff$source=="interventionB"),"p.value"]  )
    lm.coeff[which(lm.coeff$source=="interventionB"),"intChg.est"] <-
      lm.coeff[which(lm.coeff$source=="interventionB"),"estimate"]
    lm.coeff[which(lm.coeff$source=="interventionB"),"intChg.p.value.actual"] <-
      lm.sum$p.pv[which(names(lm.sum$p.pv)=="interventionB")]
    lm.coeff[which(lm.coeff$source=="interventionB"),"intChg.est.actual"] <-
      lm.sum$p.coeff[which(names(lm.sum$p.coeff)=="interventionB")]

    ### create list of remaining comparisons
    interven2 <- interven1[-1]
    interven1 <- interven1[1:length(interven1)-1]
    intervenN <- length(interven1)

    # for cases when there are more comparisons than just the "A->B" comparison
    if(intervenN > 0) {

      # Extract coefficients, Var-Cov matrix, & df from gam3 model
      beta    <- lmo$coefficients        # coefficients vector
      VCmat   <- lmo$Vp                  # variance-covariance matrix of coefficents
      dfe     <- lmo$df.residual         # residual degrees of freedom

      for (i in 1:intervenN) {
        # create vector of 0, 1, and -1
        x <- rep(0,length(beta))
        x[which(names(beta)==interven1[i])] <- -1
        x[which(names(beta)==interven2[i])] <- +1
        # calc p-value
        est   <- t(x) %*% beta           # estimate of change
        var   <- t(x) %*% VCmat %*% x    # variance of change
        sigma <- sqrt(var)               # get standard error from variance
        t     <- est/sigma               # compute a t-statistic
        pval  <- 2*(1-pt(abs(t),df=dfe)) # calculate p-value for h0:t=0
        lm.coeff[which(lm.coeff$source==interven2[i]),"intChg.est"]     <-  round(est,6)
        lm.coeff[which(lm.coeff$source==interven2[i]),"intChg.p.value"] <-  .fmtPval(pval)
        lm.coeff[which(lm.coeff$source==interven2[i]),"intChg.est.actual"]     <-  est
        lm.coeff[which(lm.coeff$source==interven2[i]),"intChg.p.value.actual"] <-  pval
      }
    }
    # 01May2018: add label 
    intervenList <- iSpec$intervenList
    intervenList$interventionLabel <- paste0('intervention',intervenList$intervention)
    lm.coeff <- merge(lm.coeff, intervenList[ ,c("interventionLabel","label")], 
                      by.x="source",by.y="interventionLabel", all.x=TRUE)
    
  }
  return(lm.coeff)
}


############################
#' Compute and present report on percent different for log-transformed data
#'
#' @param por.diff Output from gam.por.diff
#' @param iSpec data set specifications
#' @export
#'
.gamDiffPORtbl <- function(por.diff, iSpec) {

# -----< Change history >--------------------------------------------
# 04Nov2016: JBH: Modified to accomodate por.diff as a 2 member list. Added 'Estimate.Adj' term
# 03Jun2016: JBH: Moved calculations of percent change from this function over to
#                 .gamDiff. Thus this function now just returns a simple table. (commented
#                 lines of code can be removed if QC is completed.)

  transform  <- iSpec$transform
  # logConst   <- iSpec$logConst
  # per.mn     <- por.diff$per.mn
  # per.mn.obs <- por.diff$per.mn.obs
  # pct.chg    <- por.diff$pct.chg

  if(transform) {
    #code for log
    CI <- paste0(100*(1-por.diff[[1]]$alpha),"% Confidence interval for log difference")
    tmp <-data.frame (
        Calculation = c( "Baseline log mean (geometric mean)",
                         "Current log mean (geometric mean)",
                         "Estimated log difference  ",
                         "Std. Err. log difference  ",
                         CI,
                         "Difference p-value  ",
                         "Period of Record Percent Change Estimate (%)",
                         "Period of Record  "),
        Estimate    = c( paste0(as.character(round(por.diff[[1]]$per.mn[1],4))      , " (",
                                as.character(round(por.diff[[1]]$per.mn.obs[1],4))  , ")"),
                         paste0(as.character(round(por.diff[[1]]$per.mn[2],4))      , " (",
                                as.character(round(por.diff[[1]]$per.mn.obs[2],4))  , ")"),
                         as.character(       round(por.diff[[1]]$diff.est,4)),
                         as.character(       round(por.diff[[1]]$diff.se,4)),
                         paste0("(", as.character(round(por.diff[[1]]$diff.ci[1],4)), " , ",
                                     as.character(round(por.diff[[1]]$diff.ci[2],4)),")"),
                         as.character(.fmtPval(por.diff[[1]]$diff.pval) ),
                         paste0(as.character(round(por.diff[[1]]$pct.chg,2)),"%"),
                         paste0(as.character(por.diff[[1]]$base.yr[1]), " - ",
                                as.character(por.diff[[1]]$test.yr[2])) ),
        Estimate.Adj    = c( paste0(as.character(round(por.diff[[2]]$per.mn[1],4))      , " (",
                                as.character(round(por.diff[[2]]$per.mn.obs[1],4))  , ")"),
                         paste0(as.character(round(por.diff[[2]]$per.mn[2],4))      , " (",
                                as.character(round(por.diff[[2]]$per.mn.obs[2],4))  , ")"),
                         as.character(       round(por.diff[[2]]$diff.est,4)),
                         as.character(       round(por.diff[[2]]$diff.se,4)),
                         paste0("(", as.character(round(por.diff[[2]]$diff.ci[1],4)), " , ",
                                as.character(round(por.diff[[2]]$diff.ci[2],4)),")"),
                         as.character(.fmtPval(por.diff[[2]]$diff.pval) ),
                         paste0(as.character(round(por.diff[[2]]$pct.chg,2)),"%"),
                         paste0(as.character(por.diff[[2]]$base.yr[1]), " - ",
                                as.character(por.diff[[2]]$test.yr[2])) ) )
  } else {
    #code for raw
    CI <- paste0(100*(1-por.diff[[1]]$alpha),"% Confidence interval for difference")
    tmp <-data.frame (
      Calculation = c( "Baseline mean",
                       "Current mean",
                       "Estimated difference  ",
                       "Std. Err. difference  ",
                       CI,
                       "Difference p-value  ",
                       "Period of Record Percent Change Estimate (%)",
                       "Period of Record  "),
      Estimate    = c( paste0(as.character(round(por.diff[[1]]$per.mn[1],4)) ),
                       paste0(as.character(round(por.diff[[1]]$per.mn[2],4)) ),
                       as.character(       round(por.diff[[1]]$diff.est,4)),
                       as.character(       round(por.diff[[1]]$diff.se,4)),
                       paste0("(", as.character(round(por.diff[[1]]$diff.ci[1],4)), " , ",
                                   as.character(round(por.diff[[1]]$diff.ci[2],4)),")"),
                       as.character(.fmtPval(por.diff[[1]]$diff.pval) ),
                       paste0(as.character(round(por.diff[[1]]$pct.chg,2)),"%"),
                       paste0(as.character(por.diff[[1]]$base.yr[1]), " - ",
                              as.character(por.diff[[1]]$test.yr[2])) )   ,
      Estimate.Adj    = c( paste0(as.character(round(por.diff[[2]]$per.mn[1],4)) ),
                       paste0(as.character(round(por.diff[[2]]$per.mn[2],4)) ),
                       as.character(       round(por.diff[[2]]$diff.est,4)),
                       as.character(       round(por.diff[[2]]$diff.se,4)),
                       paste0("(", as.character(round(por.diff[[2]]$diff.ci[1],4)), " , ",
                              as.character(round(por.diff[[2]]$diff.ci[2],4)),")"),
                       as.character(.fmtPval(por.diff[[2]]$diff.pval) ),
                       paste0(as.character(round(por.diff[[2]]$pct.chg,2)),"%"),
                       paste0(as.character(por.diff[[2]]$base.yr[1]), " - ",
                              as.character(por.diff[[2]]$test.yr[2])) )    )
  }

#  gamDiffPORtbl <- list (pct.chg=pct.chg, gamDiffPORtbl= tmp)
  gamDiffPORtbl <- tmp

}

