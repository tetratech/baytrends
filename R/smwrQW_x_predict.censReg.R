#' Predict Values
#'
#' Predicts values from a censored regression object: method for "censReg" object.
#'
#'Some combinations of \code{type} and \code{se.fit} and \code{interval}
#'are not possible, depending on the distribution of the model. If \code{type}
#'is "link" or the distribution is normal, then all combinations are possible.
#'the value for type may be "mean" only if the distribution is log-normal, and 
#'the only additional option is "estimate" for \code{se.fit}. For either log-normal
#'or common log distributions, if the \code{type} is "response," then the options for
#'\code{se.fit} and \code{interval} ae "none" or "prediction."
#'
#' @param object the output from \code{censReg}.
#' @param newdata an optional data frame in for predictions.
#'If omitted, the observations from \code{object} are used.
#' @param type the type of predicted value. To get predictions on the
#'original scale of the response, use "response." To get predictions on the
#'transformed scale of the response, use "link." To get the mean predictions
#'for log-normal data, use "mean."
#' @param se.fit the type of standard errors of the predicted value to include
#'in the result.
#' @param interval type type of interval calculation. See \code{Details}.
#' @param level the confidence or prediction interval.
#' @param na.action function determining what should be done with missing values
#'in newdata. The default is to predict NA.
#' @param \dots further arguments passed to or from other methods.
#' @return A vector of predictions or a matrix of predictions and the other
#'selected statistics. In contrast with other regression predict functions,
#'a list of fitted values and standard errors is never returned.
#' @keywords internal
#' @note The predicted value for \code{type} = "mean" uses the adjusted maximum
#'likelihood method for bias correction described by Cohn (1988) and Cohn and 
#'others (1992). that method will give sligthly different predicted values than
#'the minimum variance unbiased estimate (MVUE) described by Bradu and Mundlak (1970).
#' @references Bradu, D., and Mundlak, Y. 1979, Estimation in lognormal linear models:
#'Journal of the American Statistical Association, v. 65, no. 39, p. 198--211.\cr
#'Cohn, T.A., 1988, Adjusted maximum likelihood estimation of the moments of lognormal
#'populations form type I censored samples: U.S. Geological Survey Open-File Report 88-350, 34 p.\cr
#'Cohn, T.A., Gilroy, E.J., and Baier, W.G., 1992, Estimating fluvial transport of trace
#'constituents using a regression model with data subject to censoring: Proceeding of the 
#'Joint Statistical Meeting, Boston, August 9--13, 1992, p. 142--151.
#' @seealso \code{\link{censReg}}, \code{\link{predictMVUE}}
#' @method predict censReg
#' @export
predict.censReg <- function(object, newdata, type=c("response", "link", "mean"),
  se.fit=c("none", "estimate", "prediction"), 
  interval=c("none", "confidence", "prediction"), level=0.95,
  na.action=na.pass, ...)  {
  ## Coding history:
  ##    2013Apr23 DLLorenz Original Coding
  ##
  ## Preliminaries
  type <- match.arg(type)
  se.fit <- match.arg(se.fit)
  interval <- match.arg(interval)
  clev <- substring(as.character(level), 2L)
  saved.na.action <- NULL
  ## Get X
  if(missing(newdata) || is.null(newdata))
    X <- object$XLCAL
  else {
    Terms <- delete.response(object$terms)
    m <- model.frame(Terms, newdata, na.action = na.action)
    X <- model.matrix(Terms, m)
    ## Need to manually extract and process NAs becuase can't pass to FORTRAN
    ckna <- is.na(rowSums(X))
    if(any(ckna)) {
      saved.na.action <- which(ckna)
      attr(saved.na.action, "class") <- "exclude"
      X <- X[!ckna,]
    }
  }
  pred <- censReg_AMLE.pred(object, X)
  ## The predictions and other stuff
  if(pred$IERR != 0)
    stop("Internal error")
  DF <- object$NOBSC - object$NPAR
  if(object$dist == "normal" || type == "link") {# All predictions the same
    predictions <- matrix(pred$ESTIM, ncol=1, dimnames=list(NULL, "fit"))
    if(se.fit == "estimate")
      predictions <- cbind(predictions, see=pred$ESTSEE)
    else if(se.fit == "prediction")
      predictions <- cbind(predictions, sep=pred$ESTSEP)
    if(interval == "confidence") { # create upper and lower confidence intervals
      tval <- qt(1 - (1 - level)/2, DF)
      cls <- cbind(pred$ESTIM + pred$ESTSEE*tval, pred$ESTIM - pred$ESTSEE*tval)
      colnames(cls) <- paste(c("ucl", "lcl"), clev, sep="")
      predictions <- cbind(predictions, cls)
    }
    else if(interval == "prediction") {
      tval <- qt(1 - (1 - level)/2, DF)
      cls <- cbind(pred$ESTIM + pred$ESTSEP*tval, pred$ESTIM - pred$ESTSEP*tval)
      colnames(cls) <- paste(c("upl", "lpl"), clev, sep="")
      predictions <- cbind(predictions, cls)
    }
  } # End of normal distribution
  else if(object$dist == "lognormal") {
    if(type == "response") {
      predictions <- matrix(exp(pred$ESTIM), ncol=1, dimnames=list(NULL, "fit"))
      if(se.fit == "estimate")
        warning("Standard errors of the estimate are not possible for \"response\" prediction of log-normal data")
      else if(se.fit == "prediction") {
        ## Properties of log-normal dist
        mu <- pred$ESTIM
        sigma <- pred$ESTSEP
        varp <- (exp(sigma^2) - 1) * exp(2*mu + sigma^2)
        predictions <- cbind(predictions, sep=sqrt(varp))
      }
      if(interval == "confidence")
       warning("Confidence intervals are not possible for \"response\" prediction of log-normal data")
      else if(interval == "prediction") {
        tval <- qt(1 - (1 - level)/2, DF)
        cls <- cbind(exp(pred$ESTIM + pred$ESTSEP*tval), exp(pred$ESTIM - pred$ESTSEP*tval))
        colnames(cls) <- paste(c("upl", "lpl"), clev, sep="")
        predictions <- cbind(predictions, cls)
      }
    } # End of response
    else { # must be mean
      predictions <- matrix(pred$BACKEST, ncol=1, dimnames=list(NULL, "fit"))
      if(se.fit == "estimate")
        predictions <- cbind(predictions, see=sqrt(pred$BACKVAR))
      else if(se.fit == "prediction")
        warning("Standard errors of prediction are not possible for \"mean\" prediction of log-normal data")
      if(interval != "none")
        warning("No interval estimates are not possible for \"mean\" prediction of log-normal data")
    }
  } # End of log normal
  else { # Only remaining distribution is common log
    if(type == "mean") {
      warning("Mean estimates are not possible for common log data")
      return(napredict(saved.na.action, rep(NA_real_, ncol(X))))
    }
    ## Type must be response
    predictions <- matrix(10^(pred$ESTIM), ncol=1, dimnames=list(NULL, "fit"))
    if(se.fit == "estimate")
      warning("Standard errors of the estimate are not possible for \"response\" prediction of common log data")
    else if(se.fit == "prediction") {
      ## Properties of log-normal dist, convert from log10 to natural
      mu <- pred$ESTIM * 2.302585092994045
      sigma <- pred$ESTSEP * 2.302585092994045
      varp <- (exp(sigma^2) - 1) * exp(2*mu + sigma^2)
      predictions <- cbind(predictions, sep=sqrt(varp))
    }
    if(interval == "confidence")
     warning("Confidence intervals are not possible for \"response\" prediction of common log data")
    else if(interval == "prediction") {
      tval <- qt(1 - (1 - level)/2, DF)
      cls <- cbind(10^(pred$ESTIM + pred$ESTSEP*tval), 10^(pred$ESTIM - pred$ESTSEP*tval))
      colnames(cls) <- paste(c("upl", "lpl"), clev, sep="")
      predictions <- cbind(predictions, cls)
    }
  } # End of common log
  if(ncol(predictions) == 1L)   
    predictions <- as.vector(predictions)
  return(napredict(saved.na.action, predictions))
}
