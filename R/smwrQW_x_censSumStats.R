#' @title Summary Statistics
#'
#' @description Computes selected summary statistics for censored data.
#'
#' @param \dots any number of vectors or a data frame of the data to
#'be summarized. If \code{\dots} is a data frame, then only numeric data,
#'including "lcens," "mcens," and "qw" data types, will be summarized.
#' @param group the data to group the data in \code{\dots}.
#' @param Nums logical, include the number of observations and numbers of 
#'censored values in the output data.frame.
#' @param Stats logical, include the mean and standard deviation
#'in the output data.frame.
#' @param Probs the argument to quantile specifying the desired probabilities
#'in the output data.frame
#' @param na.rm logical, remove missing values before processing?
#' @param method the method to use for computing the statistics. Must be one of
#'"log MLE," "MLE," "log ROS," "ROS," or "flipped K-M." The method "flipped K-M"
#'is only applicaple to un- or left-censored data.
#' @return A data.frame with the selected statistcs for each variable
#'in \code{\dots} grouped by each unique value in \code{group}.
#' @keywords internal univar
#' @export
censSumStats <- function(..., group=NULL, Nums=TRUE, Stats=TRUE,
												 Probs=(0:4)/4, na.rm=TRUE, method="log MLE") {
	## Coding history:
	##    2013Feb25 DLLorenz Original Coding from sumStats
	##    2013Feb25          This version.
	##
	## Internal function to compute # left, right, and interval censoring
	NumCen <- function(x) {
		x <- as.mcens(x)
		NL <- sum(x@censor.codes == -1, na.rm=TRUE)
		NR <- sum(x@censor.codes == 1, na.rm=TRUE)
		NI <- sum(x@interval, na.rm=TRUE)
		return(c(NL, NR, NI))
	}
	## Internal function to rbind data frames that contain censored data
	rbindQW <- function(xlist) {
		# xlist is a list of lists, each list in xlist contains a list
		# of data that can be converted to a data.frame, but may contain 
		# lcens or mcens data and so cannot be assembled by the default methods
		## Get the names
		dfnames <- names(xlist[[1]])
		## Get the types
		dftypes <- lapply(dfnames, function(i) 
			sapply(xlist, function(x) class(x[[i]])[1L]))
		dftypes <- sapply(dftypes, function(x) {	
			if(any(x == "mcens")) {
				retval <- "mcens"
			} else if(any(x == "lcens")) {
				retval <- "lcens"
			} else 
				retval <- ""
			retval
		})
		names(dftypes) <- dfnames
		## Get the data organized by columns
		dfdata <- lapply(dfnames, function(i) {
			retval <- lapply(xlist, function(x) {
				if(dftypes[i] == "mcens") {
					return(as.mcens(x[[i]]))
				} else if(dftypes[i] == "lcens") {
					return(as.lcens(x[[i]]))
				} else 
					return(x[[i]])
			})
			return(do.call(c, retval))
		})
		# suppress conversion to factors
		dfdata$stringsAsFactors <- FALSE
		dfdata <- do.call(data.frame, dfdata)
		names(dfdata) <- dfnames
		return(dfdata)
	}
	## Begin
	dots <- list(...)
	ndg <- length(dots)
	if(ndg == 1) {
		dotname <- deparse(substitute(...))
		dots <- dots[[1]]
		if(mode(dots) == "numeric") {
			dots <- list(dots)
			names(dots) <- dotname
		}
	}
	else { # multiple vectors were specified
		dotname <- as.list(match.call())
		## Drop named components
		dotname$Num <- dotname$group <- dotname$Stats <- NULL
		dotname$Probs <- dotname$na.rm <- dotname$method <-NULL
		dotname <- sapply(dotname, deparse)
		names(dots) <- dotname[-1L] # drop the call to censSumStats
	}
	dots <- dots[sapply(dots, is.numeric)]
	ndg <- length(dots)
	## Fix names of Probs so that they can be converted to a data.frame
	if(!is.null(Probs) && length(Probs) > 0)
		ProbNames <- paste("Pct", round(Probs * 100,
																		if(length(Probs) > 1) 2 - log10(diff(range(Probs)))
																		else 2), sep = ".")
	else
		ProbNames = ""
	if(!is.null(group)) {
		groups <- interaction(group, drop=TRUE)
		
		retval <- by(as.data.frame(dots), INDICES=groups, FUN=censSumStats, 
								 Nums=Nums, Stats=Stats, Probs=Probs, na.rm=na.rm,
								 method=method)
		
		retgrp <- by(as.data.frame(group, stringsAsFactors=FALSE),
								 INDICES=groups, FUN=function(x, n) {
								 	xx <- x[1,,drop=FALSE]
								 	if(n > 1) {
								 		xx <- as.data.frame(lapply(xx, rep, times=n), stringsAsFactors=FALSE)
								 	}
								 	xx	
								 }, n=ndg)
		if(substring(method, 1L, 1L) == "f") { # flipped K-M
			retval <- rbindQW(retval)
		} else
			retval <- do.call("rbind", retval)
		## The by functions appears to strip a single column data frame of its class
		if(class(retgrp[[1]]) != 'data.frame') {
			retgrp <- as.matrix(unlist(retgrp))
			colnames(retgrp) <- "Group" # assign a simple name
		}
		else
			retgrp <- do.call("rbind", retgrp)
		retval <- cbind(retgrp, retval)
		## Fix case where there is only one variable--results in Variable == dots
		if(ndg == 1 && !is.null(dotname))
			levels(retval$Variable) <- dotname
	}
	else { # no grouping
		retval <- lapply(dots, function(x, na.rm=TRUE, Nums, Stats, Probs,
																		ProbNames, method) {
			retpart <- list()
			if(!is.null(Nums) && Nums) {
				retpart <- c(sum(!is.na(x)), as.list(NumCen(x)))
				names(retpart) <- c("Num", "Nleft", "Nright", "Ninterval")
			}
			if(!is.null(Stats) && Stats) {
				retStats <- censStats(x, method=method, na.rm=na.rm)
				retpart <- c(retpart, unclass(retStats))
			}
			if(!is.null(Probs) && length(Probs) > 0) {
				retprob <-  censQuantile(x, probs=Probs, na.rm=na.rm, type=2, method=method)
				retprob <- unclass(retprob)
				names(retprob) <- ProbNames
				retpart <- c(retpart, retprob)
			}
			retpart
		} # end of function
		,na.rm=na.rm, Nums=Nums, Stats=Stats,
		Probs=Probs, ProbNames=ProbNames, method=method)
		VarNames <- names(retval)
		if(substring(method, 1L, 1L) == "f") { # flipped KM
			retval <- rbindQW(retval)
		} else {
			retval <- lapply(retval, as.data.frame)
			retval <- as.data.frame(do.call("rbind", retval))
		}
		if(!is.null(VarNames))
			retval <- cbind(Variable=VarNames, retval, stringsAsFactors=FALSE)
		return(retval)
	} # end of else
	row.names(retval) <- seq(nrow(retval))
	return(retval)
}
