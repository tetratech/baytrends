#'Event Processing
#'
#'Computes the event number \code{eventNum}, the length of events
#'\code{eventLen}, or the sequence number for individual observations within an
#'event \code{eventSeq}.  Added from smwrBase.
#'
#' @rdname eventProcessing
#' @aliases eventLen eventNum eventSeq
#' @param event a logical vector where \code{TRUE} indicates that an event
#'occurred. Missing values are treated as instructed by \code{na.fix}.
#' @param reset a logical value indicating whether the event is assumed to
#'continue until the next event, or only while event is \code{TRUE}.
#' @param na.fix the value to use where event has missing values (\code{NA}s).
#' @param eventno an integer vector indicating the event number. Generally the
#'output from the \code{eventNum} function.
#' @param summary a logical value, controlling output. See \bold{Value} for
#'details.
#' @return The function \code{eventNum} returns an integer vector the same
#'length as \code{event} indicating the event sequence number.\cr
#'
#'The function \code{eventLen} returns an integer vector the same length as
#'\code{eventno} indicating the sequence length of the event if \code{summary}
#'is \code{FALSE}, or a named integer vector indicating the sequence length of
#'each event if \code{summary} is \code{TRUE}.\cr
#'
#'The function \code{eventSeq} returns an integer vector the same length as
#'\code{eventno} indicating the sequence number of each element in the
#'event.
#' @keywords internal manip
#' @export
#' @examples
#'
#'## Notice the difference caused by setting reset to TRUE
#'eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE))
#'eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE)
eventNum <- function(event, reset=FALSE, na.fix=FALSE) {
  ## Coding history:
  ##    2005????? DLLorenz Initial coding.
  ##    2006May18 DLLorenz function name changes
  ##    2007Apr05 DLLorenz Added eventLen
  ##    2011May25 DLLorenz Begin Conversion to R 
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##    2025Oct20 JBH: Fully vectorized via rle(); no while-loop; Single pass
  ##    in “run space,” then rep() to element level; Clear reset semantics:
  ##    with reset=FALSE, extend each TRUE run’s ID over the immediately
  ##    following FALSE run only; Robust NA handling via na.fix before
  ##    processing; Fewer allocations 
  
  # legacy smwrBase code
  # event[is.na(event)] <- na.fix
  # event.rle <- rle(c(event, FALSE))
  # number <- 0L
  # ret.val <- rep(0L, length(event) + 1L)
  # i <- 1L
  # beg <- 1L
  # while(i < length(event.rle$values)) {
  #   if(event.rle$values[i]) {
  #     number <- number + 1L
  #     end <- beg + event.rle$lengths[i] + event.rle$lengths[i+1] - 1L
  #     ret.val[beg:end] <- number
  #     beg <- end + 1L
  #     i <- i + 2L
  #   } else {
  #     beg <- event.rle$lengths[i] + 1L # can only be 1
  #     i <- i + 1L
  #   }
  # }
  # ret.val <- ret.val[seq(along=event)] # remove the last value
  # if(reset)
  #   ret.val <- ifelse(event, ret.val, 0L)
  # return(ret.val)
  
  # Normalize and run-length encode
  event[is.na(event)] <- na.fix
  r <- rle(as.logical(event))
  nrun <- length(r$lengths)
  
  # IDs per run: increment on TRUE runs, 0 on FALSE runs
  id_run <- integer(nrun)
  id_run[r$values] <- cumsum(r$values)[r$values]
  
  if (!reset) {
    # Extend each TRUE run's id over the immediately following FALSE run
    extend <- which(r$values) + 1L
    extend <- extend[extend <= nrun]               # guard last run
    id_run[extend] <- pmax(id_run[extend], id_run[extend - 1L])
  } else {
    # Keep FALSE runs at 0
    id_run[!r$values] <- 0L
  }
  
  # Expand back to element level
  rep(id_run, r$lengths)
  
}

#' @rdname eventProcessing
#' @export
#' @examples
#'
#'## Notice the difference caused by setting reset to TRUE
#'eventSeq(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE)))
#'eventSeq(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE))
eventSeq <- function(eventno) {
  ## Compute the sequence number within an event
  
  ## Coding history: 
  ##     2025Oct20 JBH: Replaced apply(.., seq) with
  ##     sequence(r$lengths) (C-level vectorization); Outputs zero for non-event
  ##     positions in one shot; No intermediate matrices/lists
  
  # legacy smwrBase code
  # ret.val <- as.vector(unlist(apply(as.matrix(rle(eventno)$lengths),
  #                                   1L, function(x) seq(1L,x))))
  # ret.val <- ifelse(eventno == 0L, 0L, ret.val)
  # return(ret.val)
  
  # Sequence within runs, zeroed outside events
  r <- rle(eventno)
  seq_in_run <- sequence(r$lengths)
  # Zero positions where eventno == 0
  rep(ifelse(r$values == 0L, 0L, 1L), r$lengths) * seq_in_run
  
  
}

#' @rdname eventProcessing
#' @export
#' @examples
#'
#'## Notice the difference caused by setting reset to TRUE
#'eventLen(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE))
#'## This is an example of the summary option
#'eventLen(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE), summary=TRUE)
eventLen <- function(eventno, summary=FALSE) {
  ## Compute the length of each event
  ## if summary is desired, then return named vector of lengths
  
  ## Coding history: 
  ##     2025Oct20 JBH: Vectorized: rep(ifelse(r$values>0L, r$lengths, 0L),
  ##     r$lengths) for per-element lengths • summary=TRUE returns a named 
  ##     vector of lengths for positive event IDs only • Zero outside events;
  
  # legacy smwrBase code
  # event.rle <- rle(eventno)
  # if(summary) {
  #   event.sel <- event.rle$values > 0L
  #   retval <- event.rle$lengths[event.sel]
  #   names(retval) <- event.rle$values[event.sel]
  #   return(retval)
  # }
  # event.rle$values <- (event.rle$values > 0L) * event.rle$length
  # return(rep(event.rle$values, times=event.rle$length))
  
  r <- rle(eventno)
  if (summary) {
    keep <- r$values > 0L
    out  <- r$lengths[keep]
    names(out) <- r$values[keep]
    return(out)
  }
  # Length per element, zero outside events
  rep(ifelse(r$values > 0L, r$lengths, 0L), r$lengths)
  
  
}
