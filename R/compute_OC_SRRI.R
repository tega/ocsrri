#' Compute the SRRI from a timeseries of returns and an asOfDate
#'
#' Compute the SRRI from a timeseries of returns and an asOfDate
#'
#' @param iid the INTERNAL_ID of the investment object.
#'
#' @param retXts.l a list of xts weekly returns, where each component is the result of createWeeklyReturnsTs.
#'
#' @param asOfDate the reference date.
#'
#' @param show_info logical. If TRUE the 'INFO: ...' messages are printed to the console.
#' 
#' @return a list with the following fields:
#'
#' 		- ret.xts the xts of weekly returns
#' 		- dataRange the range of ret.xts
#' 		- nObs the number of weekly returns 
#' 		- realizedYearlyVol the realized yearly volatility
#' 		- srri the synthetic risk and reward indicator
#' 		- info a named vector of character strings
#'
#' @export

computeOC_SRRI <- function(iid, retXts.l, asOfDate, show_info = TRUE) {
  
  if (show_info) message(paste("INFO: compute srri for INTERNAL_ID", iid))
  ## extract the return series
  ret.xts <- retXts.l[[iid]]
  assertthat::assert_that(!is.null(ret.xts))
  
  isWednesday <- as.POSIXlt(asOfDate)$wday == 3
  
  eDate <- asOfDate
  if (isWednesday) nbWeeks <- 259L else nbWeeks <- 260L
  sDate <- eDate - nbWeeks*7L
  
  requiredDates.v <- c(startDate = sDate, endDate = eDate)
  requiredPeriod <- paste(requiredDates.v, collapse = "/")
  
  use.xts <- ret.xts[requiredPeriod,]
  assertthat::assert_that(nrow(use.xts) > 0)
  nObs <- nrow(use.xts)
  
  ## if the next assertion is wrong the algorithm
  ## has a bug and must be corrected!!!
  assertthat::assert_that(nObs <= 260) 
  
  
  availableDates.v <- range(zoo::index(use.xts))
  names(availableDates.v) <- c("startDate", "endDate")
  
  info <- character(0)
  ## check if the timeseries is update w.r.t. the asOfDate, i.e. the number of missing days
  ## at the end of the timeseries is larger than 7
  nbMissingDaysAtEnd <- as.numeric(requiredDates.v["endDate"] - availableDates.v["endDate"])
  if (nbMissingDaysAtEnd > 7) {
    msg <- paste0("INFO: the required (available) end date is ", requiredDates.v[["endDate"]],
      " (", availableDates.v[["endDate"]],"). Update required.")
    if (show_info) message(msg)
    info <- c(info, 'end_date_issue' = msg)
  }
  ## check if the startDate is as required by the 5 years data history
  nbMissingDaysAtStart <- as.numeric(availableDates.v["startDate"] - requiredDates.v["startDate"])
  if (nbMissingDaysAtStart > 7) {
    msg <- paste0("INFO: the required (available) start date is ", requiredDates.v[["startDate"]],
      " (", availableDates.v[["startDate"]],").")
    if (show_info) message(msg)
    info <- c(info, 'start_date_issue' = msg)
  }
  ## info about the number of missing observations: 
  ## if it is larger than 10% of the 260 weeks, i.e. 5 obs/week * 26 weeks
  ## if it is zero, i.e. nbObs == 260
  ## if it is less than 10% but not zero
  if (nbMissingDaysAtStart + nbMissingDaysAtEnd > 5*26) {
    msg <- paste0("INFO: the number of observations is equal to ", nObs, "/260.\n")
    if (show_info) message(msg)
    info <- c(info, 'more_10%_missing_obs' = msg)
  } else if (nObs == 260) {
    msg <- paste0("INFO: Inv. obj. with INTERNAL_ID ", iid, " has ", nObs, "/260 observations.\n")
    if (show_info) message(msg)
    info <- c(info, 'zero%_of_missing_obs' = msg)
  } else {
    msg <- paste0("INFO: Inv. obj. with INTERNAL_ID ", iid, " has ", nObs, "/260 observations.\n")
    if (show_info) message(msg)
    info <- c(info, 'less_10%_missing_obs' = msg)
  }
  if (length(info) == 0) info <- NA_character_ 
  
  realizedYearlyVol <- sd(use.xts[, 1]) * sqrt(52) * sqrt((nObs-1) / nObs)
  
  srri <- ocsrri::volToSrri(realizedYearlyVol)
  
  result.l <- list(
    ret.xts = use.xts,
    dateRange = range(zoo::index(use.xts)),
    nObs = nObs,
    realizedYearlyVol = realizedYearlyVol,
    srri = srri,
    info = info
  )
  
  return(result.l)
}
