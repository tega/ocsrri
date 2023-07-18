#' Compute the SRRI from a timeseries of returns and an asOfDate
#'
#' Compute the SRRI from a timeseries of returns and an asOfDate
#'
#' @param i an integer number between 1 and nrow(map.tib).
#' 
#' @param usedProxy.tib the tibble containing unique combinations of <iid_proxy, type_proxy>. Note that
#' the two columns of usedProxy.tib must have name 'iid' and 'type', respectively. 
#' 
#' @param ts.l the result of spliting the list of default timeseries by factors 'Idtf', 'TSType' and 'PriceCcy'.
#' 
#' @param li the list of investments with the PXX tibbles.
#' 
#' @param lix the list of indices with the PXX tibbles.
#' 
#' @return the xts of weekly returns. The attributes 'Ccy' and 'ts_type' are assigned to the returned xts.
#'  
#' @export

createWeeklyReturnTs <- function(i, usedProxy.tib, ts.l, li, lix) {
  
  ## check that the idx_int_id is available
  assertthat::assert_that(i <= nrow(usedProxy.tib))
  
  iid <- usedProxy.tib[i, "iid", drop = TRUE]
  type <- usedProxy.tib[i, "type", drop = TRUE]
  message(paste("INFO: creating return series for iid", iid, "of type", type))
  
  ## extract the available xts for the desired iid
  tsXts.l <- ts.l[[iid]]
  
  ## filter information for the desired iid
  if (type == "inv") {
    io.tib <- dplyr::filter(li$P01, .data[['Idtf5']] == iid)
    assertthat::assert_that(nrow(io.tib) == 1)
    instrCcy <- io.tib[["InstrCcy"]]
    
  } else if (type == "idx") {
    io.tib <- dplyr::filter(lix$P04, .data[['Idtf5']] == iid)
    
    ## are the index data available?
    assertthat::assert_that(nrow(io.tib) == 1)
    ## get the index currency
    instrCcy <- io.tib[["IndexCcy"]]
  } else {
    stop(paste("Invalid proxy type", type))
  }
  
  ## check that the Closing xts in that currency is available
  assertthat::assert_that(is.element("Closing", names(tsXts.l)))
  assertthat::assert_that(is.element(instrCcy, names(tsXts.l[["Closing"]])))
  
  ts.xts <- tsXts.l[["Closing"]][[instrCcy]]
  
  # wts.xts <- ocTsutils::complete_nwe_locf_to_weekly(ts.xts, wday = 3)
  # wRet.xts <- diff(log(wts.xts))[-1 ,] ## remove first obs which is NA
  wRet.xts <- ocTsutils::to_weekly_log_returns(ts.xts, wday = 3L, skip_first = TRUE)
  xts::xtsAttributes(wRet.xts) <- list(Ccy = instrCcy, TsType = "Closing")
  
  return(wRet.xts)
}

