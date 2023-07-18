#' Check consistency of a given map.tib at a reference date
#'
#' Check consistency of a given map.tib at a reference date
#'
#' @param asOfDate the reference date used to adjust the map.tib.
#' @param map.tib the map between investment iid and proxy iid. the map.tib is
#' defined by means of the following 6 columns 'iid', 'type', 'iid_proxy', 'type_proxy',
#' 'StartDate', 'Expiry'
#' @param li the exported list of investments containing the PXX tibbles.
#'
#' @return an empty data.frame(iid = character(0), iid_proxy = character(0), stringsAsFactors = FALSE) if
#' no cycles are detected, the iid -> iid_proxy data.frame map of the involved investments otherwise.
#' @export

proxyConsistencyCheck <- function(asOfDate, map.tib, li) {
  
  message(paste("Info: proxy consistency check @ ", asOfDate))
  usedProxy.tib <- ocsrri::proxyMapByTimeRange(asOfDate, ndays = 0L, map.tib)
  adjusted.tib <- ocsrri::adjustDynamicProxy(usedProxy.tib, asOfDate, li)
  result.l <- ocsrri::proxyMapValidityCheck(adjusted.tib, action = "warn")
  
  return(result.l[["map.df"]])
}
