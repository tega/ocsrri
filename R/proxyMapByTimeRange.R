#' Get the internal identifiers of the used proxy SRRI_OC
#'
#' Given an asOfDate and the required connection parameters the internal identifiers of the 
#' used proxy SRRI_OC are returned
#'
#' @param dt the as of date for the request.
#' @param ndays the number of days in the past that must be considered in the identification
#' of the used proxy. Use ndays = 0 for an atomic selection based on the asOfDate dt only.
#' @param map.tib the map between investments and proxies. The following columns are needed: 
#'        'iid', 'type', 'iid_proxy', 'type_proxy', 'StartDate', 'Expiry'
#' @return A four columns tibble with 'iid', 'type', 'iid_proxy' and 'type_proxy' 
#' defining the map between I.O./Index and corresponding proxy. 
#' No particular check is performed to check for circularity in the proxy assignment between 
#' investment objects. If you need to avoid circular autoreferencing use the 
#' proxyValidityCheck function.
#' 
#' @examples
#' \dontrun{
#' proxyMapByTimeRange(dt = Sys.Date(), ndays = 7L*260L, map.tib)
#' } 
#' @export


proxyMapByTimeRange <- function(dt, ndays = 7L*260L, map.tib) {
  
  checkmate::assert_date(dt, len = 1L, any.missing = FALSE)
  
  filtered.tib <- ocalc::filter_date_interval_on_range(map.tib, dt, ndays)
  
  result.tbl <- unique(filtered.tib[, c("iid", "type", "iid_proxy", "type_proxy")])
  
  return(result.tbl)
}
