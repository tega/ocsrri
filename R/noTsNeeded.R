#' Identifies the special proxies that do not require a timeseries for srri calculation
#'
#' Identifies the special proxies that do not require a timeseries for srri calculation
#' 
#' @param map.tib a tibble with a 'iid_proxy' field. 
#'
#' @return a logical vector of the same length as map.tib[['iid_proxy']]
#'
#' @export

noTsNeeded <- function(map.tib) {
  specialProxies.v <- c("srri_lipper", "no_change", paste0("srri_", 1:7), "srri_NA")
  
  return(is.element(map.tib[["iid_proxy"]], specialProxies.v))
}
