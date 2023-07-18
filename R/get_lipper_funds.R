#' Extract the INTERNAL_ID, ISIN pair of the investments objects available in Lipper
#'
#' Extract the INTERNAL_ID, ISIN pair of the investments objects available in Lipper
#'
#' @param asOfDate the as of date at which to extract the information
#' 
#' @param li list of investment objects from the Allocare request, i.e. a named list
#'        with field names "P01" "P03" "P33" "P39" "P41" "P42" "P43" "P44" "P45" "P46" "P47" "P52".
#' 
#' @return a tibble with columns INTERNAL_ID and ISIN.
#'         
#' @examples
#' \dontrun{
#'  li <- list()
#'	get_lipper_funds(Sys.Date(), li)
#' }
#' @author Claudio Ortelli
#' @export

get_lipper_funds <- function(asOfDate, li) {
  message("INFO: extract historical inv -> proxy map from li_withProxy$P42 (assuming indices do not have proxy!)")
  map.tib <- ocsrri::getProxyMap(li)
  ## extract the current iid -> iid_proxy map
  message(paste("INFO: construct inv -> proxy map as of", asOfDate))
  asOfMap.tib <- ocsrri::proxyMapByTimeRange(asOfDate, ndays = 0L, map.tib)
  
  message("Info: assigning the correct proxy for 'by_rat_mat' method")
  finalMap.tib <- ocsrri::adjustDynamicProxy(asOfMap.tib, asOfDate, li)
  ## inform about the type of the used proxy (investment or index)
  
  message("Info: extract 'srri_lipper'")
  lipper.tib <- dplyr::filter(finalMap.tib, .data[["iid_proxy"]] == "srri_lipper")
  
  selected.tib <- dplyr::filter(li$P01, is.element(.data[["Idtf5"]], lipper.tib$iid))[,c("Idtf5", "ISIN")]
  colnames(selected.tib) <- c("INTERNAL_ID", "ISIN")
  return(selected.tib)
}
