#' Adjust the map.tib between iid and iid_proxy
#'
#' Adjust the map.tib between iid and iid_proxy by replacing the 'by_rat_mat'
#'        values of the iid_proxy with the associated iid_proxy (see the map
#'        produced by the createRatingData function).
#'
#' @param map.tib the map between investment iid and proxy iid.
#' @param asOfDate the reference date used to adjust the map.tib.
#' @param li the exported list of investments containing the PXX tibbles.
#'
#' @return a tibble identical to the input map.tib where the entries of
#'    the field iid_proxy equal to 'by_rat_mat' have been replaced by the 
#'    correct proxy according to the mapByCcy.l component of the rating.l
#'    created by the createRatingData function. For the modified rows the
#'    field type_proxy is set equal to 'idx'.
#'
#' @export
adjustDynamicProxy <- function(map.tib, asOfDate, li) {

  message("INFO: adjust dynamic proxy 'by_rat_mat'")
  is.dynProxy.v <- map.tib[["iid_proxy"]] == "by_rat_mat"
  if (!any(is.dynProxy.v)) return(map.tib)

  mapNoDyn.tib <- map.tib[!is.dynProxy.v,]
  mapDyn.tib <- map.tib[is.dynProxy.v,]

  proxy.l <- proxy_rat_mat(mapDyn.tib, asOfDate, li)

  mapDyn.tib[["iid"]] <- names(proxy.l)
  mapDyn.tib[["iid_proxy"]] <- unname(sapply(proxy.l, function(x) x[["iid_proxy"]]))
  mapDyn.tib[["type_proxy"]] <- unname(sapply(proxy.l, function(x) x[["type_proxy"]]))
  mapDynAdjusted.tib <- rbind(mapNoDyn.tib, mapDyn.tib)
  return(mapDynAdjusted.tib)
}
