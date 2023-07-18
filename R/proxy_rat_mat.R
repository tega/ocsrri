# TODO: Add comment
# 
# Author: claudio
###############################################################################


proxy_rat_mat <- function(map.tib, asOfDate, li) {
  
  ## create the rating data
  rd.l <- createRatingData(asOfDate)
  
  message("INFO: create iid vector of investments with dynamic proxy")
  is.dynProxy.v <- map.tib[["iid_proxy"]] == "by_rat_mat"
  if (!any(is.dynProxy.v)) return(map.tib)
  
  mapNoDyn.tib <- map.tib[!is.dynProxy.v,]
  mapDyn.tib <- map.tib[is.dynProxy.v,]
  
  iidInvWithDynProxy.v <- mapDyn.tib[["iid"]]
  names(iidInvWithDynProxy.v) <- iidInvWithDynProxy.v
  
  result.l <- lapply(iidInvWithDynProxy.v, proxy_rat_mat_by_iid, asOfDate, li, 
      rd.l$rating.l, rd.l$matClass.v, rd.l$mapByCcy.l, rd.l$allowedInvType.v )
  
  return(result.l)
}
