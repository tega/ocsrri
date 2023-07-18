#' Title map the internal id to the corresponding srri
#'
#' Title map the internal id to the corresponding srri
#'
#' @param iid the INTERNAL_ID of the investment object.
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
#' @examples
#' \dontrun{
#'	
#' }
#' @author Claudio Ortelli
#' @export

options('alc.user' = 'claudio', 'alc.password' = 'Welcome2015')
single_inv_iid_to_srri <- function(iid, asOfDate, show_info = TRUE) {
  
  li <- ococpu::smart('ocwalc', 'get_list')(
    req_ext = 'R01',
    arglst = tibble::tibble('Request type' = '12', 'Investment identifier' = iid, 'Identifier system' = "INTERNAL_ID"), 
    setup = ocas::s(verbose = TRUE)
  )
  
  assertthat::assert_that(is.element("P01", names(li)))
  if (nrow(li[["P01"]]) == 0) {
    message("Warning: no investment object with INTERNAL_ID ==", iid, "\n")
    return(1L)
  }
  
  map.tib <- ocsrri::getProxyMap(li)
  
  map.df <- ocsrri::proxyConsistencyCheck(asOfDate, map.tib, li)
  
  if (nrow(map.df) > 0) {
    message(paste('Warning: proxy consistency check failed for investment with INTERNAL_ID ==', iid))
    return(2L)
  }
  
  ## extract the current iid -> iid_proxy map
  message(paste("INFO: construct inv -> proxy map as of", asOfDate))
  asOfMap.tib <- ocsrri::proxyMapByTimeRange(asOfDate, ndays = 0L, map.tib)
  
  message("INFO: assigning the correct proxy for 'by_rat_mat' method")
  finalMap.tib <- ocsrri::adjustDynamicProxy(asOfMap.tib, asOfDate, li)
  
  ## inform about the type of the used proxy (investment or index)
  message("INFO: the following map will be used:")
  message(
    paste(finalMap.tib$iid, " ", finalMap.tib$type, " -> ",
      finalMap.tib$iid_proxy, " ", finalMap.tib$type_proxy, "\n", sep = "")
  )
  ## check if the time series is necessary to compute srri
  noTsNeeded.tib <- dplyr::filter(finalMap.tib, ocsrri::noTsNeeded(finalMap.tib))
  tsNeeded.tib <-  dplyr::filter(finalMap.tib, !ocsrri::noTsNeeded(finalMap.tib))
  if (nrow(tsNeeded.tib) > 0) {
    message("INFO: SRRI computation requires a time series. Downloading ...")
    tsNeeded.tib[['iid_proxy']][ tsNeeded.tib[['iid_proxy']] == 'own_ts' ] <- tsNeeded.tib[['iid']][ tsNeeded.tib[['iid_proxy']] == 'own_ts']
    
    ## message("INFO: create the tib with the <iid_proxy, type_proxy> fields for TS creation")
    usedProxyTs.tib <- unique(tsNeeded.tib[, c("iid_proxy", "type_proxy")])
    
    ## message("INFO: rename used proxy columns <iid_proxy, type_proxy> -> <iid, type>")
    colnames(usedProxyTs.tib) <- c("iid", "type")
    
    ## message("INFO: get the default 'Closing' timeseries of the used proxies (investment and indices)")
    ts.tib <- ococpu::smart('ocwalc', 'get_default_ts')(info = usedProxyTs.tib, setup = ocas::s(verbose = verbose))
    
    ## message("INFO: split default timeseries by 'Idtf', 'TSType' and 'PriceCcy' ...")
    tsxts.l <- ocalc::splitByFactor(ts.tib, factor.v = c("Idtf", "TSType", "PriceCcy"), asXts = TRUE)
    
    message('INFO: import data for active indices (R04 request)')
    lix <- ococpu::smart('ocwalc', 'get_list')(
      req_ext = 'R04',
      arglst = list(),
      setup = ocas::s(verbose = verbose))
    
    message("INFO: create weekly Wednesday return timeseries")
    retXts.l <- lapply(1:nrow(usedProxyTs.tib), ocsrri::createWeeklyReturnTs, usedProxyTs.tib, tsxts.l, li, lix)
    names(retXts.l) <- usedProxyTs.tib[["iid"]]
    
    message("INFO: compute SRRI")
    result.l <- lapply(names(retXts.l), ocsrri::computeOC_SRRI, retXts.l, asOfDate)
    names(result.l) <- names(retXts.l)
    
    message("INFO: assign proxy SRRI to investments with proxies requiring a timeseries")
    srri.v <- sapply(result.l, function(x)return(as.character(x$srri)))
    
    tsNeeded.tib[["SRRI_proxy"]] <- srri.v[ tsNeeded.tib[["iid_proxy"]] ]
    message(
      paste(tsNeeded.tib$iid," ", tsNeeded.tib$type, " -> ",
        tsNeeded.tib$iid_proxy, " ", tsNeeded.tib$type_proxy, " --srri--> ", tsNeeded.tib$SRRI_proxy, "\n", sep = "")
    )
    return(result.l)
  }
  
  noChange.tib <- dplyr::filter(noTsNeeded.tib, .data[['iid_proxy']] == 'no_change')
  if (nrow(noChange.tib) == 1) { 
    message("INFO: identified investment that does not need a change in srri")
    noChange.tib[["SRRI_proxy"]] <- "not changed"
    current_srri <- ocalc::get_inv_rating_as_of(id = iid, rating_agency = "SRRI_OC", dt = asOfDate, li = li)
    message(
      paste(noChange.tib$iid," ", noChange.tib$type, " -> ",
        noChange.tib$iid_proxy, " ", noChange.tib$type_proxy, " --srri--> ", noChange.tib$SRRI_proxy, "\n", sep = "")
    )
    message(
      paste("Current SRRI rating: ", current_srri, "\n", sep = "")
    )
    srri_v <- current_srri
    names(srri_v) <- dplyr::pull(noChange.tib, "ISIN")[1]
  }
  lipper.tib <- dplyr::filter(noTsNeeded.tib, .data[['iid_proxy']] == 'srri_lipper')
  if (nrow(lipper.tib) == 1) {
    message("INFO: identifying investments with srri from lipper")
    lipper.tib[["SRRI_proxy"]] <- "from lipper"
    current_srri <- ocalc::get_inv_rating_as_of(id = iid, rating_agency = "SRRI_OC", dt = asOfDate, li = li)
    lipper_xls_srri_v <- readLipperXls(fname = paste0('//rackstation/MarcoShare/Per Panos/Lipper_richieste_srri/lista lipper srri fondi ', asOfDate,'.xlsx'),
      verbose = TRUE)
    ??
    message(
      paste(lipper.tib$iid," ", lipper.tib$type, " -> ",
        lipper.tib$iid_proxy, " ", lipper.tib$type_proxy, " --srri--> ", lipper.tib$SRRI_proxy, "\n", sep = "")
    )
    message(
      paste("Current SRRI rating: ", current_srri, "\n", sep = "")
    )
    message("INFO: identifying investments with static assigned srri")
    ??
      staticSrri.tib <- dplyr::filter(noTsNeeded.tib, .data[['iid_proxy']] != 'no_change' &
          .data[['iid_proxy']] != 'srri_lipper')
    staticSrri.tib[['SRRI_proxy']] <- gsub("^srri_", "", staticSrri.tib[['iid_proxy']])
    ## replace NA with maximal available srri, i.e. 7
    staticSrri.tib[['SRRI_proxy']] <- gsub("^NA$", "7", staticSrri.tib[['SRRI_proxy']])
    message(
      paste(staticSrri.tib$iid," ", staticSrri.tib$type, " -> ",
        staticSrri.tib$iid_proxy, " ", staticSrri.tib$type_proxy, " --srri--> ", staticSrri.tib$SRRI_proxy, "\n", sep = "")
    )
  }
  browser()
  5+7
  
  3+9
  
}