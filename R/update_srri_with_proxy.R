#' Update the SRRI of investiment object with the SRRI of the proxy if defined.
#'
#' Update the SRRI of investiment object with the SRRI of the proxy if defined.
#'
#' @param li_date the date of the li(yyyymmdd).RData file from which the li list
#'        (list of investments) is constructed. This date determines the .RData file
#'        used to filter the fund and similar investments whose ISIN is saved in the
#'        output csv file to be imported in Lipper. li_date must be a date.
#'
#' @param push should the result be pushed to Allocare. Default to FALSE. Functionality not
#'        working yet. Function stops with error message if push = TRUE.
#'
#' @param outDir The output directory where to write the 'invalidMapElements.RData' file.
#'        Defaults to ".".
#'
#' @param verbose Should the INFO messages be displayed? Defaults to FALSE.
#'
#' @return a named list with the following tibbles: tsNeeded.tib, staticSrri.tib,
#'                 noChange.tib, lipper.tib.
#'
#'
#' @examples
#' \dontrun{
#'	result_l <- update_srri_with_proxy(li_date = Sys.Date() - 1, push = FALSE,
#'              outDir = ".", verbose = TRUE)
#' }
#' @author Claudio Ortelli
#' @export


update_srri_with_proxy <- function(li_date, push = FALSE, outDir = ".", verbose = FALSE) {


  asOfDate <- lubridate::today('UTC')

  assertthat::assert_that(inherits(li_date, 'Date'))
  li_date_str <- format(li_date, format = '%Y%m%d')

  message('INFO: loading file of active instruments')
  li <- ocutil::load_single_var(file.path(ocutil::default_dir('allocare_dir'), paste0('li', li_date_str, '.RData')))

  ## extract proxy used by the active investments
  message("INFO: extract historical inv -> proxy map from li_withProxy$P42 (assuming i.o. of type index do not have proxy!)")
  map.tib <- ocsrri::getProxyMap(li)

  ## extract all startDate and Expiry
  message("INFO: create vector of relevant dates for proxy's consistency check")
  relevantDate.v <- sort(na.omit(unique(c(map.tib$StartDate, map.tib$Expiry))))
  if (length(relevantDate.v) > 0) {
    relevantDate.v <- c(min(relevantDate.v) - 1, relevantDate.v, max(relevantDate.v) + 1)
  } else {
    relevantDate.v <- asOfDate
  }

  invalidMapElements <- function(asOfDate.v, map.tib, li) {

    invMapEl.l <- lapply(asOfDate.v, ocsrri::proxyConsistencyCheck, map.tib, li)
    names(invMapEl.l) <- as.Date(asOfDate.v)

    ## remove good dates
    isGood.v <- sapply(invMapEl.l, nrow) == 0

    return(invMapEl.l[!isGood.v])
  }

  message("INFO: apply consistency check on the relevant dates")
  invMapEl.l <- invalidMapElements(relevantDate.v, map.tib, li)
  if (length(invMapEl.l) > 0) {
    fpath <- file.path(outDir, "invalidMapElements.RData")
    save(invMapEl.l, file = fpath)
    message(paste("Info: there are dates at which the validity check of the proxy Map results in invalid elements.\nOutput stored in ", fpath))
    stop("SRRI calculation stopped. See 'invalidMapElements.RData' in current working directory.")
  }

  ## extract the current iid -> iid_proxy map
  message(paste("INFO: construct inv -> proxy map as of", asOfDate))
  asOfMap.tib <- ocsrri::proxyMapByTimeRange(asOfDate, ndays = 0L, map.tib)

  message("Info: assigning the correct proxy for 'by_rat_mat' method")
  finalMap.tib <- ocsrri::adjustDynamicProxy(asOfMap.tib, asOfDate, li)

  ## inform about the type of the used proxy (investment or index)
  message("INFO: the following map will be used:")
  message(
    paste(finalMap.tib$iid, " ", finalMap.tib$type, " -> ",
      finalMap.tib$iid_proxy, " ", finalMap.tib$type_proxy, "\n", sep = "")
  )

  message("INFO: extract 'srri_lipper', 'no_change', 'srri_i' i = 1,...,7, NA proxies from the map for later processing")
  noTsNeeded.tib <- dplyr::filter(finalMap.tib, ocsrri::noTsNeeded(finalMap.tib))
  tsNeeded.tib <-  dplyr::filter(finalMap.tib, !ocsrri::noTsNeeded(finalMap.tib))

  message("INFO: replace iid_proxy 'own_ts' with the iid of the investment")
  tsNeeded.tib[['iid_proxy']][ tsNeeded.tib[['iid_proxy']] == 'own_ts' ] <- tsNeeded.tib[['iid']][ tsNeeded.tib[['iid_proxy']] == 'own_ts']

  message("INFO: create the tib with the <iid_proxy, type_proxy> fields for TS creation")
  usedProxyTs.tib <- unique(tsNeeded.tib[, c("iid_proxy", "type_proxy")])
  message("INFO: rename used proxy columns <iid_proxy, type_proxy> -> <iid, type>")
  colnames(usedProxyTs.tib) <- c("iid", "type")

  message("INFO: get the default 'Closing' timeseries of the used proxies (investment and indices)")
  ts.tib <- ococpu::smart('ocwalc', 'get_default_ts')(info = usedProxyTs.tib, setup = ocas::s(verbose = verbose))

  message("INFO: split default timeseries by 'Idtf', 'TSType' and 'PriceCcy' ...")
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

  message("INFO: identifying investments that do not need a change in srri")
  noChange.tib <- dplyr::filter(noTsNeeded.tib, .data[['iid_proxy']] == 'no_change')
  noChange.tib[["SRRI_proxy"]] <- "not changed"
  message(
    paste(noChange.tib$iid," ", noChange.tib$type, " -> ",
      noChange.tib$iid_proxy, " ", noChange.tib$type_proxy, " --srri--> ", noChange.tib$SRRI_proxy, "\n", sep = "")
  )

  message("INFO: identifying investments with srri from lipper")
  lipper.tib <- dplyr::filter(noTsNeeded.tib, .data[['iid_proxy']] == 'srri_lipper')
  lipper.tib[["SRRI_proxy"]] <- "from lipper"
  message(
    paste(lipper.tib$iid," ", lipper.tib$type, " -> ",
      lipper.tib$iid_proxy, " ", lipper.tib$type_proxy, " --srri--> ", lipper.tib$SRRI_proxy, "\n", sep = "")
  )

  message("INFO: identifying investments with static assigned srri")
  staticSrri.tib <- dplyr::filter(noTsNeeded.tib, .data[['iid_proxy']] != 'no_change' &
      .data[['iid_proxy']] != 'srri_lipper')
  staticSrri.tib[['SRRI_proxy']] <- gsub("^srri_", "", staticSrri.tib[['iid_proxy']])
  ## replace NA with maximal available srri, i.e. 7
  staticSrri.tib[['SRRI_proxy']] <- gsub("^NA$", "7", staticSrri.tib[['SRRI_proxy']])
  message(
    paste(staticSrri.tib$iid," ", staticSrri.tib$type, " -> ",
      staticSrri.tib$iid_proxy, " ", staticSrri.tib$type_proxy, " --srri--> ", staticSrri.tib$SRRI_proxy, "\n", sep = "")
  )

  if (push) {
    stop("This step must be verified because currently the push goes to MARIO")
    push.tib <- rbind(tsNeeded.tib, staticSrri.tib)

    message("INFO: push investment object with ts or static computed SRRI results to Allocare Portfolio")
    pushInv.tib41 <- tibble::tibble(
      '#EditFunct' = 'U', 'InvObjType' = '1',
      'Idtf' = push.tib$iid, 'IdtfSys' = 'INTERNAL_ID',
      'RatingAgency' = 'SRRI_OC', 'RatingCode' = push.tib[['SRRI_proxy']],
      'StartDate' = format(asOfDate, format = '%Y%m%d'),
      'Expiry' = NA_character_)

    ococpu::smart('ocwalc', 'push')(
      req_ext = 'P41', arglst = pushInv.tib41,
      ocas::s(odbc_service = 'MARIO', verbose = TRUE))

    message("INFO: push used index proxy SRRI to Allocare Portfolio")
    pushIdx.tib41 <- dplyr::filter(push.tib,
                                   .data[["iid_proxy"]] != "no_change",
                                   !grepl('^srri_[1-7]$', .data[['iid_proxy']]),
                                   .data[["type_proxy"]] == "idx")
    pushIdx.tib41 <- pushIdx.tib41[, c("iid_proxy", "SRRI_proxy")]

    pushIdxtib41 <- tibble::tibble(
      '#EditFunct' = 'U', 'InvObjType' = '8',
      'Idtf' = pushIdx.tib41$iid_proxy, 'IdtfSys' = 'INTERNAL_ID',
      'RatingAgency' = 'SRRI_OC', 'RatingCode' = pushIdx.tib41[['SRRI_proxy']],
      'StartDate' = format(asOfDate, format = '%Y%m%d'),
      'Expiry' = NA_character_)

    ococpu::smart('ocwalc', 'push')(
      req_ext = 'P41', arglst = pushIdxtib41,
      ocas::s(odbc_service = 'MARIO', verbose = TRUE))
  }

  return(list(tsNeeded.tib = tsNeeded.tib, staticSrri.tib = staticSrri.tib,
      noChange.tib = noChange.tib, lipper.tib = lipper.tib))
}
