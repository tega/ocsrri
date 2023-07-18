#' Compute and push the SRRI to Allocare AMS
#'
#' Compute and push the SRRI to Allocare AMS
#'
#' @param li_date the date of the li(yyyymmdd).RData file from which the li list
#'        (list of investments) is constructed. This date determines the .RData file
#'        used to filter the fund and similar investments whose ISIN is saved in the
#'        output csv file to be imported in Lipper. li_date must be a date.
#'
#' @param lipper_xlsx_date the date identifying the xlsx file to load. By convention
#'        the file name must be equal to 'lista lipper srri fondi yyyy-mm-dd.xlsx'.
#'
#' @param lipper_srri_dir the directory where to search for the lipper xlsx file-
#'
#' @param dryrun if TRUE the data are not push to Allocare.
#'
#' @return a named list with the following slots:
#'     p41.tib (the data pushed to AMS), computed.tib, fund.tib, cash.tib, ccyFwd.tib, unassigned.tib
#'
#' @examples
#' \dontrun{
#'	result_l <- push_srri(Sys.Date(), Sys.Date() - 2, lipper_srri_dir = ".", dryrun = TRUE)
#' }
#' @author Claudio Ortelli
#' @export

push_srri <- function(
  li_date,
  lipper_xlsx_date,
  lipper_srri_dir = '//rackstation/MarcoShare/Per Panos/Lipper_richieste_srri',
  dryrun = FALSE) {

  ## ----------------------------------
  today <- lubridate::today('UTC')

  li <- load_li_rdata(li_date)
  li[["P01"]] <- dplyr::filter(li[["P01"]], .data[["InvStatus"]] == 1)
  p01 <- li[['P01']]
  #p42 <- li[['P42']]

  cleanLipper.l <- cleanLipperIsin(get_lipper_funds(today, li), TRUE)

  ## step 1
  message("INFO: compute SRRI for investments with OC_SRRI_proxy")
  withProxyDefined.l <- update_srri_with_proxy(li_date, push = FALSE, outDir = ".", verbose = FALSE)
  withProxyDefined.tib <- do.call(rbind, withProxyDefined.l)
  withProxyDefined.tib[["iid"]]
  computed_iid.v <- withProxyDefined.tib[["iid"]][ withProxyDefined.tib[["SRRI_proxy"]] != "not changed" &
      withProxyDefined.tib[["SRRI_proxy"]] != "from lipper"]
  computed_srri.v <- withProxyDefined.tib[["SRRI_proxy"]] [ withProxyDefined.tib[["SRRI_proxy"]] != "not changed"  &
      withProxyDefined.tib[["SRRI_proxy"]] != "from lipper"]

  ## step 2
  message("INFO: acquiring Lipper srri")
  lipper_xls_file <- file.path(
    lipper_srri_dir,
    paste0(
      'lista lipper srri fondi ',
      lipper_xlsx_date,
      '.xlsx'
    )
  )
  xldata <- readxl::read_xlsx(lipper_xls_file, skip = 2L, col_names = FALSE)
  checkmate::assert_true(ncol(xldata) == 6)
  srri_isin <- xldata[, c(3,4,5,6)] %>% magrittr::set_names(c('srri', 'ISIN', 'assetType', 'status')) %>% dplyr::distinct()
  message("INFO: remove 'Asset Status = Liquidated' funds from imported universe")
  srri_isin_1 <- srri_isin %>% dplyr::filter(.data$status != 'Liquidated')
  message("INFO: search for duplicated <srri, ISIN> pairs")
  dup_srri_isin.v <- srri_isin_1[duplicated(srri_isin_1[['ISIN']]), ][['ISIN']]

  if (length(dup_srri_isin.v) > 0) {
    message("INFO: found duplicated <srri, ISIN> pairs")
    duplicated.tib <- srri_isin_1 %>% dplyr::filter(.data[['ISIN']] %in% dup_srri_isin.v)
    message("INFO: remove NA 'Asset Type' from duplicated" )
    cleaned_dup.tib <- duplicated.tib %>% dplyr::filter(!is.na(.data[["assetType"]]))
    removed_dup.tib <-  duplicated.tib %>% dplyr::filter(is.na(.data[["assetType"]]))
    dup_dup_srri_isin.v <- cleaned_dup.tib[duplicated(cleaned_dup.tib[['ISIN']]),] [['ISIN']]
    if (length(dup_dup_srri_isin.v) == 0) {
      message("INFO: removed 'Asset Type' = NA and no further duplicated <srri, ISIN> pairs found")
    } else {
      message("INFO: after removing 'Asset Type' = NA duplicated <srri, ISIN> pairs remains!")
      stop("Duplicated <srri, ISIN> pairs in imported Lipper universe. Manual intervention required.")
    }
  }

  srri_isin_2 <- srri_isin_1 %>% dplyr::filter(
    !is.element(.data[['ISIN']], cleaned_dup.tib[["ISIN"]]))
  srri_isin_2 <- rbind(srri_isin_2, cleaned_dup.tib)
  srri_isin_2 <- srri_isin_2[, c("srri", "ISIN")] %>% dplyr::distinct()


  srri_isin_internal <- srri_isin_2 %>% dplyr::left_join(p01[, c('ISIN', 'Idtf5', 'Idtf4', 'InstrCcy')], by = 'ISIN')
  dup_srri <- srri_isin_internal[duplicated(srri_isin_internal[['ISIN']]), ]
  all_dup_srri <- srri_isin_internal %>% dplyr::filter(.data[['ISIN']] %in% dup_srri[['ISIN']]) %>% dplyr::arrange(.data[['ISIN']])

  checkmate::assert(all(!is.na(srri_isin_internal[['Idtf5']])))
  checkmate::assert(all(!is.na(srri_isin_internal[['srri']])))

  today_ymd <- format(lubridate::today('UTC'), format = '%Y%m%d')
  message("INFO: identify funds with srri_lipper but invalid srri value")
  fundWithoutValidLipperSRRI.tib <- cleanLipper.l$isin.tib %>% dplyr::filter(!is.element(.data[["INTERNAL_ID"]], srri_isin_internal[['Idtf5']]))
  srri_isin_internal <- srri_isin_internal %>% dplyr::filter(!is.element(.data[['Idtf5']], computed_iid.v))
  fund_iid.v <- as.character(srri_isin_internal[['Idtf5']])
  fund_srri.v <- as.character(srri_isin_internal[['srri']])

  ## step 3
  message("INFO: filtering 'Cash Accounts' from P01 tib")
  cash.tib <- p01[p01[['InvStatus']] == '1' & p01[['InvType']] == 'Cash Accounts', ]
  cash.tib <- cash.tib %>% dplyr::filter(!is.element(.data[['Idtf5']], computed_iid.v))
  cash_iid.v <- cash.tib[['Idtf5']]
  cash_srri.v <- rep_len('1', length(cash_iid.v))
  active_iid.v <- setdiff(p01[p01[['InvStatus']] == '1', ][['Idtf5']], computed_iid.v)


  ## step 4
  message("INFO: filtering 'Currency Forwards' from P01 tib")
  fwd.tib <- p01[p01[['InvStatus']] == '1' & p01[['InvType']] == 'Currency Forwards', ]
  fwd.tib <- fwd.tib %>% dplyr::filter(!is.element(.data[['Idtf5']], computed_iid.v))
  valid_fwd.tib <- fwd.tib %>% dplyr::filter(grepl("^(?:FS|FB) [A-Z]{3}/[A-Z]{3}", .data[["ShortName"]]))

  ccy1.v <- substr(valid_fwd.tib[["ShortName"]], 4, 6)
  ccy2.v <- substr(valid_fwd.tib[["ShortName"]], 8, 10)
  isCHF1.v <- ccy1.v == "CHF"
  isCHF2.v <- ccy2.v == "CHF"
  ccy.tib <- unique(tibble::tibble('Quoted Currency' = ccy1.v, Currency = ccy2.v))

  ## fx w.r.t. CHF
  isSimple.v <- isCHF1.v | isCHF2.v
  isDouble.v <- !isSimple.v

  involvedFx.v <- unique(c(ccy1.v[!isCHF1.v], ccy2.v[!isCHF2.v]))
  lts <- ococpu::smart('ocwalc', 'get_list')(
    req_ext = 'R07',
    arglst = tibble::tibble('Request type' = 2, 'Time Series Type' = 3, 'Quoted currency' = 'CHF',
      'Currency' = involvedFx.v, 'TS search mode' = 2),
    ocas::s(verbose = TRUE))

  ts.tib <- lts[["P06"]][, c("Ccy", "TSType", "PriceDate", "Price", "PriceType")]
  colnames(ts.tib) <- c("Idtf", "TSType", "PriceDate", "Price", "PriceType")
  ts.tib[['PriceCcy']] <- 'CHF'
  ts.tib[['PriceDate']] <- lubridate::ymd(ts.tib$PriceDate)

  xts.l <- ocalc::splitByFactor(ts.tib, factor.v = c("Idtf", "TSType", "PriceCcy"), asXts = TRUE)


  getTs <- function(x.v, xts.l) {
    # x.v is a named vector
    ## if 'Quoted Currency' == 'CHF' -> take from lts the 'Currency' ts
    ## else if 'Currency' == 'CHF' -> take from lts the 'Quoted Currency' and compute 1/x
    ## else take from lts the Currency and Quoted Currency and compute currency / Quoted currency

    if (x.v[['Quoted Currency']] == 'CHF') {
      assertthat::assert_that(is.element(x.v[['Currency']], names(xts.l)))
      return(xts.l[[ x.v[['Currency']] ]]$Closing$CHF)
    } else if (x.v[['Currency']] == 'CHF') {
      assertthat::assert_that(is.element(x.v[['Quoted Currency']], names(xts.l)))
      return(1 / xts.l[[ x.v[['Quoted Currency']] ]]$Closing$CHF)
    } else {
      assertthat::assert_that(is.element(x.v[['Currency']], names(xts.l)))
      assertthat::assert_that(is.element(x.v[['Quoted Currency']], names(xts.l)))
      return(xts.l[[ x.v[['Currency']] ]]$Closing$CHF / xts.l[[ x.v[['Quoted Currency']] ]]$Closing$CHF)
    }
  }


  fxTs.l <- apply(ccy.tib, 1, getTs, xts.l)
  names(fxTs.l) <- paste(ccy.tib$`Quoted Currency`, ccy.tib$Currency, sep = "/")

  message("INFO: create weekly Wednesday return timeseries")
  createWeeklyReturnTs_simple <- function(i, ts.l) {
    # wts.xts <- ocTsutils::complete_nwe_locf_to_weekly(ts.l[[i]], wday = 3)
    # wRet.xts <- diff(log(wts.xts))[-1, ]
    wRet.xts <- ocTsutils::to_weekly_log_returns(ts.l[[i]], wday = 3L, skip_first = TRUE)
    xts::xtsAttributes(wRet.xts) <- list(Ccy = names(ts.l)[[i]], ts_type = "Closing")
    return(wRet.xts)
  }

  retXts.l <- lapply(1:length(fxTs.l), createWeeklyReturnTs_simple, fxTs.l)
  names(retXts.l) <- names(fxTs.l)

  message("INFO: compute and assign SRRI to currency forward investments")
  srri.l <- lapply(names(retXts.l), ocsrri::computeOC_SRRI, retXts.l, today)
  names(srri.l) <- names(retXts.l)

  srri.v <- sapply(srri.l, function(x)return(as.character(x$srri)))
  assertthat::assert_that(!any(is.na(srri.v)))

  srriMap.tib <- valid_fwd.tib[, c("ShortName","Idtf5")]
  srriMap.tib[["srri"]] <- srri.v[substr(srriMap.tib[["ShortName"]], 4, 10)]
  colnames(srriMap.tib) <- c("ShortName", "iid", "srri")

  ccyFwd_iid.v <- as.character(srriMap.tib[["iid"]])
  ccyFwd_srri.v <- as.character(srriMap.tib[["srri"]])


  ## step 5
  message("INFO: identifying investments with unassigned SRRI")
  assigned_iid.v <-  c(computed_iid.v,  fund_iid.v,  cash_iid.v,  ccyFwd_iid.v)
  assigned_srri.v <- c(computed_srri.v, fund_srri.v, cash_srri.v, ccyFwd_srri.v)
  unassigned_iid.v <- setdiff(active_iid.v, assigned_iid.v)

  message("INFO: assigning maximal SRRI to investmetns with unsassigned SRRI")
  unassigned_srri.v <- rep_len('7', length(unassigned_iid.v))

  message("INFO: aggregating results in a final tibble")
  push_iid.v <-  c(assigned_iid.v,  unassigned_iid.v)
  push_srri.v <- c(assigned_srri.v, unassigned_srri.v)
  p41_pushed.tib <- tibble::tibble('#EditFunct' = 'U', 'InvObjType' = '1', 'Idtf' = push_iid.v, 'IdtfSys' = 'INTERNAL_ID',
    'RatingAgency'='SRRI_OC', 'RatingCode' = push_srri.v, 'StartDate' = today_ymd, 'Expiry' = NA_character_)


  ## liold <- ococpu::smart('ocwalc', 'get_list')(
  ##   req_ext = 'R01',
  ##   arglst = tibble::tibble('Request type' = '12', 'Investment status' = '1'),
  ##   setup = ocas::s(ini_file = '0_Export_INTERNAL_ID_only_P41.ini', verbose = TRUE))

  if (!dryrun) {
    ococpu::smart('ocwalc', 'push')(
      req_ext = 'P41',
      arglst = p41_pushed.tib,
      setup = ocas::s(verbose = TRUE))
  }

  result <- list(
    p41.tib = p41_pushed.tib,
    computed.tib = dplyr::tibble(INTERNAL_ID = computed_iid.v, SRRI = computed_srri.v),
    fund.tib = dplyr::tibble(INTERNAL_ID = fund_iid.v, SRRI = fund_srri.v),
    cash.tib = dplyr::tibble(INTERNAL_ID = cash_iid.v, SRRI = cash_srri.v),
    ccyFwd.tib = dplyr::tibble(INTERNAL_ID = ccyFwd_iid.v, SRRI = ccyFwd_srri.v),
    unassigned.tib = dplyr::tibble(INTERNAL_ID = unassigned_iid.v, SRRI = unassigned_srri.v)
  )
  return(result)
}
