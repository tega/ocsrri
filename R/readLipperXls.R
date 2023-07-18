#' Read the Excel output file from Lipper with the fund's SRRI
#'
#' Read the Excel output file from Lipper with the fund's SRRI
#'
#' @param fname the xls file name to import.
#'
#' @param verbose logical equal TRUE if INFO message should be printed
#'
#' @return a tibble with the (Lipper) srri and ISIN. The two colnames are 'srri' and 'ISIN'.
#'
#' @export

readLipperXls <- function(fname, verbose = TRUE) {
  
  if (verbose) message("INFO: acquiring Lipper srri")
  xldata <- readxl::read_xlsx(fname, skip = 2L, col_names = FALSE)
  checkmate::assert_true(ncol(xldata) == 6) 
  srri_isin <- xldata[, c(3,4,5,6)] %>% magrittr::set_names(c('srri', 'ISIN', 'assetType', 'status')) %>% dplyr::distinct()
  if (verbose) message("INFO: remove 'Asset Status = Liquidated' funds from imported universe")
  srri_isin_1 <- srri_isin %>% dplyr::filter(.data$status != 'Liquidated')
  if (verbose) message("INFO: search for duplicated <srri, ISIN> pairs")
  dup_srri_isin.v <- srri_isin_1[duplicated(srri_isin_1[['ISIN']]), ][['ISIN']]
  
  if (length(dup_srri_isin.v) > 0) {
    if (verbose) message("INFO: found duplicated <srri, ISIN> pairs")
    duplicated_tib <- srri_isin_1 %>% dplyr::filter(.data[['ISIN']] %in% dup_srri_isin.v)
    if (verbose) message("INFO: remove NA 'Asset Type' from duplicated" )
    cleaned_dup.tib <- duplicated_tib %>% dplyr::filter(!is.na(.data[["assetType"]]))
    removed_dup.tib <-  duplicated_tib %>% dplyr::filter(is.na(.data[["assetType"]]))
    dup_dup_srri_isin.v <- cleaned_dup.tib[duplicated(cleaned_dup.tib[['ISIN']]),] [['ISIN']]
    if (length(dup_dup_srri_isin.v) == 0) {
      if (verbose) message("INFO: removed 'Asset Type' = NA and no further duplicated <srri, ISIN> pairs found")  
    } else {
      if (verbose) message("INFO: after removing 'Asset Type' = NA duplicated <srri, ISIN> pairs remains!")
      stop("Duplicated <srri, ISIN> pairs in imported Lipper universe. Manual intervention required.")
    }
  }
  
  srri_isin_2 <- srri_isin_1 %>% dplyr::filter(
    !is.element(.data[['ISIN']], cleaned_dup.tib[["ISIN"]]))
  srri_isin_2 <- rbind(srri_isin_2, cleaned_dup.tib)
  srri_isin_2 <- srri_isin_2[, c("srri", "ISIN")] %>% dplyr::distinct()
  
  
  return(srri_isin_2)
}
