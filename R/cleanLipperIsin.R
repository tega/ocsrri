#' Clean the tibble with the ISIN codes from NA and invalid ISIN
#'
#' Clean the tibble with the ISIN codes from NA and invalid ISIN
#'
#' @param isin.tib a tibble with the ISIN and INTERNAL_ID columns.
#'
#' @param verbose logical equal TRUE if INFO message should be printed
#'
#' @return a named list with the following three fields:
#'
#' 		- isin.tib
#'    - wrongIsin.tib
#'    - naIsin.tib
#'
#' @export

cleanLipperIsin <- function(isin.tib, verbose = TRUE) {

  naIsin.tib <- dplyr::filter(isin.tib, is.na(.data[["ISIN"]]))
  if (nrow(naIsin.tib) > 0) {
    if (verbose) {
      msg <- paste0(naIsin.tib[["INTERNAL_ID"]], collapse = ", ")
      msg <- paste("INFO: inv. with ISIN = NA detected:", msg, "\n\n")
      message(msg)
    }
    isin.tib <- dplyr::filter(isin.tib,
        !is.element(.data[["INTERNAL_ID"]], naIsin.tib[["INTERNAL_ID"]]))
  }

  wrongIsin.v <- !figir::isin_check(isin.tib[["ISIN"]])
  wrongIsin.tib <- isin.tib[wrongIsin.v,]
  if (nrow(wrongIsin.tib) > 0) {
    if (verbose) {
      msg <- paste(c("INTERNAL_ID", wrongIsin.tib[["INTERNAL_ID"]]), 
          c("ISIN", wrongIsin.tib[["ISIN"]]), sep = ",", collapse = "\n")
      msg <- paste("INFO: inv. with invalid ISIN detected:\n", msg, "\n\n", sep = "")
      message(msg)
    }
    isin.tib <- isin.tib[!wrongIsin.v, ]
  }

  if (verbose) {
    msg <- paste(c("INTERNAL_ID", isin.tib[["INTERNAL_ID"]]), 
        c("ISIN", isin.tib[["ISIN"]]), sep = ",", collapse = "\n")
    msg <- paste("INFO: Valid ISIN for Lipper:\n", msg, "\n\n", sep = "")
    message(msg)
  }

  return(list(isin.tib = isin.tib, wrongIsin.tib = wrongIsin.tib, naIsin.tib = naIsin.tib))
}
