#' Extracts the ISIN code of the investments available in Lipper
#'
#' Extracts the ISIN code of the investments available in Lipper
#'
#' @param li the list of investments information with P01 tibble.
#' 
#' @param asOfDate the asOfDate to be used to determine the list of available investment types.
#' 
#' @param withInternalId logical indicating if the INTERNAL_ID should be included in the output
#' @return the character vector of ISIN codes of the available investments or a tibble with 
#' INTERNAL_ID and ISIN
#' 
#' @examples
#' li <- list(P01 = tibble::tibble(Description = "Stock Funds", ISIN = "LU0546265769"))
#' extractAvailableInLipper(li, as.Date("2021-09-20"))
#'  
#' @export

extractAvailableInLipper <- function(li, asOfDate, withInternalId = FALSE) {
  
  ## get Lipper map 
  data.df <- createLipperInvestmentTypes(asOfDate)
  isElement.v <- is.element(li[["P01"]][["InvType"]], data.df[["Description"]])
  
  if (!all(isElement.v)) {
    missing.v <- unique(li[["P01"]][["InvType"]][!isElement.v])
    message <- paste0(missing.v, collapse = ", ")
    message <- paste("Following inv. types are unknown:", message)
    stop(message)
  }
  
  invTypeInLipper.v <- data.df[["Description"]] [ as.logical(data.df[["InLipper"]]) ]
  inLipper.v <- is.element(li[["P01"]][["InvType"]], invTypeInLipper.v)
  
  if (withInternalId) {
    result <- li[["P01"]][inLipper.v, c("Idtf5", "ISIN")]
    colnames(result) <- c("INTERNAL_ID", "ISIN")
  } else {
    result <- li[["P01"]][["ISIN"]][inLipper.v]
  }
  
  return(result)
}
