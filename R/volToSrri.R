#' Map yearly realized volatility to SRRI
#'
#' Map yearly realized volatility to SRRI
#'
#' @param yearlyVol the yearly realized volatility computed with weekly returns over a 5 years period (260 obs.).
#'  
#' @return the SRRI figure. See CERs Guidelines 10-673 on the methodology for the calculation of the synthetic risk and reward indicator.
#'
#' @examples
#' srri <- volToSrri(0.15)
#'  
#' @export
volToSrri <- function(yearlyVol) {
  
  if (is.na(yearlyVol)) return(yearlyVol)
  
  if (yearlyVol <= 0.005) return(1L)
  if (yearlyVol <= 0.02 ) return(2L)
  if (yearlyVol <= 0.05 ) return(3L)
  if (yearlyVol <= 0.10 ) return(4L)
  if (yearlyVol <= 0.15 ) return(5L)
  if (yearlyVol <= 0.25 ) return(6L)
  return(7L)
}
