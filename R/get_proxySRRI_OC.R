#' Get the SRRI_OC of the proxy assigned to an I.O.
#'
#' Get the SRRI_OC value of the proxy assigned to 
#' an investment object (I.O.) or NA if the I.O. 
#' does not have a proxy.
#'
#' @param id investment object id (string).
#' @param asOfDate the as of date for the SRRI_OC.
#' @param li list of Pxx tibbles of investments data.
#' @param lix list of Pxx tibbles of index data.
#'
#' @return The SRRI_OC value (character) of the proxy linked to the I:O.
#'
#' @examples
#'  lix <- list('P41' = tibble::tibble('Idtf' = '38668', 'RatingAgency' = 'SRRI_OC',
#'    'RatingCode' = '5', 'StartDate' = as.Date('2021-01-09'), 'Expiry' = as.Date(NA_character_)))
#' 
#'  li <- list('P42' = tibble::tibble('Idtf' = '12345', 'IndClass' = 'OC_SRRI_proxy',
#'    'IndCode' = '38668', 'StartDate' = as.Date('2021-01-09'), 'Expiry' = as.Date(NA_character_)),
#'      'P41' = tibble::tibble('Idtf' = '552234', 'RatingAgency' = 'SRRI_OC',
#'         'RatingCode' = '7', 'StartDate' = as.Date('2021-01-09'), 'Expiry' = as.Date(NA_character_)))
#' 
#' 
#'  get_proxySRRI_OC('12345', as.Date('2021-01-09'), li, lix)
#' @export


get_proxySRRI_OC <- function(id, asOfDate, li, lix) {

  proxyIntId <- ocalc::get_inv_classif_as_of(id = id, ind_class = "OC_SRRI_proxy", 
                                            dt = asOfDate, li = li)
  
  if (is.na(proxyIntId)) return(proxyIntId)
  
  ## try in the i.o. list
  proxySRRI <- ocalc::get_inv_rating_as_of(id = proxyIntId, 
                                           rating_agency = 'SRRI_OC',
                                           dt = asOfDate,
                                           li = li)
  if (!is.na(proxySRRI)) return(proxySRRI)
  
  proxySRRI <- ocalc::get_inv_rating_as_of(id = proxyIntId, 
                                           rating_agency = 'SRRI_OC',
                                           dt = asOfDate,
                                           li = lix)
  
  return(proxySRRI)
}
