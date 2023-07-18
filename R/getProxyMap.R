#' Get the map between iid and iid_proxy
#'
#' Get the map between iid and iid_proxy
#' 
#' @param li the 'list of investments' containing the P42 tibble. 
#'
#' @description This function constructs the map between the investment objects and the corresponding 
#' proxy used in SRRI calculation. The 'li' list of instruments argument contains all information available at a given point in time. Usually
#' the 'li' is the current (today) list of investment objects.
#' 
#' @return a tibble with 'iid', 'type', 'iid_proxy', 'type_proxy', 'StartDate', 'Expiry' columns
#' where iid stands for INTERNAL_ID and 'type' denotes the type of the investment, i.e. `inv` (investment) or
#' `idx` (index).
#'
#' @export
getProxyMap <- function(li) {
  
  # P42.tib the P42 tib with columns 'Idtf', 'IndCode', 'IndClass', 'StartDate', 'Expiry'
  P42.tib <- dplyr::filter(li$P42[, c('Idtf', 'IndCode', 'IndClass', 'StartDate', 'Expiry')], 
      .data[['IndClass']] == 'OC_SRRI_proxy')
  if (nrow(P42.tib) == 0) {
    result.tib <- tibble::tibble(iid = character(0), type = character(0), iid_proxy = character(0),
        type_proxy = character(0), StartDate = as.Date(NA)[-1], Expiry = as.Date(NA)[-1])
    return(result.tib)
  }
 
  colnames(P42.tib) <- c('iid', 'iid_proxy', 'IndClass', 'StartDate', 'Expiry')
  P42.tib[['type']] <- 'inv'
  P42.tib[['type_proxy']] <- NA_character_
  
  ## replace type_proxy for method 'by_rat_mat'
  is.by_rat_mat.v <- P42.tib[['iid_proxy']] == 'by_rat_mat'
  P42.tib[['type_proxy']][is.by_rat_mat.v] <- 'idx'
  P42.tib[['type_proxy']][grepl('^inv_', P42.tib[['iid_proxy']])] <- 'inv'
  P42.tib[['type_proxy']][grepl('^idx_', P42.tib[['iid_proxy']])] <- 'idx'
  
  ## replace type_proxy for method 'no_change'
  is.no_change.v <- P42.tib[['iid_proxy']] == 'no_change'
  P42.tib[['type_proxy']][is.no_change.v] <- P42.tib[['type']][is.no_change.v]

  ## replace type_proxy for method 'srri_lipper'
  is.no_change.v <- P42.tib[['iid_proxy']] == 'srri_lipper'
  P42.tib[['type_proxy']][is.no_change.v] <- P42.tib[['type']][is.no_change.v]
  
  ## replace type_proxy for method 'srri_x'
  is_srri_x.v <- grepl('^srri_[1-7]$', P42.tib[['iid_proxy']])
  P42.tib[['type_proxy']][is_srri_x.v] <- P42.tib[['type']][is_srri_x.v]
  
  ## replace type_proxy for method 'srri_NA'
  is_srri_na.v <- P42.tib[['iid_proxy']] == 'srri_NA'
  P42.tib[['type_proxy']][is_srri_na.v] <- P42.tib[['type']][is_srri_na.v]
  
  ## remove the inv and idx prefix from iid_proxy
  P42.tib[['iid_proxy']] <- gsub('^inv_', '', P42.tib[['iid_proxy']])
  P42.tib[['iid_proxy']] <- gsub('^idx_', '', P42.tib[['iid_proxy']])
  P42.tib <- P42.tib[, c('iid', 'type', 'iid_proxy', 'type_proxy', 'StartDate', 'Expiry')]
  return(P42.tib)
}
