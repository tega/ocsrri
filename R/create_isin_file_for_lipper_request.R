#' Create the csv file with the ISIN for the lipper SRRI request
#'
#' Create the csv file with the ISIN for the lipper SRRI request
#'
#' @param li_date the date of the li(yyyymmdd).RData file from which the li list
#'        (list of investments) is constructed. This date determines the .RData file
#'        used to filter the fund and similar investments whose ISIN is saved in the
#'        output csv file to be imported in Lipper. li_date must be a date.
#'
#' @param outDir the output directory where to save the data.
#'
#' @examples
#' \dontrun{
#'  li <- list()
#'	create_isin_file_for_lipper_request(li, outDir = ".")
#' }
#' @author Claudio Ortelli
#' @export


create_isin_file_for_lipper_request <- function(
  li_date,
  outDir = '//rackstation/MarcoShare/Per Panos/Lipper_richieste_srri') {

  ## -------------------------------
  today <- lubridate::today('UTC')

  li <- load_li_rdata(li_date)
  li[["P01"]] <- dplyr::filter(li[["P01"]], .data[["InvStatus"]] == 1)

  message('INFO: extracting ISIN for Lipper request')
  isinLipper.tib <- get_lipper_funds(today, li)
  cleanLipper.l <- cleanLipperIsin(isinLipper.tib, TRUE)

  file_with_path <- file.path(outDir, paste0("lipper_isin_", li_date, ".csv"))
  message(paste0('INFO: file for Lipper request in ', file_with_path))
  write.table(
    as.data.frame(cleanLipper.l$isin.tib[,"ISIN"]),
    file = file_with_path, 
    row.names = FALSE, col.names = FALSE)

  return(invisible(0))

}
