#' Load list of investments in a li(yyyymmdd).RData file
#'
#' Load list of investments in a li(yyyymmdd).RData file
#'
#' @param li_date the date of the li(yyyymmdd).RData file from which the li list 
#'        (list of investments) is constructed. This date determines the .RData file
#'        used to filter the fund and similar investments whose ISIN is saved in the
#'        output csv file to be imported in Lipper. li_date must be a date.
#' 
#' @return li list of investment objects from the Allocare request, i.e. a named list
#'         with field names "P01" "P03" "P33" "P39" "P41" "P42" "P43" "P44" "P45" "P46" "P47" "P52".
#'         
#' @examples
#' \dontrun{
#'	li <- load_li_rdata(Sys.Date())
#' }
#' @author Claudio Ortelli
#' @export


load_li_rdata <- function(li_date) {
  
  todaystr <- format(lubridate::today('UTC'), format = '%Y%m%d')
  li_date_str <- format(li_date, format = '%Y%m%d')
  
  to_load <- paste0("li", li_date_str, ".RData")
  file_with_path <- file.path(ocutil::default_dir('allocare_dir'), to_load)
  
  message('INFO: loading instruments file')
  if (file.access(file_with_path, mode = 4) == -1L) {
    msg <- paste0("Il file ", file_with_path, " non esite o mancano i diritti di lettura.")
    stop(msg)
  }
  
  if (li_date_str != todaystr) {
    msg <- paste("INFO: the 'li(yyyymmdd).RData' file has a time stamp different from the current date.")
    message(msg)
  }
  message(paste0("INFO: processing file ", to_load))
  li <- ocutil::load_single_var(file.path(ocutil::default_dir('allocare_dir'), to_load))
  return(li)
}