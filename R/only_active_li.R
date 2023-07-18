#' Title Remove inactive investments from a li(x) list of investments (indices)
#'
#' Given a list of investments (indices) where each component of the list is
#' a Pxx tibble (for a R01 request a P01 and additional
#' Pxx files as output or for a R04 request a P04 and additinal Pxx files) 
#' the function returns for each Pxx tibble the
#' subset of attributes of the active investments (indices) only.
#'
#' @param li the list of Pxx files. P01 (or P04) is mandatory in the list.
#'
#' @return a named list similar to li with the inactive investments
#' removed from every Pxx tibble of the list.
#'
#'
#' @examples
#' \dontrun{
#'	lia <- only_active_li(li)
#' }
#' @author Claudio Ortelli
#' @export

only_active_li <- function(li) {
  available_P_v <- names(li)
  
  if (all(is.element(c("P01", "P04"), available_P_v))) {
    msg <- "Only one Pxx file between P01 and P04 can be in the li list argument"
    stop(msg)
  }
  
  if (all(!is.element(c("P01", "P04"), available_P_v))) {
    msg <- "Exactly one Pxx file between P01 and P04 is mandatory in order to remove inactive investments!"
    stop(msg)
  }
  if (is.element("P01", available_P_v)) {
    Pxx <- "P01"
    implemented_P_v <- c(
      "P01", "P03", "P33", "P39",
      "P41", "P42", "P43", "P44",
      "P45", "P46", "P47", "P52")
  } else {
    Pxx <- "P04"
    implemented_P_v <- c(
      "P04", "P41", "P42", "P43",
      "P44", "P45", "P46")
  }
  
  not_implemented_P_v <- setdiff(available_P_v, implemented_P_v)
  
  if (length(not_implemented_P_v) > 0) {
    msg <- "Removing inactive instruments from the followin Pxx files is not implemented yet:\n"
    msg <- paste0(msg, paste(not_implemented_P_v, collapse = ", "))
    stop(msg)
  }
  if (Pxx == "P01") {
    li[[Pxx]] <- dplyr::filter(li[[Pxx]], .data[["InvStatus"]] == "1")
  } else {
    li[[Pxx]] <- dplyr::filter(li[[Pxx]], .data[["Status"]] == "1")
  }
  iid_active_v <- li[[Pxx]][["Idtf5"]]
  
  filt <- function(x, act) {
    if (is.element("Idtf", colnames(x))) {
      #keep_v <- x[["Idtf"]] %in% act
      #result <- x[keep_v, , drop = FALSE]
      result <- dplyr::filter(x, .data[["Idtf"]] %in% act)
    } else if (is.element("#Idtf", colnames(x))) {
      # result <- dplyr::filter(x, is.element(.data[["#Idtf"]], act))
      #keep_v <- x[["#Idtf"]] %in% act
      #result <- x[keep_v, , drop = FALSE]
      result <- dplyr::filter(x, .data[["#Idtf"]] %in% act)
    } else {
      stop("The tibble has no column 'Idtf' or '#Idtf'")
    }
    return(result)
  }
  lia_l <- lapply(
    li[setdiff(available_P_v, Pxx)],
    filt,
    act = iid_active_v)
  lia_l <- c(li[Pxx], lia_l)
  return(lia_l)
}

