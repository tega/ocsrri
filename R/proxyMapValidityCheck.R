#' Check the validity of SRRI_OC_proxy identifiers
#'
#' Check the validity of SRRI_OC_proxy identifiers by requiring that
#' an investment object or an index used as proxy by another I.O. or
#' index can not have a proxy.
#'
#' @param proxyMap.tib a tibble with 2 columns <Int_id, Proxy_Int_id>
#' @param strict logical. If TRUE proxy may not have a proxy.
#' @param action character string equal to "warn" or "stop" determining
#' the behaviour of the function in case of invalid entries (i.e. I.O. used
#' as proxy having themself a proxy). If action = 'stop' the function stops
#' with an error message. If action = 'warn' a warning is produced but the
#' function continues the execution. If action is missing no warning and no
#' error is reportded.
#'
#'
#' @return A named list with
#'    - info a character string with possible values "no_cycles" or "cycles"
#'    - map.df the data.frame with the mapping iid -> iid_proxy of the self
#'      referencing investments.
#'
#' @examples
#' proxyMap.tib <- data.frame(iid = c("1", "2", "3"), type = rep("inv", 3), 
#'                     iid_proxy = c("2", "3", "5"), type_proxy = rep("idx", 3), 
#'                     stringsAsFactors = FALSE)
#' result.l <- proxyMapValidityCheck(proxyMap.tib, strict = TRUE, action = 'warn')
#' @export

proxyMapValidityCheck <- function(proxyMap.tib, strict = TRUE, action) {
  
  checkmate::assert_character(proxyMap.tib[["iid"]])
  checkmate::assert_character(proxyMap.tib[["iid_proxy"]])
  assertthat::assert_that(any(proxyMap.tib[["iid"]] == proxyMap.tib[["iid_proxy"]]) == FALSE)
  
  result.l <- cycles(proxyMap.tib[, c('iid', 'iid_proxy')], strict = strict, verbose = TRUE)
  
  if (result.l$info == "no_cycles" | missing(action)) return(result.l)
  
  assertthat::assert_that(is.element(action, c("warn", "stop")))
  
  message("Info: the following INTERNAL_ID generate a cycle error.")
  message(paste(result.l$map.df$iid, " -> ", result.l$map.df$iid_proxy, "\n", sep = ""))
    
  if (action == "stop") stop("Cycles found.")
  
  return(result.l)
}

