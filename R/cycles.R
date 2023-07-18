#' Check if the map internal_id -> internal_id_proxy contains cycles
#'
#' Given a data.frame with columns iid and iid_proxy the implicit map  
#' iid -> iid_proxy is tested for cycles. The idea is to avoid 
#' autoreferencing  of investment objects when computing the OC_SRRI.
#'
#' @param map.df data.frame with character columns iid and iid_proxy.
#' 
#' @param strict logical. If TRUE a proxy can not have a proxy.
#' 
#' @param verbose logical. If true a sequence of info messages is printed. 
#' 
#' @return "cycles" is returned if one or more autoreferencing sets of 
#'         investments are identified. "no_cycles" otherwise.
#' 
#' @examples
#' h.df <- data.frame(
#'    iid = c("a", "b", "c", "e", "d", "v", "z", "k"),
#'    iid_proxy = c("b", "c", "k", "h", "p", "z", "s", "q"),
#'          stringsAsFactors = FALSE)
#' cycles(h.df, strict = FALSE, verbose = TRUE)
#'  
#' @export


cycles <- function(map.df, strict, verbose) {
  
  GP_halt_check <- function(GP.v, P.v) {
    ## GP.v the vector of good proxy id
    ##  P.v the vector of proxy id
    
    if (verbose) message("Info from good proxy halt check.")
    if (length(GP.v) == 0) {
      if (length(P.v) > 0) {
        if (verbose) message("Info: find.cycles terminated with at least one cycle found.")   
        return("cycles")
      } else {
        if (verbose) message("Info: find.cycles terminated with no cycles found.")
        return("no_cycles")
      }
    }
    if (verbose) message("Info: this good proxys are available:")
    if (verbose) message(paste(GP.v, "\n"))
    return("continue")
  }
  
  GI_halt_check <- function(GI.v, I.v) {
    ## GI.v good index vector
    ## I.v index vector
    if (verbose) message("Info from good investment halt check.")
    if (length(GI.v) == 0) {
      if (length(I.v) > 0) {
        if (verbose) message("Info: terminated with at least one cycle found")
        return("cycles")
      } else {
        if (verbose) message("Info: terminated with no cycles found")
        return("no_cycles")
      }
    }
    if (verbose) message("Info: this good investments will be intersected with remaining proxys:")
    if (verbose) message(paste(GI.v, "\n"))
    return("continue")
  }
  
  if (verbose) message("Info: this map must be explained:")
  if (verbose) message(paste(map.df[["iid"]], " -> ", map.df[["iid_proxy"]], "\n", sep = ""))
  
  if (strict) {
    common.v <- intersect(map.df[["iid"]], map.df[["iid_proxy"]])
    if (length(common.v) > 0) {
      get.v <- is.element(map.df[["iid"]], common.v) | is.element(map.df[["iid_proxy"]], common.v)
      map.df <- map.df[get.v, ]
      if (verbose) message("Info: strict check is enabled. Exceptions found:")
      if (verbose) message(paste(map.df$iid, " -> ", map.df$iid_proxy, "\n", sep = ""))
      return(list(info = "cycles", map.df = map.df))
    } else {
      if (verbose) message("Info: strict check is enabled. No exceptions found.\n")
      result.l <- list(info = "no_cycles", map.df = data.frame(iid = character(0),
              iid_proxy = character(0), stringsAsFactors = FALSE))
      return(result.l)
    }
  }
  
  ## determine the (good) set of proxy that do not have a proxy
  I.v <- map.df[["iid"]]
  P.v <- map.df[["iid_proxy"]]
  GP.v <- setdiff(P.v, I.v)
  
  while (TRUE) {
    
    ## check that GP.v is not empty
    info <- GP_halt_check(GP.v, P.v)
    if (grepl("cycles$", info)) {
      result <- list(
          info=info, 
          map.df = data.frame(iid = I.v, iid_proxy = P.v, 
              stringsAsFactors = FALSE)
      )
      return(result)
    }
    toRemove.v <- is.element(P.v, GP.v)
    P.v <- P.v[!toRemove.v]
    
    GI.v <- I.v[toRemove.v]
    I.v <- I.v[!toRemove.v]
    if (verbose) message("Info: this investments (pre-images) are good:")
    if (verbose) message(paste(GI.v, "\n"))
    
    if (verbose) message("Info: this map remains to be explained:")
    if (verbose) {
      if (length(I.v) > 0) {
        message(paste(I.v, " -> ", P.v, "\n", sep = ""))
      } else {
        message("No items left.")
      }
    }
    info <- GI_halt_check(GI.v, I.v)
    
    if (grepl("cycles$", info)) {
      result <- list(
          info=info, 
          map.df = data.frame(iid = I.v, iid_proxy = P.v, 
              stringsAsFactors = FALSE)
      )
      return(result)
    }  
    
    ## determine which GI.v are in P.v in order to define the new
    ## vector of good proxy
    GP.v <- intersect(P.v, GI.v)
    
    ## continue this way    
  }
}