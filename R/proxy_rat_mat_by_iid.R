

proxy_rat_mat_by_iid <- function(iid, asOfDate, li, rating.l, matClass.v, 
    mapByCcy.l, allowedInvType.v) {
  
  io.tib <- dplyr::filter(li$P01, .data[['Idtf5']] == iid)
  assertthat::assert_that(nrow(io.tib) == 1)
  ccy <- io.tib[["InstrCcy"]]
  invType <- io.tib[["InvType"]]
  
  message(paste0("INFO: Processing INT_ID ", iid, ", inv. type = ", invType,", ccy = ", ccy))
  
  if (is.element(ccy, names(mapByCcy.l))) {
    
    ## SRRI_by_rat_mat_allowedInvType.v is in createRatingData.R
    if(!is.element(invType, allowedInvType.v)) {
      message <- paste0("The investment with INT_ID ", iid, " has an invalid inv. type '", invType, "' for method 'by_rat_mat'.")
      stop(message)
    }
    
    p03.tib <- dplyr::filter(li$P03, .data[['#Idtf']] == iid)
    mat.tib <- dplyr::filter(p03.tib, .data[['FeatureType']] == "Maturity")
    
    if (nrow(mat.tib) == 1) {
      redemptionDate <- mat.tib[1, "StartDate", drop = TRUE]
    } else {
      redemptionDate <- asOfDate + 365.25*20
    }
    
    ttm <- as.numeric(redemptionDate - asOfDate) / 365
    
    ratSP <- ocalc::get_inv_rating_as_of(iid, rating_agency = "S&P", dt = asOfDate, li = li)
    ratM  <- ocalc::get_inv_rating_as_of(iid, rating_agency = "Moody's", dt = asOfDate, li = li)
    ratF  <- ocalc::get_inv_rating_as_of(iid, rating_agency = "Fitch", dt = asOfDate, li = li)
    
    ratingClass.v <- character(0)
    
    if (!is.na(ratSP)) {
      ratingClass.v <- c(sp = mapToRatingClass("sp", ratSP, rating.l))
    }
    if (!is.na(ratM)) {
      ratingClass.v <- c(ratingClass.v, c(moody = mapToRatingClass("moody", ratM, rating.l)))
    }
    if (!is.na(ratF)) {
      ratingClass.v <- c(ratingClass.v, c(fitch = mapToRatingClass("fitch", ratF, rating.l)))
    }
    
    if(length(ratingClass.v) == 0) {
      ## assign highest available rating class
      ratingClass.v <- sort(names(rating.l$sp$ratClass.l), decreasing = TRUE) [1]
    }
    
    ratingClass <- sort(ratingClass.v, decreasing = TRUE) [1]
    
    if (ttm >= 0) {
      mappedMat <- (names(matClass.v)[ ttm < matClass.v]) [1]
      iid_proxy <- mapByCcy.l[[ccy]][ratingClass, mappedMat]
    } else {
      mappedMat = NA_character_
      iid_proxy = 'srri_7' ## Fake index with SRRI = 1
    }  
    
    ## determine the type of the iid_proxy, i.e. idx or inv
    ## currently only idx are allowed!
    assertthat::assert_that(!is.element(iid_proxy, li$P01[["Idtf5"]]))
    type_proxy <- "idx"
    
  } else {
    iid_proxy = 'srri_7' ## Fake index with SRRI = 7
    type_proxy = 'idx'
    ratingClass = NA_character_
    mappedMat = NA_character_
  }
  
  result.v <- c(iid_proxy, type_proxy, ratingClass, mappedMat, ccy)
  names(result.v) <- c('iid_proxy', 'type_proxy', 'ratingClass', 'mappedMat', 'ccy')
  return(result.v)
}

