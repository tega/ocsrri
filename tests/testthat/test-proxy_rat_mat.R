test_that("proxy_rat_mat works", {
      
      P41.1 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "Moody's", RatingCode= "Aa1", StartDate = as.Date(NA), Expiry = as.Date(NA))
      P41.2 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "S&P", RatingCode= "AA-", StartDate = as.Date("2014-11-10"), Expiry = as.Date(NA))
      P41.3 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "Fitch", RatingCode = "A-", StartDate = as.Date("2015-11-10"), Expiry = as.Date(NA))
      P41.4 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "24588", RatingAgency = "Moody's", RatingCode= "Aaa", StartDate = as.Date(NA), Expiry = as.Date(NA))
      P41.5 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "24588", RatingAgency = "S&P", RatingCode= "AAA", StartDate = as.Date(NA), Expiry = as.Date("2017-02-17"))
      P41.6 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "24588", RatingAgency = "S&P", RatingCode= "NR", StartDate = as.Date("2017-02-18"), Expiry = as.Date(NA))
      
      P41 <- rbind(P41.1, P41.2, P41.3, P41.4, P41.5, P41.6)
     
      li <- list(
          P01 = tibble::tibble(Idtf5 = c("24588", "22866"), InstrCcy = c("EUR", "USD"), 
              InvType = c("Zero Bonds", "Ordinary Bonds")),
          P03 = tibble::tibble(`#Idtf` = c("24588", "22866"), FeatureType = "Maturity", StartDate = as.Date(c("2017-02-17","2021-11-15"))),
          P41 = P41
      )      
      
      asOfDate <- as.Date("2017-02-15")
      map.tib <- tibble::tibble(iid = c("24588", "22866"), iid_proxy = c("by_rat_mat", "by_rat_mat"))
      result.l <- proxy_rat_mat(map.tib, asOfDate, li)
      testthat::expect_equal(length(result.l), 2)
      testthat::expect_equal(names(result.l), c("24588", "22866"))
      
      
    })


