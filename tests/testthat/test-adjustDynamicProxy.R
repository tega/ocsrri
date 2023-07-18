testthat::test_that('adjustDynamicProxy works', {
      
      map.tib <- tibble::tibble(iid = c("22866", "24588"), 
          type = rep("inv", 2), iid_proxy = rep("by_rat_mat", 2), 
          type_proxy = rep(NA_character_, 2))
          
          
      P41.1 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "Fitch", RatingCode= "CCC", StartDate = as.Date(NA), Expiry = as.Date(NA))
      P41.2 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "24588", RatingAgency = "Moody's", RatingCode= "Aaa", StartDate = as.Date(NA), Expiry = as.Date(NA))
      P41.3 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "11111", RatingAgency = "S&P", RatingCode= "NR", StartDate = as.Date(NA), Expiry = as.Date(NA))
      
      P41 <- rbind(P41.1, P41.2, P41.3)
      
      li <- list(
          P01 = tibble::tibble(Idtf5 = c("24588", "22866"), InstrCcy = c("EUR", "USD"), 
              InvType = c("Zero Bonds", "Ordinary Bonds")),
          P03 = tibble::tibble(`#Idtf` = c("24588", "22866"), FeatureType = "Maturity", StartDate = as.Date(c("2017-02-17","2021-11-15"))),
          P41 = P41
      )      
      
      asOfDate <- as.Date("2017-02-15")
      
      expectedMap.tib <- tibble::tibble(iid = c("22866", "24588"), 
          type = rep("inv", 2), iid_proxy = c("57053", "57056"), 
          type_proxy = rep("idx", 2))      
      result.tib <- adjustDynamicProxy(map.tib, asOfDate, li)
      testthat::expect_equal(result.tib, expectedMap.tib)
      
      ## return on empty tib
      result.tib <- adjustDynamicProxy(map.tib[-(1:2),], asOfDate, li)
      testthat::expect_equal(result.tib, map.tib[-(1:2),])
    })


