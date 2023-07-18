test_that("proxy_rat_mat_by_iid works", {
      
      P41.1 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "Moody's", RatingCode= "Aa1", StartDate = as.Date(NA), Expiry = as.Date(NA))
      P41.2 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "S&P", RatingCode= "AA-", StartDate = as.Date("2014-11-10"), Expiry = as.Date(NA))
      P41.3 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "Fitch", RatingCode = "A-", StartDate = as.Date("2015-11-10"), Expiry = as.Date(NA))
      P41 <- rbind(P41.1, P41.2, P41.3)
      li <- list(
          P01 = tibble::tibble(Idtf5 = "22866", InstrCcy = "USD", 
              InvType = "Ordinary Bonds"),
          P03 = tibble::tibble(`#Idtf` = "22866", FeatureType = "Maturity", StartDate = as.Date("2023-01-01")),
          P41 = P41
      )      
      
      asOfDate <- as.Date("2021-08-30")
      
      rd.l <- createRatingData(asOfDate)
      rating.l <- rd.l$rating.l
      mapByCcy.l <- rd.l$mapByCcy.l
      allowedInvType.v <- rd.l$allowedInvType.v
      matClass.v <- rd.l$matClass.v
      
      iid <- "22866"
      
      ## should return correct values "1. Investment grade", "[1,3)", "USD"
      expected.v <- c(iid_proxy = "57048", type_proxy = "idx", ratingClass = "1. Investment grade", mappedMat = "[1,3)", ccy = "USD")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid, asOfDate, li, rating.l, matClass.v, 
              mapByCcy.l, allowedInvType.v), expected.v)
      
      ## should return correct values "57054", "2. Non-investment grade", "[5, Inf)", "USD"
      li$P03[1, 'StartDate'] <- as.Date("2027-01-01")
      li$P41[3, 'RatingCode'] <- "DD"
      expected.v <- c(iid_proxy = "57054", type_proxy = "idx", ratingClass = "2. Non-investment grade", mappedMat = "[5,Inf)", ccy = "USD")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid, asOfDate, li, rating.l, matClass.v,
              mapByCcy.l, allowedInvType.v), expected.v)
      
      ## should assign highest rating
      li$P41 <- li$P41[-(1:3), ]
      expected.v <- c(iid_proxy = "57054", type_proxy = "idx", ratingClass = "2. Non-investment grade", mappedMat = "[5,Inf)", ccy = "USD")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid, asOfDate, li, rating.l, matClass.v,  
              mapByCcy.l, allowedInvType.v), expected.v)
      
      ## negative time to maturity
      li$P03[1, 'StartDate'] <- as.Date("2018-01-01")
      expected.v <- c(iid_proxy = "srri_7", type_proxy = "idx", ratingClass = "2. Non-investment grade", mappedMat = NA_character_, ccy = "USD")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid, asOfDate, li, rating.l, matClass.v, 
              mapByCcy.l, allowedInvType.v), expected.v)
      
      ## error on invalid investment type
      li$P03[1, 'StartDate'] <- as.Date("2027-01-01")
      li$P01[1, "InvType"] <- "Special Bonds"
      
      testthat::expect_error(proxy_rat_mat_by_iid(iid, asOfDate, li, rating.l, matClass.v, 
              mapByCcy.l, allowedInvType.v),
          regexp = "^The investment with INT_ID 22866 has an invalid inv\\. type")
      
      ## error on 'inv' by_rat_mat proxy 
      P41 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "24588", RatingAgency = "S&P", RatingCode= "NR", StartDate = as.Date("2017-02-15"), Expiry = as.Date(NA))
      
      li <- list(
          P01 = tibble::tibble(Idtf5 = c("24588", "22866"), InstrCcy = c("USD", "USD"), 
              InvType = c("Zero Bonds", "Ordinary Bonds")),
          P03 = tibble::tibble(`#Idtf` = c("24588", "22866"), FeatureType = rep("Maturity",2), StartDate = as.Date(c("2021-09-17","2021-11-15"))),
          P41 = P41
      )
      
      mapByCcy_new.l <- mapByCcy.l
      mapByCcy_new.l$USD["2. Non-investment grade", "[0,1)"] <- "22866"
      testthat::expect_error(proxy_rat_mat_by_iid(iid = "24588", asOfDate, li, rating.l, matClass.v, 
              mapByCcy_new.l, allowedInvType.v),
          regexp = '!is\\.element\\(iid_proxy, li\\$P01\\[\\["Idtf5"\\]\\]\\) is not TRUE')
       
      ## regression test: these values generated an error, now bug corrected
      P41.1 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "24588", RatingAgency = "Moody's", RatingCode= "Aaa", StartDate = as.Date(NA), Expiry = as.Date(NA))
      P41.2 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "24588", RatingAgency = "S&P", RatingCode= "AAA", StartDate = as.Date(NA), Expiry = as.Date("2017-02-14"))
      P41.3 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "24588", RatingAgency = "S&P", RatingCode= "NR", StartDate = as.Date("2017-02-15"), Expiry = as.Date(NA))
      
      P41 <- rbind(P41.1, P41.2, P41.3)
      
      li <- list(
          P01 = tibble::tibble(Idtf5 = c("24588", "22866"), InstrCcy = c("USD", "USD"), 
              InvType = c("Zero Bonds", "Ordinary Bonds")),
          P03 = tibble::tibble(`#Idtf` = c("24588", "22866"), FeatureType = rep("Maturity",2), StartDate = as.Date(c("2017-02-17","2021-11-15"))),
          P41 = P41
      )
      
      asOfDate <- as.Date("2017-02-15")
      expected.v <- c(iid_proxy = "57055", type_proxy = "idx", ratingClass = "2. Non-investment grade", mappedMat = "[0,1)", ccy = "USD")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid = "24588", asOfDate, li, rating.l, matClass.v, 
              mapByCcy.l, allowedInvType.v), expected.v)
      
      
      
      ## added new test with no rating
      P41.1 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "24588", RatingAgency = "Moody's", RatingCode= "Aaa", StartDate = as.Date(NA), Expiry = as.Date(NA))
      
      P41 <- P41.1[-1, ]
      
      asOfDate <- as.Date("2017-02-15")
      expected.v <- c(iid_proxy = "57055", type_proxy = "idx", ratingClass = "2. Non-investment grade", mappedMat = "[0,1)", ccy = "USD")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid = "24588", asOfDate, li, rating.l, matClass.v, 
              mapByCcy.l, allowedInvType.v), expected.v)
      
      ## test [0,1), 1. Inv Grade, USD
      asOfDate <- as.Date("2021-01-01")
      P41 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "Fitch", RatingCode = "A-", StartDate = as.Date(NA), Expiry = as.Date(NA))
      li <- list(
          P01 = tibble::tibble(Idtf5 = "22866", InstrCcy = "USD", 
              InvType = "Ordinary Bonds"),
          P03 = tibble::tibble(`#Idtf` = "22866", FeatureType = "Maturity", StartDate = as.Date("2021-01-01")),
          P41 = P41
      )      
      expected.v <- c(iid_proxy = "57048", type_proxy = "idx", ratingClass = "1. Investment grade", mappedMat = "[0,1)", ccy = "USD")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid = "22866", asOfDate, li, rating.l, matClass.v, 
              mapByCcy.l, allowedInvType.v), expected.v)
      
      ## test [1,3), 1. Inv Grade, USD
      asOfDate <- as.Date("2021-01-01")
      P41 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "Fitch", RatingCode = "A-", StartDate = as.Date(NA), Expiry = as.Date(NA))
      li <- list(
          P01 = tibble::tibble(Idtf5 = "22866", InstrCcy = "USD", 
              InvType = "Ordinary Bonds"),
          P03 = tibble::tibble(`#Idtf` = "22866", FeatureType = "Maturity", StartDate = as.Date("2023-01-01")),
          P41 = P41
      )      
      expected.v <- c(iid_proxy = "57048", type_proxy = "idx", ratingClass = "1. Investment grade", mappedMat = "[1,3)", ccy = "USD")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid = "22866", asOfDate, li, rating.l, matClass.v, 
              mapByCcy.l, allowedInvType.v), expected.v)
      
      ## test [3,5), 1. Inv Grade, USD
      asOfDate <- as.Date("2021-01-01")
      P41 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "Fitch", RatingCode = "A-", StartDate = as.Date(NA), Expiry = as.Date(NA))
      li <- list(
          P01 = tibble::tibble(Idtf5 = "22866", InstrCcy = "USD", 
              InvType = "Ordinary Bonds"),
          P03 = tibble::tibble(`#Idtf` = "22866", FeatureType = "Maturity", StartDate = as.Date("2025-01-01")),
          P41 = P41
      )      
      expected.v <- c(iid_proxy = "57049", type_proxy = "idx", ratingClass = "1. Investment grade", mappedMat = "[3,5)", ccy = "USD")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid = "22866", asOfDate, li, rating.l, matClass.v, 
              mapByCcy.l, allowedInvType.v), expected.v)
      
      ## test [5,Inf), 1. Inv Grade, USD
      asOfDate <- as.Date("2021-01-01")
      P41 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "Fitch", RatingCode = "A-", StartDate = as.Date(NA), Expiry = as.Date(NA))
      li <- list(
          P01 = tibble::tibble(Idtf5 = "22866", InstrCcy = "USD", 
              InvType = "Ordinary Bonds"),
          P03 = tibble::tibble(`#Idtf` = "22866", FeatureType = "Maturity", StartDate = as.Date("2029-01-01")),
          P41 = P41
      )      
      expected.v <- c(iid_proxy = "57051", type_proxy = "idx", ratingClass = "1. Investment grade", mappedMat = "[5,Inf)", ccy = "USD")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid = "22866", asOfDate, li, rating.l, matClass.v, 
              mapByCcy.l, allowedInvType.v), expected.v)
      
      ## test [0,1), 2. Non-inv Grade, USD
      asOfDate <- as.Date("2021-01-01")
      P41 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "Fitch", RatingCode = "CCC", StartDate = as.Date(NA), Expiry = as.Date(NA))
      li <- list(
          P01 = tibble::tibble(Idtf5 = "22866", InstrCcy = "USD", 
              InvType = "Ordinary Bonds"),
          P03 = tibble::tibble(`#Idtf` = "22866", FeatureType = "Maturity", StartDate = as.Date("2029-01-01")),
          P41 = P41
      )      
      expected.v <- c(iid_proxy = "57054", type_proxy = "idx", ratingClass = "2. Non-investment grade", mappedMat = "[5,Inf)", ccy = "USD")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid = "22866", asOfDate, li, rating.l, matClass.v, 
              mapByCcy.l, allowedInvType.v), expected.v)
      
      ## test non EUR, non USD and non CHF bond
      asOfDate <- as.Date("2021-01-01")
      P41 <- tibble::tibble(InvObjType = "Investment", IdtfSys = "INTERNAL_ID", 
          Idtf = "22866", RatingAgency = "Fitch", RatingCode = "CCC", StartDate = as.Date(NA), Expiry = as.Date(NA))
      li <- list(
          P01 = tibble::tibble(Idtf5 = "22866", InstrCcy = "ZAR", 
              InvType = "Ordinary Bonds"),
          P03 = tibble::tibble(`#Idtf` = "22866", FeatureType = "Maturity", StartDate = as.Date("2029-01-01")),
          P41 = P41
      )      
      expected.v <- c(iid_proxy = "srri_7", type_proxy = "idx", ratingClass = NA_character_, mappedMat = NA_character_, ccy = "ZAR")
      testthat::expect_equal(proxy_rat_mat_by_iid(iid = "22866", asOfDate, li, rating.l, matClass.v, 
              mapByCcy.l, allowedInvType.v), expected.v)

    })
