testthat::test_that('getProxyMap works', {
      
      ## test with empty tib
      li <- list(P42 = tibble::tibble(Idtf = character(0), IndCode = character(0), IndClass = character(0),
              StartDate = as.Date(NA)[-1], Expiry = as.Date(NA)[-1]))
      expected.tib <- tibble::tibble(iid = character(0), type = character(0), iid_proxy = character(0),
          type_proxy = character(0), StartDate = as.Date(NA)[-1], Expiry = as.Date(NA)[-1])
      testthat::expect_equal(getProxyMap(li), expected.tib)
      
      ## return on empty tib
      P42.tib <- ocutil::stringToDataFrame(
          "  Idtf,IndCode,IndClass,StartDate,Expiry
              # --/-------/--------/---------/------
              38872,by_rat_mat,OC_SRRI_proxy,2021-09-03,NA
              27388,by_rat_mat,OC_SRRI_proxy,2021-09-03,NA
              10983,idx_37725,OC_SRRI_proxy,2021-06-07,2021-06-15
              10983,idx_24257,OC_SRRI_proxy,2021-06-16,2021-08-18
              10983,inv_33154,OC_SRRI_proxy,2021-08-19,NA
              12345,     xkd9,     OC_Pippo,2021-03-12,NA"
      )
      P42.tib[["StartDate"]] <- as.Date(P42.tib[["StartDate"]])
      P42.tib[["Expiry"]] <- as.Date(P42.tib[["Expiry"]])
      P42.tib <- tibble::as_tibble(P42.tib)
      exp.tib <- ocutil::stringToDataFrame(
          "  iid,type,iid_proxy,type_proxy,StartDate,Expiry
              # --/----/---------/----------/---------/------
              38872,inv,by_rat_mat,idx,2021-09-03,NA
              27388,inv,by_rat_mat,idx,2021-09-03,NA
              10983,inv,37725,idx,2021-06-07,2021-06-15
              10983,inv,24257,idx,2021-06-16,2021-08-18
              10983,inv,33154,inv,2021-08-19,NA"
      )
      exp.tib[["StartDate"]] <- as.Date(exp.tib[["StartDate"]])
      exp.tib[["Expiry"]] <- as.Date(exp.tib[["Expiry"]])
      exp.tib <- tibble::as_tibble(exp.tib)
      li <- list(P42 = P42.tib)
      testthat::expect_equal(getProxyMap(li), exp.tib)
      
      ## with special methods no_change, inv_own_ts
      P42.tib <- ocutil::stringToDataFrame(
          "  Idtf,IndCode,IndClass,StartDate,Expiry
              # --/-------/--------/---------/------
              38872,by_rat_mat,OC_SRRI_proxy,2021-09-03,NA
              27388,by_rat_mat,OC_SRRI_proxy,2021-09-03,NA
              10983,idx_37725,OC_SRRI_proxy,2021-06-07,2021-06-15
              10983,no_change,OC_SRRI_proxy,2021-06-16,2021-08-18
              10983,inv_own_ts,OC_SRRI_proxy,2021-08-19,NA
              12345,srri_7,OC_SRRI_proxy,2021-03-12,NA
              12345,srri_8,OC_pippo,2021-03-12,NA"
      )
      P42.tib[["StartDate"]] <- as.Date(P42.tib[["StartDate"]])
      P42.tib[["Expiry"]] <- as.Date(P42.tib[["Expiry"]])
      P42.tib <- tibble::as_tibble(P42.tib)
      exp.tib <- ocutil::stringToDataFrame(
          "  iid,type,iid_proxy,type_proxy,StartDate,Expiry
              # --/----/---------/----------/---------/------
              38872,inv,by_rat_mat,idx,2021-09-03,NA
              27388,inv,by_rat_mat,idx,2021-09-03,NA
              10983,inv,37725,idx,2021-06-07,2021-06-15
              10983,inv,no_change,inv,2021-06-16,2021-08-18
              10983,inv,own_ts,inv,2021-08-19,NA
              12345,inv,srri_7,inv,2021-03-12,NA"
      )
      exp.tib[["StartDate"]] <- as.Date(exp.tib[["StartDate"]])
      exp.tib[["Expiry"]] <- as.Date(exp.tib[["Expiry"]])
      exp.tib <- tibble::as_tibble(exp.tib)
      li <- list(P42 = P42.tib)
      testthat::expect_equal(getProxyMap(li), exp.tib)
      
      ## with special methods no_change, inv_own_ts, srri_lipper, srri_NA
      P42.tib <- ocutil::stringToDataFrame(
          "  Idtf,IndCode,IndClass,StartDate,Expiry
              # --/-------/--------/---------/------
              38872,by_rat_mat,OC_SRRI_proxy,2021-09-03,NA
              27388,by_rat_mat,OC_SRRI_proxy,2021-09-03,NA
              10983,idx_37725,OC_SRRI_proxy,2021-06-07,2021-06-15
              10983,no_change,OC_SRRI_proxy,2021-06-16,2021-08-18
              10983,inv_own_ts,OC_SRRI_proxy,2021-08-19,NA
              12345,srri_7,OC_SRRI_proxy,2021-03-12,NA
              12345,srri_8,OC_pippo,2021-03-12,NA
              12346,srri_lipper,OC_SRRI_proxy,2021-03-12,NA
              12349,srri_NA,OC_SRRI_proxy,2021-03-13,NA"
      )
      P42.tib[["StartDate"]] <- as.Date(P42.tib[["StartDate"]])
      P42.tib[["Expiry"]] <- as.Date(P42.tib[["Expiry"]])
      P42.tib <- tibble::as_tibble(P42.tib)
      exp.tib <- ocutil::stringToDataFrame(
          "  iid,type,iid_proxy,type_proxy,StartDate,Expiry
              # --/----/---------/----------/---------/------
              38872,inv,by_rat_mat,idx,2021-09-03,NA
              27388,inv,by_rat_mat,idx,2021-09-03,NA
              10983,inv,37725,idx,2021-06-07,2021-06-15
              10983,inv,no_change,inv,2021-06-16,2021-08-18
              10983,inv,own_ts,inv,2021-08-19,NA
              12345,inv,srri_7,inv,2021-03-12,NA
              12346,inv,srri_lipper,inv,2021-03-12,NA
              12349,inv,srri_NA,inv,2021-03-13,NA"
      )
      exp.tib[["StartDate"]] <- as.Date(exp.tib[["StartDate"]])
      exp.tib[["Expiry"]] <- as.Date(exp.tib[["Expiry"]])
      exp.tib <- tibble::as_tibble(exp.tib)
      li <- list(P42 = P42.tib)
      testthat::expect_equal(getProxyMap(li), exp.tib)
    })


