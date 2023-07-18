testthat::test_that('proxyMapByTimeRange with ndays = 0 works', {
      
      map.tib <- ocutil::stringToDataFrame(
          "  iid,type,iid_proxy,type_proxy,StartDate,Expiry
              # --/----/---------/----------/---------/------
              38872,inv,by_rat_mat,idx,2021-09-03,NA
              27388,inv,by_rat_mat,idx,2021-09-03,NA
              10983,inv,37725,idx,2021-04-07,2021-06-15
              10983,inv,24257,idx,2021-06-16,2021-08-18
              10983,inv,33154,inv,2021-08-19,NA"
      )
      map.tib[["StartDate"]] <- as.Date(map.tib[["StartDate"]])
      map.tib[["Expiry"]] <- as.Date(map.tib[["Expiry"]])
      map.tib <- tibble::as_tibble(map.tib)
      
      ## test 1: return empty map
      result.tib <- proxyMapByTimeRange(as.Date('2020-04-19'), 0L, map.tib)
      expected.tib <- map.tib[-(1:5), -(5:6)]
      testthat::expect_equal(result.tib, expected.tib)
      
      ## return 1x4 tibble 
      result.tib <- proxyMapByTimeRange(as.Date('2021-08-19'), 0L, map.tib)
      expected.tib <- map.tib[ 5, -(5:6)]
      testthat::expect_equal(result.tib, expected.tib)
      
       
    })



testthat::test_that('proxyMapByTimeRange with ndays > 0 works', {
      
      map.tib <- ocutil::stringToDataFrame(
          "  iid,type,iid_proxy,type_proxy,StartDate,Expiry
              # --/----/---------/----------/---------/------
              38872,inv,by_rat_mat,idx,2021-09-03,NA
              27388,inv,by_rat_mat,idx,2021-09-03,NA
              10983,inv,37725,idx,2021-04-07,2021-06-15
              10983,inv,24257,idx,2021-06-16,2021-08-18
              10983,inv,33154,inv,2021-08-19,NA"
      )
      map.tib[["StartDate"]] <- as.Date(map.tib[["StartDate"]])
      map.tib[["Expiry"]] <- as.Date(map.tib[["Expiry"]])
      map.tib <- tibble::as_tibble(map.tib)
      
      ## test 1: return empty map
      result.tib <- proxyMapByTimeRange(as.Date('2020-04-19'), 20L, map.tib)
      expected.tib <- map.tib[-(1:5), -(5:6)]
      testthat::expect_equal(result.tib, expected.tib)
      
      ## test 2: return all up to the first two
      result.tib <- proxyMapByTimeRange(as.Date('2021-09-02'), 365L, map.tib)
      expected.tib <- map.tib[-(1:2), -(5:6)]
      testthat::expect_equal(result.tib, expected.tib)
      
    })
