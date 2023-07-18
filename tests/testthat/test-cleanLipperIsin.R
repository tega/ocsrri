testthat::test_that('cleanLipperIsin works', {
      
      isin.tib <- tibble::tibble(
          INTERNAL_ID = "1", 
          ISIN = "12345")
      empty.tib <- tibble::tibble(
          INTERNAL_ID = character(0),
          ISIN = character(0)
          )
      result.l <- list(isin.tib = empty.tib, wrongIsin.tib  = isin.tib,
          naIsin.tib = empty.tib)    
      testthat::expect_equal(cleanLipperIsin(isin.tib, verbose = FALSE), result.l)
      
      isin.tib <- tibble::tibble(
          INTERNAL_ID = c("1", "2"), 
          ISIN = c("CH0311189580", NA))
      result.l <- list(isin.tib = isin.tib[-2,], wrongIsin.tib  = empty.tib,
          naIsin.tib = isin.tib[-1,])    
      testthat::expect_equal(cleanLipperIsin(isin.tib, verbose = FALSE), result.l)
      
      
      isin.tib <- tibble::tibble(
          INTERNAL_ID = c("1", "2", "3"), 
          ISIN = c("CH0311189580", NA, "adk2345"))
      result.l <- list(isin.tib = isin.tib[1,], wrongIsin.tib  = isin.tib[3,],
          naIsin.tib = isin.tib[2,])    
      testthat::expect_equal(cleanLipperIsin(isin.tib, verbose = FALSE), result.l)
      
      ## same as before with verbose = TRUE
      isin.tib <- tibble::tibble(
          INTERNAL_ID = "1", 
          ISIN = "12345")
      empty.tib <- tibble::tibble(
          INTERNAL_ID = character(0),
          ISIN = character(0)
      )
      result.l <- list(isin.tib = empty.tib, wrongIsin.tib  = isin.tib,
          naIsin.tib = empty.tib)    
      testthat::expect_equal(cleanLipperIsin(isin.tib, verbose = TRUE), result.l)
      
      isin.tib <- tibble::tibble(
          INTERNAL_ID = c("1", "2"), 
          ISIN = c("CH0311189580", NA))
      result.l <- list(isin.tib = isin.tib[-2,], wrongIsin.tib  = empty.tib,
          naIsin.tib = isin.tib[-1,])    
      testthat::expect_equal(cleanLipperIsin(isin.tib, verbose = TRUE), result.l)
      
      
      isin.tib <- tibble::tibble(
          INTERNAL_ID = c("1", "2", "3"), 
          ISIN = c("CH0311189580", NA, "adk23d5"))
      result.l <- list(isin.tib = isin.tib[1,], wrongIsin.tib  = isin.tib[3,],
          naIsin.tib = isin.tib[2,])    
      testthat::expect_equal(cleanLipperIsin(isin.tib, verbose = TRUE), result.l)
      
     
    })
