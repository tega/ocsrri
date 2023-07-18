testthat::test_that('extractAvailableInLipper works', {
      
      li <- list(P01 = tibble::tibble(InvType = "abc", ISIN = "12345"))
      asOfDate <- as.Date("2021-09-20")
      
      testthat::expect_error(extractAvailableInLipper(li, asOfDate, FALSE), 
          regexp = "Following inv. types are unknown: abc")
      
      li <- list(P01 = tibble::tibble(InvType = c("Stock Funds", "Bond Funds", "Cash Accounts"), 
              ISIN = c("1", "12345", "3")))
      testthat::expect_equal(extractAvailableInLipper(li, asOfDate, FALSE), 
          c("1", "12345"))
      
      li <- list(P01 = tibble::tibble(InvType = "Cash Accounts", ISIN = "3"))
      testthat::expect_equal(extractAvailableInLipper(li, asOfDate, FALSE), 
          character(0))
      
      li <- list(P01 = tibble::tibble(Idtf5 = "1", InvType = "Cash Accounts", ISIN = "3"))
      testthat::expect_equal(extractAvailableInLipper(li, asOfDate, TRUE), 
          tibble::tibble(INTERNAL_ID = character(0), ISIN = character(0)))
      
      li <- list(P01 = tibble::tibble(InvType = c("Stock Funds", "Bond Funds", "Cash Accounts"), 
              ISIN = c("1", "12345", "3"), Idtf5 = c("1", "2", "3")))
      result.tib <- tibble::tibble(INTERNAL_ID = c("1", "2"), ISIN = c("1", "12345"))
      testthat::expect_equal(extractAvailableInLipper(li, asOfDate, TRUE), 
          tibble::tibble(INTERNAL_ID = c("1", "2"), ISIN = c("1", "12345")))
    })
