test_that("mapToRatingClass works", {
      
      asOfDateRating.l <- createRatingData(as.Date("2021-08-30")) [["rating.l"]]
      
      testthat::expect_equal(mapToRatingClass("sp", "AA+", asOfDateRating.l), "1. Investment grade")
      testthat::expect_equal(mapToRatingClass("sp", "CCC+r", asOfDateRating.l), "2. Non-investment grade")
      
      testthat::expect_equal(mapToRatingClass("moody", "A3", asOfDateRating.l), "1. Investment grade")
      testthat::expect_equal(mapToRatingClass("moody", "Caa3", asOfDateRating.l), "2. Non-investment grade")      
      
      testthat::expect_equal(mapToRatingClass("fitch", "A-", asOfDateRating.l), "1. Investment grade")
      testthat::expect_equal(mapToRatingClass("fitch", "DD", asOfDateRating.l), "2. Non-investment grade")
      
      testthat::expect_error(mapToRatingClass("fitch", "C", asOfDateRating.l), 
          regexp = 'is\\.element\\(el = rating, set = info\\.l\\[\\[\\"rating\\.v"\\]\\]\\) is not TRUE')
    })
