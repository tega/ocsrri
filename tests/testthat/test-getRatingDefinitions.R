test_that("getLastDateAvailable works", {    
      
      rating.l <- list(
          sp = list(
              "2010-01-01" = list(
                  rating.v = "AAA",
                  ratClass.l = list(
                      'Investment grade' = "AAA",
                      'Non-investment grade' = "BB+"
                  )
              ),
              "2020-01-01" = list(
                  rating.v = "AAAr",
                  ratClass.l = list(
                      'Investment grade' = "A",
                      'Non-investment grade' = "B"
                  )
              )
          ),
          moody = list(
              "2010-01-01" = list(
                  rating.v = c("Aaa",	"Aa1",	"Aa2"),
                  ratClass.l = list(
                      'Investment grade' = c("Aaa"),
                      'Non-investment grade' = c("Ba1")
                  )
              )
          )
      )
      
      
      result.l <- getLastDateAvailable(rating.l$sp, as.Date("2021-07-11"))
      testthat::expect_equal(result.l$rating.v, "AAAr")
      testthat::expect_equal(result.l$ratClass.l$'Investment grade', "A")
      
      testthat::expect_error( getLastDateAvailable(rating.l$sp, as.Date("2001-01-01")),
          regexp = "No elements of is\\.valid\\.v are true")
      
      ratingRes.l <- lapply(rating.l, getLastDateAvailable, as.Date("2021-07-11"))
      testthat::expect_equal(names(ratingRes.l), c("sp", "moody"))
      testthat::expect_equal(ratingRes.l$sp$rating.v, "AAAr")
      testthat::expect_equal(ratingRes.l$sp$ratClass.l$'Investment grade', "A")
    })
