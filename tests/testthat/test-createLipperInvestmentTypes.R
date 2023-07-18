testthat::test_that('createLipperInvestmentTypes works', {
      
      ## extract data.frame
      data.df <- createLipperInvestmentTypes(as.Date("2021-09-20"))
      
      testthat::expect_equal(nrow(data.df), 217)
      testthat::expect_equal(ncol(data.df), 4)
      testthat::expect_equal(colnames(data.df), c("Value", "Description", 
              "InLipper", "Investment Category"))
      
      ## extract data.frame
      data.df <- createLipperInvestmentTypes(as.Date("2021-11-20"))
      
      testthat::expect_equal(nrow(data.df), 217)
      testthat::expect_equal(ncol(data.df), 4)
      testthat::expect_equal(colnames(data.df), c("Value", "Description", 
              "InLipper", "Investment Category"))
      testthat::expect_true(is.element("ABS Return Equities Funds", data.df[["Description"]]))
      
      ## expect error
      testthat::expect_error(createLipperInvestmentTypes(as.Date("2009-11-20")))
      
      
    })
