testthat::test_that('volToSrri works', {
    
      ## NA returns NA
      testthat::expect_equal(volToSrri(NA), NA)
      
      ## check all risk classes
      testthat::expect_equal(volToSrri(0.001), 1L)
      testthat::expect_equal(volToSrri(0.01), 2L)
      testthat::expect_equal(volToSrri(0.03), 3L)
      testthat::expect_equal(volToSrri(0.08), 4L)
      testthat::expect_equal(volToSrri(0.11), 5L)
      testthat::expect_equal(volToSrri(0.18), 6L)
      testthat::expect_equal(volToSrri(0.26), 7L)
    })

