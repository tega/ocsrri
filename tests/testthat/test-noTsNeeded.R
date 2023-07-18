testthat::test_that('noTsNeeded works', {
      
      test.tib <- tibble::tibble(iid_proxy = character(0))
      testthat::expect_equal(noTsNeeded(test.tib), logical(0))
      
      test.tib <- tibble::tibble(iid_proxy = c('no_change'))
      testthat::expect_equal(noTsNeeded(test.tib), TRUE)
      
      test.tib <- tibble::tibble(iid_proxy = paste0('srri_', 1:7))
      testthat::expect_equal(noTsNeeded(test.tib), rep(TRUE,7))
      
      test.tib <- tibble::tibble(iid_proxy = c('no_change', '123', paste0("srri_", 1:7)))
      testthat::expect_equal(noTsNeeded(test.tib), c(TRUE, FALSE, rep(TRUE,7)))
      
      test.tib <- tibble::tibble(iid_proxy = c('no_change', 'srri_lipper', paste0("srri_", 1:7)))
      testthat::expect_equal(noTsNeeded(test.tib), c(TRUE, TRUE, rep(TRUE,7)))
      
      test.tib <- tibble::tibble(iid_proxy = c('321', 'srri_NA', 'no_change', 'srri_lipper', paste0("srri_", 1:7)))
      testthat::expect_equal(noTsNeeded(test.tib), c(FALSE, TRUE, TRUE, TRUE, rep(TRUE,7)))
      
      test.tib <- tibble::tibble(iid_proxy = c('srri_NA'))
      testthat::expect_equal(noTsNeeded(test.tib), TRUE)
    })


