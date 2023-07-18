testthat::test_that('get_proxySRRI_OC from lix', {
  lix <- list('P41' = tibble::tibble('Idtf' = '38668', 'RatingAgency' = 'SRRI_OC', 'RatingCode' = '5',
                                     'StartDate' = as.Date('2021-01-09'), 'Expiry' = as.Date(NA_character_)))

  li <- list('P42' = tibble::tibble('Idtf' = '12345', 'IndClass' = 'OC_SRRI_proxy',
                                    'IndCode' = '38668', 'StartDate' = as.Date('2021-01-09'), 'Expiry' = as.Date(NA_character_)),
             'P41' = tibble::tibble('Idtf' = '552234', 'RatingAgency' = 'SRRI_OC',
                                    'RatingCode' = '7', 'StartDate' = as.Date('2021-01-09'), 'Expiry' = as.Date(NA_character_)))

  result <- get_proxySRRI_OC('12344', as.Date('2021-01-09'), li, lix)
  testthat::expect_equal(result, NA_character_)

  result <- get_proxySRRI_OC('12345', as.Date('2021-01-09'), li, lix)
  testthat::expect_equal(result, "5")
})

testthat::test_that('get_proxySRRI_OC from li', {
  lix <- list('P41' = tibble::tibble('Idtf' = '38668', 'RatingAgency' = 'SRRI_OC', 'RatingCode' = '5',
                                     'StartDate' = as.Date('2021-01-09'), 'Expiry' = as.Date(NA_character_)))

  li <- list('P42' = tibble::tibble('Idtf' = '12345', 'IndClass' = 'OC_SRRI_proxy',
                                    'IndCode' = '552234', 'StartDate' = as.Date('2021-01-09'), 'Expiry' = as.Date(NA_character_)),
             'P41' = tibble::tibble('Idtf' = '552234', 'RatingAgency' = 'SRRI_OC',
                                    'RatingCode' = '7', 'StartDate' = as.Date('2021-01-09'), 'Expiry' = as.Date(NA_character_)))

  result <- get_proxySRRI_OC('12345', as.Date('2021-01-09'), li, lix)
  testthat::expect_equal(result, "7")
})
