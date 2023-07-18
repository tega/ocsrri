testthat::test_that('only_active_li works', {
       
    ## test 1
    li <- list( P03 = 1, P33 = 1, P39 = 1)
    testthat::expect_error(only_active_li(li), 
      regexp="Exactly one Pxx file between P01 and P04 is mandatory in order to remove inactive investments!")
    
    ## test 2
    li <- list(P01 = 1, P03 = 1, P330 = 1, P331 = 1)
    testthat::expect_error(only_active_li(li), regexp="P330, P331")  
    
    ## test 3
    P01 <- tibble::tibble(
      Idtf5 = c("a", "1"), 
      InvStatus = c("1", "0"))
    
    P03 <- tibble::tibble(
      '#Idtf' = c("1", "a"), 
      test = c("test 1", "test 2"))
    
    P33 <- tibble::tibble(
      Idtf = c("1", "a"), 
      test = c("test 1", "test 2"))
    
    li <- list(P01 = P01, P03 = P03, P33 = P33)
    
    P01e <- tibble::tibble(
      Idtf5 = "a", 
      InvStatus = "1")
    
    P03e <- tibble::tibble(
      '#Idtf' = "a", 
      test = "test 2")
    
    P33e <- tibble::tibble(
      Idtf = "a", 
      test = "test 2")
    
    expected_l <- list(P01 = P01e, P03 = P03e, P33 = P33e)
    
    result_l <- only_active_li(li) 
    testthat::expect_equal(result_l, expected_l)
    
    ## test 1
    li <- list( P01 = 3, P04=3, P03 = 1, P33 = 1, P39 = 1)
    testthat::expect_error(only_active_li(li), 
      regexp="Only one Pxx file between P01 and P04 can be in the li list argument")
    
  })
