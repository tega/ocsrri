testthat::test_that('proxyMapValidityCheck is fine', {
  empty.df <- data.frame(iid = character(0), iid_proxy = character(0),
      stringsAsFactors = FALSE)
  proxyMap.tib <- data.frame(iid = c("1", "2", "3"), iid_proxy = c("4", "5", "6"),
                             stringsAsFactors = FALSE)
  result.l <- proxyMapValidityCheck(proxyMap.tib)
  testthat::expect_equal(result.l, list(info = "no_cycles", map.df = empty.df))

  proxyMap.tib <- data.frame(iid = c("1", "2", "3"), iid_proxy = c("2", "3", "6"),
                             stringsAsFactors = FALSE)
  result.l <- proxyMapValidityCheck(proxyMap.tib)
  should.df <- as.data.frame(proxyMap.tib)
  rownames(should.df) <- NULL
  testthat::expect_equal(result.l$map.df, should.df)
})

testthat::test_that('proxyMapValidityCheck warn action', {
  proxyMap.tib <- data.frame(iid = c("1", "2", "3"), iid_proxy = c("2", "3", "6"),
                             stringsAsFactors = FALSE)
  testthat::expect_message(proxyMapValidityCheck(proxyMap.tib, action = 'warn'))
})

testthat::test_that('proxyMapValidityCheck stop action', {
  proxyMap.tib <- data.frame(iid = c("1", "2", "3"), iid_proxy = c("2", "3", "6"),
                             stringsAsFactors = FALSE)
  testthat::expect_error(proxyMapValidityCheck(proxyMap.tib, action = 'stop'))
})
