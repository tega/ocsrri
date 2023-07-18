testthat::test_that('cycles works with strict = FALSE', {

      ## empty map
      h.df <- data.frame(iid=character(0), iid_proxy = character(0),
          stringsAsFactors = FALSE)
      testthat::expect_message(cycles(h.df, strict = FALSE, verbose = TRUE))
      testthat::expect_equal(cycles(h.df, strict = FALSE, verbose = FALSE)$info, 'no_cycles')


      ## single entry map
      h.df <- data.frame(iid="a", iid_proxy = "b",
          stringsAsFactors = FALSE)
      testthat::expect_message(cycles(h.df, strict = FALSE, verbose = TRUE))
      testthat::expect_equal(cycles(h.df, strict = FALSE, verbose = FALSE)$info, 'no_cycles')

      ## single entry autoreferencing map
      h.df <- data.frame(iid="a", iid_proxy = "a",
          stringsAsFactors = FALSE)
      testthat::expect_message(cycles(h.df, strict = FALSE, verbose = TRUE))
      result.l <- cycles(h.df, strict = FALSE, verbose = FALSE)
      testthat::expect_equal(result.l$info, 'cycles')
      should.df <- data.frame(iid = "a", iid_proxy = "a",
          stringsAsFactors = FALSE)
      testthat::expect_equal(result.l$map.df, should.df)

      ## multiple entry no_cycles
      h.df <- data.frame(
          iid =       c("a", "b", "c", "e", "d", "v", "z", "k"),
          iid_proxy = c("b", "c", "k", "h", "p", "z", "s", "q"),
          stringsAsFactors = FALSE)
      testthat::expect_message(cycles(h.df, strict = FALSE, verbose = TRUE))
      result.l <- cycles(h.df, strict = FALSE, verbose = FALSE)
      should.df <- data.frame(iid=character(0), iid_proxy = character(0),
          stringsAsFactors = FALSE)
      testthat::expect_equal(result.l$info, 'no_cycles')
      testthat::expect_equal(result.l$map.df, should.df)

      ## multiple entry cycles
      h.df <- data.frame(
          iid =       c("a", "b", "c", "e", "d", "v", "z", "k"),
          iid_proxy = c("b", "c", "k", "h", "p", "z", "s", "a"),
          stringsAsFactors = FALSE)
      testthat::expect_message(cycles(h.df, strict = FALSE, verbose = TRUE))
      result.l <- cycles(h.df, strict = FALSE, verbose = FALSE)
      testthat::expect_equal(result.l$info, 'cycles')
      should.df <- data.frame(
          iid =       c("a", "b", "c", "k"),
          iid_proxy = c("b", "c", "k", "a"),
          stringsAsFactors = FALSE)
      testthat::expect_equal(result.l$map.df, should.df)
    })

testthat::test_that('cycles works with strict = TRUE', {

      ## empty map
      h.df <- data.frame(iid=character(0), iid_proxy = character(0),
          stringsAsFactors = FALSE)
      testthat::expect_message(cycles(h.df, strict = TRUE, verbose = TRUE))
      testthat::expect_equal(cycles(h.df, strict = TRUE, verbose = FALSE)$info, 'no_cycles')

      ## single entry map
      h.df <- data.frame(iid="a", iid_proxy = "b",
          stringsAsFactors = FALSE)
      testthat::expect_message(cycles(h.df, strict = TRUE, verbose = TRUE))
      testthat::expect_equal(cycles(h.df, strict = TRUE, verbose = FALSE)$info, 'no_cycles')

      ## single entry autoreferencing map
      h.df <- data.frame(iid="a", iid_proxy = "a",
          stringsAsFactors = FALSE)
      result.l <- cycles(h.df, strict = TRUE, verbose = FALSE)
      testthat::expect_equal(result.l$info, 'cycles')
      should.df <- data.frame(iid = "a", iid_proxy = "a",
          stringsAsFactors = FALSE)
      testthat::expect_equal(result.l$map.df, should.df)

      ## multiple entry no_cycles
      h.df <- data.frame(
          iid =       c("a", "b", "c", "d", "e", "f", "g", "h"),
          iid_proxy = c("i", "l", "k", "m", "n", "z", "s", "q"),
          stringsAsFactors = FALSE)
      result.l <- cycles(h.df, strict = TRUE, verbose = FALSE)
      should.df <- data.frame(iid=character(0), iid_proxy = character(0),
          stringsAsFactors = FALSE)
      testthat::expect_equal(result.l$info, 'no_cycles')
      testthat::expect_equal(result.l$map.df, should.df)

      ## multiple entry cycles
      h.df <- data.frame(
          iid =       c("a", "b", "c", "d", "e", "f", "g", "h"),
          iid_proxy = c("i", "d", "k", "b", "p", "d", "s", "f"),
          stringsAsFactors = FALSE)
      testthat::expect_message(cycles(h.df, strict = TRUE, verbose = TRUE))
      result.l <- cycles(h.df, strict = TRUE, verbose = FALSE)
      testthat::expect_equal(result.l$info, 'cycles')
      should.df <- h.df[-c(1,3,5,7),]
      testthat::expect_equal(result.l$map.df, should.df)
    })
