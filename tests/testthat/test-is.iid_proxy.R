test_that("is.iid_proxy works", {
      
      map.tib <- tibble::tibble(iid = c("a", "b"), type = rep("inv", 2), 
          iid_proxy = c("x", "dynamic"), type_proxy = rep("idx",2))
           
      testthat::expect_equal(is.iid_proxy("dynamic", map.tib), c(FALSE, TRUE))
      testthat::expect_equal(is.iid_proxy("c", map.tib), c(FALSE, FALSE))
      map.tib <- map.tib[-(1:2),]
      testthat::expect_equal(is.iid_proxy("c", map.tib), logical(0))
      
    })
