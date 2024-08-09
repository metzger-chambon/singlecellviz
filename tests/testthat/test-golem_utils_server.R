library(testthat)

test_that("add_suffix and remove_suffix works", {
  test <- c("aaaa", "bb", "c")
  # For an unnamed vector, the content should have the suffix but not the names
  expect_equal(add_suffix(test, "1"), c(aaaa = "aaaa_1", bb = "bb_1", c = "c_1"))

  # For a named vector, add_suffix and remove_suffix should cancel out
  names(test) <- c("AAAA", "BB", "C")
  expect_equal(remove_suffix(add_suffix(test, "XX"), "XX"), test)
})


