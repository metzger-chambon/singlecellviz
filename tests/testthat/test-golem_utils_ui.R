
test_that("Test enurl works", {
  expect_s3_class(enurl("https://www.thinkr.fr", "ThinkR"), "shiny.tag")
  expect_equal(
    as.character(enurl("https://www.thinkr.fr", "ThinkR")),
    '<a href="https://www.thinkr.fr">ThinkR</a>'
  )
})
