# No server here

# Test that ui works
test_that("module ui works", {
  ui <- mod_homepage_ui()
  expect_s3_class(ui, "html")
})

