library(testthat)

test_that("DATA class works", {

  COMMON_DATA <- DATA$new()
  expect_s3_class(COMMON_DATA, "DATA")
  expect_null(COMMON_DATA$output)
  expect_null(COMMON_DATA$title)

  COMMON_DATA$output <- test_path("testdata", "experiment-pbmc10")

  # # Test the opening of the SOMAExperiment
  # expect_s3_class(COMMON_DATA$experiment, "SOMAExperiment")
  # # Test the content in the SOMAExperiment
  # expect_equal(COMMON_DATA$groups, c("RNA"))
  # expect_equal(COMMON_DATA$arrays, c("scale_data", "data", "counts"))
  # expect_equal(COMMON_DATA$markers, c("seurat_clusters"))
  # expect_equal(COMMON_DATA$comparison, c("Cluster 1 vs 0"))
})
