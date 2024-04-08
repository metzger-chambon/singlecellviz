library(testthat)

test_that("clean_result works", {
  table_FindMarkers <- structure(list(soma_joinid = 0:5, p_val = c(5.18811365723345e-212,
                                     5.65958128571957e-205, 2.30896567047627e-181, 1.70620252421231e-159,
                                     2.42743845470443e-158, 2.71303095129113e-152), avg_log2FC = c(3.60865772227266,
                                     3.13194489523503, -4.50932623429474, 2.56000321031997, -3.3656354212912,
                                     -3.42672084421086), pct.1 = c(0.948, 0.919, 0.342, 0.828, 0.83,
                                     0.18), pct.2 = c(0.104, 0.08, 0.966, 0.102, 0.966, 0.894), p_val_adj = c(7.11497906952995e-208,
                                    7.76154977523582e-201, 3.16651552049115e-177, 2.33988614170477e-155,
                                    3.32898909678166e-154, 3.72065064660066e-148), obs_id = c("IL32",
                                    "CD3D", "HLA-DRA", "CD3E", "CD74", "HLA-DRB1")), row.names = c(NA,
                                   -6L), class = c("tbl_df", "tbl", "data.frame"))
  table_FindAllMarkers <- structure(list(soma_joinid = 0:5, p_val = c(2.00862870747046e-140,
                                    2.62407469749693e-140, 1.28016867716637e-138, 4.35882251877664e-135,
                                    3.61879283344212e-128, 5.61200678432926e-124), avg_log2FC = c(0.725673812389474,
                                    0.724284664457209, 0.674263013155929, 0.612102687168706, 0.617975585126337,
                                    0.750693573733954), pct.1 = c(1, 0.999, 1, 0.999, 1, 0.997),
                                    pct.2 = c(0.991, 0.992, 0.995, 0.995, 0.994, 0.975), p_val_adj = c(2.75463340942499e-136,
                                    3.59865604014729e-136, 1.75562332386596e-134, 5.97768920225028e-131,
                                    4.96281249178252e-124, 7.69630610402915e-120), cluster = structure(c(1L,
                                    1L, 1L, 1L, 1L, 1L), levels = c("0", "1", "2", "3", "4",
                                    "5", "6", "7", "8"), class = "factor"), gene = c("RPS12",
                                    "RPS27", "RPS6", "RPL32", "RPS14", "RPS25"), obs_id = c("RPS12",
                                    "RPS27", "RPS6", "RPL32", "RPS14", "RPS25")), row.names = c(NA,
                                    -6L), class = c("tbl_df", "tbl", "data.frame"))

  clean_FindMarkers <- clean_result(table_FindMarkers)
  clean_FindAllMarkers <- clean_result(table_FindAllMarkers)

  # Check if the returned object is of expected class
  expect_s3_class(clean_FindMarkers, "tbl_df")
  expect_s3_class(clean_FindAllMarkers, "tbl_df")

  # Check that we have the expected table columns
  expect_equal(colnames(clean_FindMarkers), c("p_val", "avg_log2FC", "pct.1", "pct.2", "p_val_adj", "gene"))
  expect_equal(colnames(clean_FindAllMarkers), c("p_val", "avg_log2FC", "pct.1", "pct.2", "p_val_adj", "cluster", "gene"))

  # Check that we have the expected table dimensions
  expect_equal(dim(clean_FindMarkers), c(6, 6))
  expect_equal(dim(clean_FindAllMarkers), c(6, 7))

})

test_that("clean_aggrexpression works", {
  aggrexpression_table <- structure(list(soma_joinid = 0:5, `0` = c(4.27167876975651, 0,
                                    5.1948051948052, 0, 13.325896188952, 346.731843220059), `1` = c(2.86286859433152,
                                    3.96353547364249, 0, 0, 11.9255031465829, 173.78893596259), `2` = c(23.3008240566137,
                                    5.22466039707419, 0, 5.04055778622253, 18.0179396173923, 130.81800067963
                                    ), `3` = c(0, 0, 7.10370585683927, 0, 13.3765807522148, 201.768003231294
                                    ), `4` = c(5.56792873051225, 3.22893122376493, 0, 0, 5.27983104540655,
                                   150.960270460667), `5` = c(0, 0, 0, 1.94401244167963, 2.38038562247084,
                                   81.0433754589399), `6` = c(0, 0, 0, 0, 9.07548852988481, 108.626486799493
                                   ), `7` = c(0, 0, 2.70811096843638, 0, 0, 10.5155634769647), `8` = c(0,
                                   0, 0, 0, 0, 0), obs_id = c("AL627309.1", "AP006222.2", "RP11-206L10.2",
                                  "RP11-206L10.9", "LINC00115", "NOC2L")), row.names = c(NA, -6L
                                   ), class = c("tbl_df", "tbl", "data.frame"))

  cleaned <- clean_aggrexpression(aggrexpression_table)

  # Check if the returned object is of expected class
  expect_s3_class(cleaned, "tbl_df")

  # Check that we have the expected table columns
  expect_equal(colnames(cleaned), c("gene", "0", "1", "2", "3", "4", "5", "6", "7", "8"))

  # Check that we have the expected table dimensions
  expect_equal(dim(cleaned), c(6, 10))
})

test_that("numeric_to_factor works", {
  cell_annotation <- structure(list(seurat_clusters = structure(c(2L, 4L, 2L, 3L,
                               7L, 2L), levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8"
                               ), class = "factor"), nFeature_RNA = c(779L, 1352L, 1129L, 960L,
                               521L, 781L), orig.ident = structure(c("pbmc3k", "pbmc3k", "pbmc3k", "pbmc3k", "pbmc3k", "pbmc3k"), class = "character")), row.names = c(NA,
                               6L), class = "data.frame")


  result <- numeric_to_factor(cell_annotation, n = 3)

  # Check if the returned object is of expected class
  expect_s3_class(result, "data.frame")

  # Check that we have the expected table dimensions
  expect_equal(dim(result), c(6, 3))

  # Check that we have the expected class in each columns
  expect_equal(sapply(result, FUN = function(x){class(x)}), c(seurat_clusters = "factor", nFeature_RNA = "factor", orig.ident = "character"))

  # Check that we have the expected factors bins
  expect_equal(levels(result$nFeature_RNA), c("[521,780]", "(780,1.02e+03]", "(1.02e+03,1.35e+03]"))

  })
