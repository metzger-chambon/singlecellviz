library(testthat)

test_that("DimPlot works", {
  table_cell <- structure(list(V1 = c(10.5895051956177, 15.6335277557373, 13.046275138855,
                                      26.6680164337158, 13.3903284072876, 10.344783782959), V2 = c(14.850136756897,
                                     26.4807929992676, 17.4865455627441, 14.7715759277344, 9.01217460632324,
                                     16.3305702209473), seurat_clusters = structure(c(2L, 4L, 2L,
                                    3L, 7L, 2L), levels = c("0", "1", "2", "3", "4", "5", "6", "7",
                                    "8"), class = "factor"), nFeature_RNA = c(779L, 1352L, 1129L,
                                    960L, 521L, 781L), orig.ident = structure(c("pbmc3k", "pbmc3k",
                                   "pbmc3k", "pbmc3k", "pbmc3k", "pbmc3k"), class = "character")), class = "data.frame", row.names = c(NA,
                                    6L))

  table_gene <- structure(list(V1 = c(10.5895051956177, 15.6335277557373, 13.046275138855,
                                      26.6680164337158, 13.3903284072876, 10.344783782959), V2 = c(14.850136756897,
                                     26.4807929992676, 17.4865455627441, 14.7715759277344, 9.01217460632324,
                                     16.3305702209473), AGRN = c(0, 0, 0, 0, 0, 0), SDF4 = c(0, 0,
                                     1.4297439776322, 0, 0, 0)), class = "data.frame", row.names = c(NA, 6L))

  plot1 <- DimPlot(table = table_cell, features = "orig.ident")
  plot2 <- DimPlot(table = table_gene, features = c("AGRN", "SDF4"))

  # Check if the returned object is of expected class
  expect_s3_class(plot1, "patchwork")
  expect_s3_class(plot2, "patchwork")

  # Check that we have the expected plot lengths
  expect_length(plot1, 1)
  expect_length(plot2, 2)

  })

test_that("VlnPlot works", {

  table <- structure(list(AGRN = c(0, 3, 0, 0, 0, 0), SDF4 = c(0, 0, 1.4297439776322,
                           0, 0, 0), seurat_clusters = structure(c(2L, 3L, 2L, 3L, 3L, 2L
                           ), levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8"), class = "factor"),
                          nFeature_RNA = structure(c(2L, 10L, 8L, 6L, 1L, 4L), levels = c("[521,650]",
                          "(650,779]", "(779,780]", "(780,781]", "(781,870]", "(870,960]",
                          "(960,1.04e+03]", "(1.04e+03,1.13e+03]", "(1.13e+03,1.24e+03]",
                          "(1.24e+03,1.35e+03]"), class = "factor"), orig.ident = structure(c("pbmc3k",
                          "pbmc3k", "pbmc3k", "pbmc3k", "pbmc3k", "pbmc3k"), class = "character")), class = "data.frame", row.names = c("AAACATACAACCAC",
                          "AAACATTGAGCTAC", "AAACATTGATCAGC", "AAACCGTGCTTCCG", "AAACCGTGTATGCG",
                          "AAACGCACTGGTAC"))


  plot1 <- VlnPlot(table = table, features = c("AGRN", "SDF4"), group = "orig.ident", split.by = "orig.ident")
  plot2 <- VlnPlot(table = table, features = c("AGRN"), group = "seurat_clusters", split.by = "seurat_clusters")

  # Check if the returned object is of expected class
  expect_s3_class(plot1, "patchwork")
  expect_s3_class(plot2, "patchwork")

  # Check that we have the expected plot length
  expect_length(plot1, 2)
  expect_length(plot2, 1)

  })

test_that("DotPlot works", {

  table <- structure(list(AGRN = c(0, 3, 0, 0, 0, 0), SDF4 = c(0, 0, 1.4297439776322,
                       0, 0, 0), seurat_clusters = structure(c(2L, 3L, 2L, 3L, 3L, 2L
                       ), levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8"), class = "factor"),
                        nFeature_RNA = structure(c(2L, 10L, 8L, 6L, 1L, 4L), levels = c("[521,650]",
                        "(650,779]", "(779,780]", "(780,781]", "(781,870]", "(870,960]",
                        "(960,1.04e+03]", "(1.04e+03,1.13e+03]", "(1.13e+03,1.24e+03]",
                        "(1.24e+03,1.35e+03]"), class = "factor"), orig.ident = structure(c("pbmc3k",
                        "pbmc3k", "pbmc3k", "pbmc3k", "pbmc3k", "pbmc3k"), class = "character")), class = "data.frame",
                        row.names = c("AAACATACAACCAC", "AAACATTGAGCTAC", "AAACATTGATCAGC", "AAACCGTGCTTCCG", "AAACCGTGTATGCG","AAACGCACTGGTAC"))

  table1 <- table[, c("AGRN", "SDF4", "orig.ident")]
  table2 <- table[, c("AGRN", "seurat_clusters")]
  plot2 <- DotPlot(table = table2, features = c("AGRN"), group = "seurat_clusters")

  # Check if the returned object is of expected class
  expect_s3_class(plot2, "ggplot")

})

test_that("HeatmapPlot works", {

  table_nosplit <- structure(list(gene = structure(c(2L, 2L, 3L, 3L, 1L, 1L, 4L,
                                       4L), levels = c("Ifi202b", "Ugdh", "Igha", "Olfr1034"), class = "factor"),
                    group = c("PTEN3", "PTENHIF3", "PTEN3", "PTENHIF3", "PTEN3",
                              "PTENHIF3", "PTEN3", "PTENHIF3"), expression = c(2.657e+09,
                     898800000, 144300000, 244300000, 1.881e+09, 76030000, 36320000,
                     205700000), marker_of = c(Ugdh = "PTEN3", Ugdh = "PTEN3",
                                               Igha = "PTENHIF3", Igha = "PTENHIF3", Ifi202b = "PTEN3",
                                               Ifi202b = "PTEN3", Olfr1034 = "PTENHIF3", Olfr1034 = "PTENHIF3"
                     ), text = c("Gene: Ugdh\nExpression: 2.657e+09\nMarker of group: PTEN3",
                                 "Gene: Ugdh\nExpression: 898800000\nMarker of group: PTEN3",
                                 "Gene: Igha\nExpression: 144300000\nMarker of group: PTENHIF3",
                                 "Gene: Igha\nExpression: 244300000\nMarker of group: PTENHIF3",
                                 "Gene: Ifi202b\nExpression: 1.881e+09\nMarker of group: PTEN3",
                                 "Gene: Ifi202b\nExpression: 76030000\nMarker of group: PTEN3",
                                 "Gene: Olfr1034\nExpression: 36320000\nMarker of group: PTENHIF3",
                                 "Gene: Olfr1034\nExpression: 205700000\nMarker of group: PTENHIF3"
                     )), row.names = c(NA, -8L), class = c("tbl_df", "tbl", "data.frame"
                                                           ))

  table_split <- structure(list(gene = structure(c(2L, 2L, 3L, 3L, 1L, 1L, 4L,
                                                   4L), levels = c("Ifi202b", "Ugdh", "Ifi202b", "Ugdh"), class = "factor"),
                                group = c("A", "B", "A", "B", "A",
                                          "B", "A", "B"),
                                group2 = c("1", "1", "1", "1", "2",
                                           "2", "2", "2"),
                                expression = c(2.657e+09,
                                               898800000, 144300000, 244300000, 1.881e+09, 76030000, 36320000,
                                               205700000), marker_of = c(Ugdh = "PTEN3", Ugdh = "PTEN3",
                                                                         Igha = "PTENHIF3", Igha = "PTENHIF3", Ifi202b = "PTEN3",
                                                                         Ifi202b = "PTEN3", Olfr1034 = "PTENHIF3", Olfr1034 = "PTENHIF3"
                                               ), text = c("Gene: Ugdh\nExpression: 2.657e+09\nMarker of group: PTEN3",
                                                           "Gene: Ugdh\nExpression: 898800000\nMarker of group: PTEN3",
                                                           "Gene: Igha\nExpression: 144300000\nMarker of group: PTENHIF3",
                                                           "Gene: Igha\nExpression: 244300000\nMarker of group: PTENHIF3",
                                                           "Gene: Ifi202b\nExpression: 1.881e+09\nMarker of group: PTEN3",
                                                           "Gene: Ifi202b\nExpression: 76030000\nMarker of group: PTEN3",
                                                           "Gene: Olfr1034\nExpression: 36320000\nMarker of group: PTENHIF3",
                                                           "Gene: Olfr1034\nExpression: 205700000\nMarker of group: PTENHIF3"
                                               )), row.names = c(NA, -8L), class = c("tbl_df", "tbl", "data.frame"
                                               ))


  plot1 <- HeatmapPlot(table = table_nosplit, split = FALSE)
  plot2 <- HeatmapPlot(table = table_split, split = TRUE)

  # Check if the returned object is of expected class
  expect_s3_class(plot1, "ggplot")
  expect_s3_class(plot2, "ggplot")

  # Check that splitting is working
  expect_s3_class(plot1$facet, "FacetNull")
  expect_s3_class(plot2$facet, "FacetGrid")

})
