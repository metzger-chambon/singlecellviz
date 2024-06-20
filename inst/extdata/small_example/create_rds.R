library(Seurat)
library(SeuratData)
devtools::load_all()
# or
# library(singlecellviz)

dir <- system.file("extdata/small_example",
                   package = "singlecellviz",
                   mustWork = TRUE)

InstallData("pbmc3k")
pbmc3k <- LoadData("pbmc3k", type = "pbmc3k.final")
seuratObj <- pbmc3k
rm(pbmc3k)

seuratObj <- UpdateSeuratObject(seuratObj)

seuratObj@meta.data$RNA_snn_res.0.5 <- NULL
seuratObj@meta.data$CellType <- seuratObj@active.ident

seuratObj <- FindVariableFeatures(seuratObj,
                                  nfeatures = 100)

seuratObj <- subset(seuratObj,
                    subset = CellType %in% c("DC", "Mk"),
                    features = VariableFeatures(seuratObj))

seuratObj <- FindNeighbors(seuratObj, dims = 1:10)
seuratObj <- FindClusters(seuratObj, resolution = 1.5)

seuratObj@meta.data$RNA_snn_res.1.5 <- NULL

summary(seuratObj@meta.data)



##################
# Process object to fit to tiledb
# Check which Ident is the current one

seuratObj@active.ident
# Check which assays you want to keep (and if you have all expected features)
seuratObj@assays
# Check which reductions to keep (and with what they were calculated)
seuratObj@reductions
# if the saved assay is not the same as the one used in the reduction write_soma() will:
# - skip PCA
# - add measurment UMAP and TSNE to current assay by default

# Remove some assays to get a lighter object
DefaultAssay(seuratObj) <- "RNA"
seuratObj <- DietSeurat(seuratObj, assay = "RNA")
# Add scale data as it is useful for AggregateExpression (used for heatmap plot)
seuratObj <- ScaleData(seuratObj)


# Compute marker gene tables
optMarkers <- list(
  list(
    name = "CellType-markers",
    Ident = "CellType",
    FindAllMarkers = list(only.pos = TRUE),
    AggregateExpression = list(assays = "RNA",
                               slot = "scale.data",
                               group.by = c("CellType")
    )
  ),
  list(
    name = "seurat_clusters-markers",
    Ident = "seurat_clusters",
    FindAllMarkers = list(only.pos = TRUE),
    AggregateExpression = list(assays = "RNA",
                               slot = "scale.data",
                               group.by = c("seurat_clusters")
    )
  )
)
markers <- compute_marker(seuratObj, optMarkers)


# Compute differential comparison tables
optComparison <- list(
  list(
    name = "seurat_clusters_1-in-DC-cells",
    subset = list(
      subset = substitute(CellType %in% c("DC"))
    ),
    Ident = "seurat_clusters",
    FindMarkers = list(
      ident.1 = c(1)
    ),
    AggregateExpression = list(
      assays = "RNA",
      slot = "scale.data",
      group.by = c("seurat_clusters")
    )
  )
)
comparison <- compute_comparison(seuratObj, optComparison)

# Save data
Idents(seuratObj) <- 'CellType'
# Remove scale as it takes up a lot of space (+1Go!)
seuratObj[["RNA"]]$scale.data <- NULL
format(object.size(seuratObj), "Mb")
format(object.size(markers), "Mb")
format(object.size(comparison), "Mb")
obj <- list(seuratObj = seuratObj, markers = markers, comparison = comparison)

saveRDS(obj, file.path(dir, "small_example.rds"))

yaml_content <- list(title = "small example",
                     output_folder = file.path(dir, "tiledb"),
                     rds = file.path(dir, "small_example.rds"),
                     description = "Dataset of Peripheral Blood Mononuclear Cells (PBMC) subsetted for Mk and DC cells. Freely available from 10X Genomics.",
                     doi = "10.1038/ncomms14049",
                     date = "16/01/2017",
                     force = TRUE)

yaml::write_yaml(yaml_content, file.path(dir, "config.yaml"))

# Populate tiledb
populate_tiledb(yaml = file.path(dir, "config.yaml"))

