# Author: gilbartv
# Aims:  - useful function to treat Seurat objects before feeding them
#          to populate_tiledb.R

library(Seurat)
library(SeuratObject)
library(yaml)
library(tidyr)

#' @description Remove assay(s) in seuratObj.
#'
#' @param seuratObj a Seurat Object
#' @param assay a vector of assay(s) name
#' @param keep a boolean saying whether the assay(s) given should be kept (TRUE) or removed (FALSE)
#'
#' @return a Seurat Object containing only desired assay(s)
#' @examples

#' @noRd

filter_assays <- function(seuratObj, assay, keep = TRUE){
  if (!keep){
    assays_to_keep <- Assays(seuratObj)[!Assays(seuratObj) %in% assay]
  } else {
    assays_to_keep <- assay
  }
  stopifnot("No assays to keep." = length(assays_to_keep) > 0)
  stopifnot("Selected assay not found in Seurat object" =
                             all(assays_to_keep %in% Assays(seuratObj)))
  DefaultAssay(seuratObj) <- assays_to_keep[1]
  assays_to_remove <- !Assays(seuratObj) %in% assays_to_keep
  names(assays_to_remove) <- Assays(seuratObj)

  for (assay in names(assays_to_remove)){
    if (assays_to_remove[assay]){
      seuratObj[[assay]] <- NULL
    }
  }
  return(seuratObj)
}

#' @description Compute all markers of a specific meta.data column.
#'
#' @param seuratObj a Seurat Object
#' @param opt a list like: list(
#'  list(
#'    name = "CellType markers",
#'    Ident = "CellType",
#'    FindAllMarkers = list(only.pos = TRUE,
#'                         min.pct = 0.25,
#'                         logfc.threshold = 0.5),
#'    AggregateExpression = list(assays = "RNA",
#'                              slot = "scale.data",
#'                              group.by = c("CellType", "orig.ident")
#'                          )
#'  )
#' )
#' @return a list like: list(
#'  "CellType markers" = list(
#'    table = result table of FindAllMarker
#'    aggrexpression = result of AggregateExpression
#'    )
#'  )
#' @examples
#' @noRd
compute_marker <- function(seuratObj, opt){
  cat("Calculating marker genes and aggrexpression.\n")
  res <- lapply(opt, function(y){
    cat("Dealing with", y$name, "\n")
    # Check parameters
    if (is.null(y$AggregateExpression$group.by)){
      y$AggregateExpression$group.by <- y$Ident
    }
    stopifnot("Value in AggregateExpression(group.by = ...) should be either c(Ident, SomeOtherAnnot) or c(Ident)" =
                y$Ident %in% y$AggregateExpression$group.by[1])
    stopifnot("Character underscore '_' in label name is not accepted in the current version of the app, please change your labels" =
                !any(sapply(levels(seuratObj@meta.data[[y$Ident]]), function(x){grepl("_", x)})))

    # Start processing
    seuratObj <- SetIdent(seuratObj, value = y$Ident)
    optFindAllMarkers <- y$FindAllMarkers
    optFindAllMarkers$object <- seuratObj
    # Same markers
    markers <- do.call(FindAllMarkers, args = optFindAllMarkers)
    # Save aggrexpression (used in the heatmap)
    optAggregateExpression <- y$AggregateExpression
    optAggregateExpression$object <- seuratObj
    aggrexpression <- do.call(AggregateExpression, args = optAggregateExpression)

    return(list(table = markers, aggrexpression = aggrexpression))
  })
  names(res) <- sapply(opt, '[[', 'name')
  return(res)
}

#' @description Compute markers for given.
#'
#' @param seuratObj a Seurat Object
#' @param opt a list like: list(
#'  list(
#'    name = "First comparison",
#'    subset = list(subset(substitute(CellType %in% c("Luminal C"))),
#'    Ident = "orig.ident",
#'    FindMarkers = list(ident.1 = PTEN),
#'    AggregateExpression = list(assays = "RNA",
#'                              slot = "scale.data",
#'                              group.by = c("orig.ident", "CellType")
#'                          )
#'  )
#' )
#' @return a list like: list(
#'  "First comparison" = list(
#'    table = result table of FindMarkers
#'    aggrexpression = result of AggregateExpression
#'    )
#'  )
#' @examples
#' @noRd
# Compute comparison tables
compute_comparison <- function(seuratObj, opt){
  cat("Calculating differential comparisons tables and related aggrexpression.\n")
  res <- lapply(opt, function(y){
    cat("Dealing with", y$name, "\n")
    # Check parameters
    if (is.null(y$AggregateExpression$group.by)){
      y$AggregateExpression$group.by <- y$Ident
    }
    stopifnot("Value in AggregateExpression(group.by = ...) should be either c(Ident, SomeOtherAnnot) or c(Ident)" =
                y$Ident %in% y$AggregateExpression$group.by[1])
    stopifnot("Character underscore '_' in label name is not accepted in the current version of the app, please change your labels" =
                !any(sapply(levels(seuratObj@meta.data[[y$Ident]]), function(x){grepl("_", x)})))

    # Subset cells of interest
    optsubset <- y$subset
    optsubset$x <- seuratObj
    seuratObj <- do.call(subset, args = optsubset)

    # SetIdent of interest
    seuratObj <- SetIdent(seuratObj, value = y$Ident)
    # FindMarkers of interest
    optFindMarkers <- y$FindMarkers
    optFindMarkers$object <- seuratObj
    comparison <- do.call(FindMarkers, args = optFindMarkers)
    # Save aggrexpression (used in the heatmap)
    optAggregateExpression <- y$AggregateExpression
    optAggregateExpression$object <- seuratObj
    aggrexpression <- do.call(AggregateExpression, args = optAggregateExpression)
    return(list(table = comparison, aggrexpression = aggrexpression))
  })
  names(res) <- sapply(opt, '[[', 'name')
  return(res)
}
