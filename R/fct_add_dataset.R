#' Compute marker
#' @description Compute several marker table and aggrexpression for
#' a dataset. The aim of this function is to facilitate creating a marker object
#' in the correct format, in order to add it to the database.
#'
#' @param seuratObj a Seurat object
#' @param opt a list of sub-lists (one for each individual marker table to compute),
#' with elements:
#' \itemize{
#' \item 'name', a character being the name that will appear in the app
#' \item 'Ident', a character being the Idents of the Seurat object
#' to use for \code{Seurat::FindAllMarkers()}
#' \item 'FindAllMarkers', a list with \code{Seurat::FindAllMarkers()} parameters
#' (except 'object', which is by default the Seurat object).
#' The name of each element in the list is the name of parameters of the function,
#' and its value is the value to give to the parameters.
#' \item 'AggregateExpression', a list with \code{Seurat::AggregateExpression()}
#' parameters (except 'object', which is by default the Seurat object).
#' The name of each element in the list is the name of parameters of the function,
#' and its value is the value to give to the parameters.
#' }
#' @return a list of sub-lists (one for each individual marker table to compute).
#' Each sub-list is named with the 'name' given as input, and contains the elements:
#' \itemize{
#' \item 'table', the output of \code{Seurat::FindAllMarkers()}
#' \item 'aggrexpression', the output of \code{Seurat::AggregateExpression()}
#' }
#' @examples
#' \dontrun{
#' data("small_seurat")
#' opt <- list(
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
#'  ) #,
#'  # list(
#'  #  name = "seurat_clusters markers",
#'  #  #...
#'  # )
#' )
#' compute_markers(small_seurat, opt)
#' }
#' @export
#' @importFrom Seurat AggregateExpression SetIdent FindAllMarkers

compute_markers <- function(seuratObj, opt){
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

#' Compute comparison
#' @description Compute comparison tables.
#'
#' @param seuratObj a Seurat object
#' @param  opt a list of sub-lists (one for each individual comparison to compute),
#' with elements:
#' \itemize{
#' \item 'name', a character being the name that will appear in the app
#' \item 'subset', a list with \code{subset()} parameters
#' (except 'x', which is by default the Seurat object).
#' \item 'Ident', a character being the Idents of the Seurat object to use for
#' \code{Seurat::FindMarkers()}
#' \item 'FindMarkers', a list with \code{Seurat::FindMarkers()} parameters
#' (except 'object', which is by default the Seurat object).
#' The name of each element in the list is the name of parameters of the function,
#' and its value is the value to give to the parameters.
#' \item 'AggregateExpression', a list with \code{Seurat::AggregateExpression()}
#' parameters (except 'object').
#' The name of each element in the list is the name of parameters of the function,
#' and its value is the value to give to the parameters.
#' }
#' @return a list of sub-lists (one for each individual marker table to compute).
#' Each sub-list is named with the 'name' given as input, and contains the elements:
#' \itemize{
#' \item 'table', the output of \code{Seurat::FindMarkers()}
#' \item 'aggrexpression', the output of \code{Seurat::AggregateExpression()}
#' }
#' @examples
#' \dontrun{
#' data("small_seurat")
#' opt <- list(
#' list(
#'   name = "First comparison",
#'   subset = list(
#'     subset = substitute(CellType %in% c("DC"))
#'   ),
#'   Ident = "seurat_clusters",
#'   FindMarkers = list(ident.1 = 1),
#'   AggregateExpression = list(assays = "RNA",
#'                              slot = "scale.data",
#'                              group.by = c("seurat_clusters", "CellType")
#'   )
#' ) #,
#' #list(
#' #   name = "Second comparison",
#' #   ...
#' #)
# )
#' )
#' compute_comparison(small_seurat, opt)
#' }
#' @export
#' @importFrom Seurat AggregateExpression SetIdent FindMarkers

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



#' Populate TileDB-SOMA with marker or comparison tables
#' @description Populate TileDB-SOMA with a new SOMACollection
#' corresponding to marker or comparison result and aggrexpression tables
#' @param res a list with result and aggrexpression tables
#' @param uri the TileDB-SOMA uri to add to
#' @param name the name of the SOMACollection
#' @param force whether to regenerate the SOMACollection if it already exists
#' @return None. It adds SOMACollection to a specified TileDB-SOMA.
#' @noRd
#'
populate_with_res <- function(res, uri, name, force = FALSE){
  # Check if such a folder already exists
  path <- file.path(uri, name)
  if (!force & dir.exists(path)){
    cat(paste(path, "already exists. If you want to",
              "update it, please, add 'force = TRUE' in the populate_with_res() function.\n"))
    return(NULL)
  }
  if (dir.exists(path)){
    cat(sprintf("Removing existing files in %s.\n", path))
    unlink(path, recursive = TRUE)
  }

  cat("Populating", uri, "with", name, "tables.\n")
  experiment <- SOMAOpen(file.path(uri), mode = "WRITE")
  if (!name %in% experiment$names()){
    experiment$set(
      object = SOMACollectionCreate(
        uri = file.path(uri, name)
      ),
      name = name
    )
  }
  for (group in names(res)) {
    cat(paste0("  Adding ", group, ' ', name, ".\n"))
    if (!group %in% experiment$get(name)$names()){
      experiment$get(name)$set(
        object = SOMACollectionCreate(
          uri = file.path(uri, name, group)
        ),
        name = group
      )
    }
    # Adding table
    if (!'result' %in% experiment$get(name)$get(group)$names()){
      experiment$get(name)$get(group)$set(
        object = write_soma(
          x = res[[group]]$table,
          uri = 'result',
          relative = TRUE,
          soma_parent = experiment$get(name)$get(group)
        ),
        name = 'result'
      )
    }
    # Adding aggrexpression
    if (!'aggrexpression' %in% experiment$get(name)$get(group)$names()){
      experiment$get(name)$get(group)$set(
        object = write_soma(
          x = res[[group]]$aggrexpression[[1]] %>% as.data.frame(), # Warning, only retrieve the first calculated assay
          uri = 'aggrexpression',
          relative = TRUE,
          soma_parent = experiment$get(name)$get(group)
        ),
        name = 'aggrexpression'
      )
    }
  }
  experiment$close()
}



#' Populate TileDB-SOMA with a new dataset.
#' @description Populate TileDB-SOMA with a new dataset.
#' @param yaml a character, corresponding to the path file of a
#' config.yaml file for a dataset. Check out the section 'Adding a new dataset'
#' in \code{vignette("database", package = "singlecellviz")}
#' for a description of the config.yaml file.
#' @return None. It creates a TileDB-SOMA in a specified folder.
#' @export
#' @importFrom yaml read_yaml
#' @importFrom tiledbsoma write_soma

populate_tiledb <- function(yaml){
  x <- read_yaml(yaml, eval.expr=TRUE)
  uri <- file.path(x$output_folder)
  cat(paste0("##################\n",
             sprintf("Treating %s.\n", x$title)))
  # Load the seurat object
  cat(sprintf("Loading %s rds object.\n", x$title))
  obj <- readRDS(x$rds)

  # Check if such a folder already exists
  if (!x$force & dir.exists(uri)){
    cat(paste(x$output_uri, "already exists. If you want to",
              "update it, please, add 'force: true' in the YAML.\n"))
    return(NULL)
  }
  if (dir.exists(uri)){
    cat(sprintf("Removing existing %s tiledb.\n", x$title))
    unlink(uri, recursive = TRUE)
  }

  cat(sprintf("Creating %s tiledb.\n", x$title))
  # Populate the database with the seurat object
  write_soma(obj$seuratObj, uri)

  # Save comparison gene tables in tiledb under the "comparison" collection
  populate_with_res(obj$comparison, uri, name = 'comparison')
  # Save marker gene tables in tiledb under the "marker" collection
  populate_with_res(obj$marker, uri, name = 'markers')

  return(NULL)
}



#' Update dataset summary
#' @description Creates a txt file summarizing information about
#' all available datasets by automatically
#' searching for \code{config.yaml} files in each subfolder of specified
#' \code{db_dir}.
#' @param db_dir a character, corresponding to the path of the database
#' (containing subfolders)
#' @param output_file a character, corresponding to the path and filename
#' to give to the txt file created
#' @param config_filename a character, corresponding to the name of the
#' yaml configuration files to look for (by default  \code{"config.yaml"})
#' @return None. It creates a txt file.
#' @examples
#' \dontrun{
#' update_summary("db/", "dataset_summary.txt")
#' }
#' @export
#' @importFrom utils write.table
#'
update_summary <- function(db_dir, output_file, config_filename = "config.yaml"){
  get_studies <- function(){
    studies <- list()
    files <- list.files(path = db_dir,
                        pattern = config_filename,
                        recursive = TRUE, full.names = TRUE)
    for (file in files){
      x <- read_yaml(file, eval.expr=TRUE)
      uri <- file.path(x$output_folder)
      experiment <- SOMAOpen(uri)
      assay <- ifelse("RNA" %in% experiment$ms$names(),
                               "RNA",
                      experiment$ms$names()[1])
      query <- SOMAExperimentAxisQuery$new(
        experiment = experiment,
        measurement_name = assay
      )
      x$ncells <- query$n_obs
      x$nfeatures <- query$n_vars
      metadata_col <- ifelse("orig.ident" %in% experiment$obs$attrnames(),
                             "orig.ident",
                             experiment$obs$attrnames()[1])
      x$nsamples <- nrow(unique(
        experiment$obs$read(column_names = c(metadata_col))$concat()$to_data_frame()
        ))

      studies[x$title] <- list(x)
    }

    studies <- data.frame(title = sapply(studies, '[[', "title"),
                          output = sapply(studies, '[[', "output_folder"),
                          rds = sapply(studies, '[[', "rds"),
                          description = sapply(studies, '[[', "description"),
                          doi = sapply(studies, '[[', "doi"),
                          date = sapply(studies, '[[', "date"),
                          nsamples = sapply(studies, '[[', "nsamples"),
                          nfeatures = sapply(studies, '[[', "nfeatures"),
                          ncells = sapply(studies, '[[', "ncells"))

    rownames(studies) <- NULL
    return(studies)
  }

  studies <- get_studies()
  # If you want to change the order of the datasets (default is alphabetical)
  studies <- studies %>% slice(3, 4, 1, 2)

  write.table(studies, file = output_file, sep = '\t', quote = FALSE,
              row.names = FALSE, col.names = TRUE)
}

