#' Compute marker
#' @description Compute several marker table and expression for
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
#' \item 'PseudobulkExpression', a list with \code{Seurat::PseudobulkExpression()}
#' parameters (except 'object', which is by default the Seurat object).
#' The name of each element in the list is the name of parameters of the function,
#' and its value is the value to give to the parameters.
#' }
#' @return a list of sub-lists (one for each individual marker table to compute).
#' Each sub-list is named with the 'name' given as input, and contains the elements:
#' \itemize{
#' \item 'table', the output of \code{Seurat::FindAllMarkers()}
#' \item 'expression', the output of \code{Seurat::PseudobulkExpression()}
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
#'    PseudobulkExpression = list(assay = "RNA",
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
#' @importFrom Seurat PseudobulkExpression SetIdent FindAllMarkers

compute_markers <- function(seuratObj, opt){
  cat("Calculating marker genes and expression.\n")
  res <- lapply(opt, function(y){
    cat("Dealing with", y$name, "\n")
    # Check parameters
    if (is.null(y$PseudobulkExpression$group.by)){
      y$PseudobulkExpression$group.by <- y$Ident
    }
    stopifnot("Value in PseudobulkExpression(group.by = ...) should be either c(Ident, SomeOtherAnnot) or c(Ident)" =
                y$Ident %in% y$PseudobulkExpression$group.by[1])
    stopifnot("Character underscore '_' in label name is not accepted in the current version of the app, please change your labels" =
                !any(sapply(levels(seuratObj@meta.data[[y$Ident]]), function(x){grepl("_", x)})))

    # Start processing
    seuratObj <- SetIdent(seuratObj, value = y$Ident)
    optFindAllMarkers <- y$FindAllMarkers
    optFindAllMarkers$object <- seuratObj
    # Same markers
    markers <- do.call(FindAllMarkers, args = optFindAllMarkers)
    # Save expression (used in the heatmap)
    optPseudobulkExpression <- y$PseudobulkExpression
    optPseudobulkExpression$object <- seuratObj
    expression <- do.call(PseudobulkExpression, args = optPseudobulkExpression)

    return(list(table = markers, expression = expression))
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
#' \item 'PseudobulkExpression', a list with \code{Seurat::PseudobulkExpression()}
#' parameters (except 'object').
#' The name of each element in the list is the name of parameters of the function,
#' and its value is the value to give to the parameters.
#' }
#' @return a list of sub-lists (one for each individual marker table to compute).
#' Each sub-list is named with the 'name' given as input, and contains the elements:
#' \itemize{
#' \item 'table', the output of \code{Seurat::FindMarkers()}
#' \item 'expression', the output of \code{Seurat::PseudobulkExpression()}
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
#'   PseudobulkExpression = list(assays = "RNA",
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
#' @importFrom Seurat PseudobulkExpression SetIdent FindMarkers

# Compute comparison tables
compute_comparison <- function(seuratObj, opt){
  cat("Calculating differential comparisons tables and related expression.\n")
  res <- lapply(opt, function(y){
    cat("Dealing with", y$name, "\n")
    # Check parameters
    if (is.null(y$PseudobulkExpression$group.by)){
      y$PseudobulkExpression$group.by <- y$Ident
    }
    stopifnot("Value in PseudobulkExpression(group.by = ...) should be either c(Ident, SomeOtherAnnot) or c(Ident)" =
                y$Ident %in% y$PseudobulkExpression$group.by[1])
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
    # Save expression (used in the heatmap)
    optPseudobulkExpression <- y$PseudobulkExpression
    optPseudobulkExpression$object <- seuratObj
    expression <- do.call(PseudobulkExpression, args = optPseudobulkExpression)
    return(list(table = comparison, expression = expression))
  })
  names(res) <- sapply(opt, '[[', 'name')
  return(res)
}



#' Populate TileDB-SOMA with marker or comparison tables
#' @description Populate TileDB-SOMA with a new SOMACollection
#' corresponding to marker or comparison result and expression tables
#' @param res a list with result and expression tables
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
    # Adding expression
    if (!'expression' %in% experiment$get(name)$get(group)$names()){
      experiment$get(name)$get(group)$set(
        object = write_soma(
          x = res[[group]]$expression[[1]] %>% as.data.frame(), # Warning, only retrieve the first calculated assay
          uri = 'expression',
          relative = TRUE,
          soma_parent = experiment$get(name)$get(group)
        ),
        name = 'expression'
      )
    }
  }
  experiment$close()
}

#' Check the validity of the object inside object.rds
#' @description Check the validity of the object inside object.rds
#' @param object a list with containg a seurat object and the results of
#' compute_markers and compute_comparison
#' @return TRUE if no error was raised
#' @importFrom Seurat Assays Reductions Cells
#' @importFrom SeuratObject Layers Features
#' @importFrom methods validObject
#' @noRd
#'
validity_rds <- function(object){
  # Check that all objects are present
  if (!all(c("seuratObj", "markers", "comparison") %in% names(object))){
    stop("Object list must contain elements 'seuratObj',
         'markers' and 'comparison'.")
  }

  # Check seuratObj
  if (!validObject(object$seuratObj)){
    stop("Element 'seuratObj' in object list must return
         is not a validObject().")
  }

  if (length(Assays(object$seuratObj)) < 1){
    stop("Element 'seuratObj' in object list must return
         at least one value in Assays().")
  }
  if (length(Reductions(object$seuratObj)) < 1){
    stop("Element 'seuratObj' in object list must return
         at least one value in Reductions().")
  }
  if (length(Layers(object$seuratObj)) < 1){
    stop("Element 'seuratObj' in object list must return
         at least one value in Layers().")
  }
  if (length(Features(object$seuratObj)) < 1){
    stop("Element 'seuratObj' in object list must return
         at least one value in Features().")
  }
  if (length(Cells(object$seuratObj)) < 1){
    stop("Element 'seuratObj' in object list must return
         at least one value in Cells().")
  }

  # Check markers
  if (length(object$markers) < 1){
    stop("Element 'markers' in object list must contain
         at least one element.")
  }
  validity <- sapply(object$markers, FUN = function(x){all(c("table", "expression") %in%
                                                             names(x))})
  if (!all(validity)){
    stop("Every elements of 'markers' in object list must contain
         elements 'table' and 'expression'.")
  }
  validity <- sapply(object$markers, FUN = function(x){nrow(x$table) > 1})
  if (!all(validity)){
    stop("Every 'table' in elements of 'markers' in object list must contain
         at least one row.")
  }
  validity <- sapply(object$markers, FUN = function(x){nrow(x$expression[[1]]) ==
                                                       length(Features(object$seuratObj))})
  if (!all(validity)){
    stop("Every 'expression' in elements of 'markers' in object list
         must contain as many rows as 'seuratObj' Features().")
  }

  # Check comparison
  if (length(object$comparison) < 1){
    stop("Element 'comparison' in object list must contain
         at least one element.")
  }
  validity <- sapply(object$comparison, FUN = function(x){all(c("table", "expression") %in%
                                                             names(x))})
  if (!all(validity)){
    stop("Every elements of 'comparison' in object list must contain
         elements 'table' and 'expression'.")
  }
  validity <- sapply(object$comparison, FUN = function(x){nrow(x$table) > 1})
  if (!all(validity)){
    stop("Every 'table' in elements of 'comparison' in object list must contain
         at least one row.")
  }
  validity <- sapply(object$comparison, FUN = function(x){nrow(x$expression[[1]]) ==
      length(Features(object$seuratObj))})
  if (!all(validity)){
    stop("Every 'expression' in elements of 'comparison' in object list
         must contain as many rows as 'seuratObj' Features().")
  }

  return(TRUE)
}

#' Populate TileDB-SOMA with a new dataset.
#' @description Populate TileDB-SOMA with a new dataset.
#' @param dataset_dir a character, corresponding to the path folder of the dataset.
#' It is where a object.rds file is expected, and where the tiledb folder will be created.
#' @param force a boolean, if a tiledb folder already exists, should it be removed?
#' @return None. It creates a TileDB-SOMA in a specified folder.
#' @export
#' @importFrom tiledbsoma write_soma

populate_tiledb <- function(dataset_dir, force = F){
  uri <- file.path(dataset_dir, "tiledb")

  # Check if such a folder already exists
  if (!force & dir.exists(uri)){
    cat(paste(uri, "already exists. If you want to",
              "update it, please, add option 'force = T'.\n"))
    return(NULL)
  }
  if (dir.exists(uri)){
    cat(sprintf("Removing existing tiledb.\n"))
    unlink(uri, recursive = TRUE)
  }

  # Load the seurat object
  cat(sprintf("Loading %s/object.rds.\n", dataset_dir))
  obj <- readRDS(file.path(dataset_dir, "object.rds"))

  cat(sprintf("Checking the validity of rds object.\n"))
  validity_rds(obj)

  # Populate the database with the seurat object
  cat(sprintf("Populating %s with Seurat object.\n", uri))
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
#' searching for \code{config.yaml}, \code{object.rds} and
#' \code{tiledb/} in each subfolder of specified
#' \code{db_dir}.
#'
#' The order of the datasets is alphanumerical according to the
#' name of the folder containing them. Therefore, to change the order of the datasets,
#' simply add the desired position to the folder name
#' (e.g. if the folder \code{zzz/} contains the first dataset to
#' show, rename it to \code{1_zzz/}).
#' @param db_dir a character, corresponding to the path of the database
#' (containing subfolders)
#' @param output_file a character, corresponding to the filename
#' to give to the txt. It will automatically be in the \code{db_dir}.
#' @param config_filename a character, corresponding to the name of the
#' yaml configuration files to look for (by default  \code{"config.yaml"})
#' @return None. It creates a txt file.
#' @examples
#' \dontrun{
#' update_summary("db/", "dataset_summary.txt")
#' }
#' @export
#' @importFrom utils write.table
#' @importFrom stringr str_sort
#'
update_summary <- function(db_dir,
                           output_file,
                           config_filename = "config.yaml"){

  studies <- list()
  folders <- list.dirs(db_dir, recursive = F)
  folders <- folders[file.exists(file.path(folders, "config.yaml"))]
  folders <- stringr::str_sort(folders, numeric = TRUE)

  for (folder in folders){
    x <- yaml::read_yaml(file.path(folder, "config.yaml"), eval.expr=TRUE)
    uri <- file.path(folder, "tiledb/")
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
                        description = sapply(studies, '[[', "description"),
                        doi = sapply(studies, '[[', "doi"),
                        date = sapply(studies, '[[', "date"),
                        nsamples = sapply(studies, '[[', "nsamples"),
                        nfeatures = sapply(studies, '[[', "nfeatures"),
                        ncells = sapply(studies, '[[', "ncells"))

  studies$output <- file.path(basename(folders), "tiledb/")
  studies$rds <- file.path(basename(folders), "object.rds")

  rownames(studies) <- NULL

  write.table(studies, file = file.path(db_dir, output_file), sep = '\t', quote = FALSE,
              row.names = FALSE, col.names = TRUE)
}

