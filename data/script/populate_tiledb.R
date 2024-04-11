# Author: gilbartv
# Aims:  - populate a tileDB arrays using the SOMA data model
#          from a yaml containing paths to seurat objects
#          following https://tiledb-inc.github.io/tiledbsc/articles/quickstart.html#populate-a-somacollection
#        - create a summary file of the datasets

library(tiledbsoma)
library(tiledb)
library(Seurat)
library(SeuratObject)
library(yaml)
library(tidyr)



# Compute marker gene tables
# Populate database with a new "name" SOMACollection containing 2 tables:
#   in res$result: the result of FindMarkers()
#   in res$aggrexpression: the result of AggregateExpression()
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



########################################
# Populate the tiledb database
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



