#' @importFrom R6 R6Class
#' @import tiledbsoma

DATA <- R6::R6Class(
    "DATA",
    public = list(
      title = NULL,
      # description = NULL,
      # doi = NULL,
      # date = NULL,
      # ncells = NULL,
      # nfeatures = NULL,
      # nsamples = NULL,
      output = NULL,
      initialize = function(){
        # Some initialization if needed
      },
      tabs_updated = c("homepage" = FALSE,
                       "information" = FALSE,
                       "explore" = FALSE,
                       "markers" = FALSE,
                       "differential" = FALSE)
      # update_tiledb = function(){
      #   if(!is.null(self$output)){
      #     uri <- file.path("../singlecelldatabase/db/tiledb", self$output)
      #     experiment <- tiledbsoma::SOMAOpen(uri) # need to specify the library for callr
      #     self$experiment <- experiment
      #     self$cell_annotation <- self$experiment$obs$attrnames()
      #     group <- self$experiment$ms$names()[1] # usually RNA
      #     self$genes <- self$experiment$ms$get(group)$get("var")$read(column_names = c("var_id"))$concat()$var_id$as_vector()
      #   }
      # }
    ),
    active = list(
      experiment = function(){
        if(!is.null(self$output)){
          uri <- file.path(self$output)
          experiment <- tiledbsoma::SOMAOpen(uri)
          return(experiment)
        } else {
          return(NULL)
        }
      },
      # cell_annotation = function(){
      #   if(!is.null(self$output)){
      #     experiment <- self$experiment
      #     cell_annotation <- experiment$obs$attrnames()
      #     return(cell_annotation)
      #   } else {
      #     return(NULL)
      #   }
      # },
      groups = function(){
        if(!is.null(self$output)){ # ask self$output and not self$experiment to not have a new api request
          order <- c("integrated", "ChromatinAssay", "SCT", "RNA")
          groups <- self$experiment$ms$names() # RNA, SCT, integrated
          groups <- order[which(order %in% groups)]
          stopifnot("Error, cannot find a valid assay (one of integrated, ChromatinAssay, SCT, RNA)." = !is.null(groups))

          return(groups)
        } else {
          return(NULL)
        }
      },
      arrays = function(){
        if(!is.null(self$output)){
          # Choose the most important group (in which the dimension should have been calculated)
          group <- self$groups[1]
          order <- c("scale_data", "data", "counts")
          arrays <- self$experiment$ms$get(group)$X$names() # counts data scale_data
          arrays <- order[which(order %in% arrays)]
          return(arrays)
        } else {
          return(NULL)
        }
      },
      markers = function(){
        if(!is.null(self$output)){
          markers <- self$experiment$get('markers')$names()
          return(markers)
        } else {
          return(NULL)
        }
      },
      comparison = function(){
        if(!is.null(self$output)){
          comparison <- self$experiment$get('comparison')$names()
          return(comparison)
        } else {
          return(NULL)
        }
      }
    ),
    private = list()
)

if (FALSE){
  source("R/fct_dataset.R")
  COMMON_DATA <- DATA$new()
  COMMON_DATA$output <- studies[1,"output"]
  COMMON_DATA$experiment
  COMMON_DATA$arrays
}


