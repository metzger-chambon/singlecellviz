#' @importFrom R6 R6Class
#' @import tiledbsoma

DATA <- R6::R6Class(
    "DATA",
    public = list(
      output = NULL,
      initialize = function(){
        # Some initialization if needed
      },
      tabs_updated = c("homepage" = FALSE,
                       "information" = FALSE,
                       "explore" = FALSE,
                       "markers" = FALSE,
                       "differential" = FALSE)
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
      groups = function(){
        if(!is.null(self$output)){
          groups <- self$experiment$ms$names() # e.g. RNA, SCT, integrated
          return(groups)
        } else {
          return(NULL)
        }
      },
      arrays = function(){
        if(!is.null(self$output)){
          # Choose the most important group (in which the dimension should have been calculated)
          group <- self$groups[1]
          arrays <- self$experiment$ms$get(group)$X$names() # e.g. counts data scale_data
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

