#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @param studies a file path to a summary of all available studies.
#' The file should be a tab separated table, with the headers:
#' \code{title  output  rds  description  doi  date  nsamples  nfeatures  ncells}.
#' By default the file read contains information about a very small subset
#' of pbmc3k present in the package. Check out
#' \code{vignette("database", package = "singlecellviz")} for more information.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom cachem cache_disk
#' @importFrom golem with_golem_options
#' @importFrom utils read.table
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = "url",
  uiPattern = "/",
  studies = NULL,
  ...
) {
  # TODO make cache optional
  shinyOptions(cache = cachem::cache_disk("./cache"))
  # shinyOptions(cache = cachem::cache_mem(max_size = 512 * 1024^2)) # 512 megabytes

  if(is.null(studies)){
    studies <- read.table(system.file("extdata",
                                      "dataset_summary.txt",
                                      package = "singlecellviz",
                                      mustWork = TRUE),
                          header = TRUE, sep = "\t")
    studies$output <- file.path(system.file(package = "singlecellviz"), studies$output)
    studies$rds <- file.path(system.file(package = "singlecellviz"), studies$rds)
  }

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      studies = studies,
      ...)
  )
}
