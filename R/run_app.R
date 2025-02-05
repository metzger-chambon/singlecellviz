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
#' @param cache_path a path to the cache directory to use.
#' If the folder already exists, make sure that the use has read and write access.
#' @param log_path a path to the log directory, where a \code{telemetry.txt} file will
#' be created. If the folder or file already exists, make sure that the use has read and write access.
#' @param authr_file a path to the authr .txt file, where the columns
#' @param tabs a list with names being tabs amongst [''] and values being boolen (TRUE to show the tab, FALSE otherwise). By default all tabs are shown
#' \code{user}, \code{password_hash}, and \code{permissions} are available. If the folder or file already exists, make sure that the use has read and write access.
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
  cache_path = NULL,
  log_path = "log",
  authr_file = NULL,
  tabs = list(),
  ...
) {

  dir.create(log_path, showWarnings = FALSE)

  if (is.null(cache_path)){
    cache_path <- file.path(tempdir(), "singlecellviz-cache/")
  }

  tabs_default = list(homepage = TRUE,
                      information = TRUE,
                      explore = TRUE,
                      markers = TRUE,
                      differential = TRUE,
                      download = TRUE)
  tabs = modifyList(tabs_default, tabs)


  shinyOptions(cache = cachem::cache_disk(cache_path))
  # shinyOptions(cache = cachem::cache_mem(max_size = 512 * 1024^2)) # 512 megabytes

  if(is.null(studies)){
    studies <- read.table(system.file("extdata",
                                      "dataset_summary.txt",
                                      package = "singlecellviz",
                                      mustWork = TRUE),
                          header = TRUE, sep = "\t")
    studies$output <- file.path(system.file("extdata", package = "singlecellviz"),
                                studies$output)
    studies$rds <- file.path(system.file("extdata", package = "singlecellviz"),
                             studies$rds)
  }

  #rownames(studies) <- studies$title

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
      log_path = log_path,
      authr_file = authr_file,
      tabs = tabs,
      ...)
  )
}
