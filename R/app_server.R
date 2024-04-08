#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @noRd
#'
#

# TODO: set maximum cache
app_server <- function(input, output, session) {
  # La stratégie du petit r et du grand R
  # https://engineering-shiny.org/structuring-project.html?q=strat%C3%A9#communication-between-modules
  COMMON_DATA <- DATA$new()
  r <- reactiveValues()
  r$tabs <- reactive(
    {input$tabs}
  )

  #https://github.com/rstudio/shiny-server/issues/480
  # cat(file=stderr(), paste0("\nStarting application: ", Sys.time(), "\n"))
  # cat(file=stderr(), paste0("\nRandom message\n"))
  # cat(file=stderr(), paste0("\nTemp dir: ", tempdir(), "\n"))
  # cat(file=stderr(), paste0("\nSys TMPDIR dir: ", Sys.getenv("TMPDIR"), "\n"))
  # cat(file=stderr(), paste0("\nSys TMP dir: ", Sys.getenv("TMP"), "\n"))
  # cat(file=stderr(), paste0("\nSys TEMP dir: ", Sys.getenv("TEMP"), "\n"))
  # cat(file=stderr(), paste0("\nWorking dir: ", getwd(), "\n"))
  
  # A series of callModule() created with golem:add_module()
  mod_dropdownmenu_server("message")

  mod_common_server("common", COMMON_DATA, r)

  mod_dataset_server("dataset", COMMON_DATA, r)

  mod_explore_server("explore", COMMON_DATA, r)

  mod_markers_server("markers", COMMON_DATA, r)

  mod_differential_server("differential", COMMON_DATA, r)

  # Cannot be put into a module
  # without creating a reactive value for names(input),
  # or passing the input to the function
  # observeEvent(input[['bookmark-bookmark1']], {
  #   names <- names(input)
  #   whitelist <- c("tabs", "common-study",
  #                  "explore-cell_annotation", "explore-gene_annotation", "explore-dimtype",
  #                  "markers-cell_annotation", "markers-split", "markers-top",
  #                  "differential-comparison", "differential-split", "differential-top",
  #                  "markers-markers_table_search", "markers-markers_table_search_columns",
  #                  "differential-comparison_table_search", "differential-comparison_table_search_columns")
  #   #cat(setdiff(names, whitelist), '\n')
  #   setBookmarkExclude(setdiff(names, whitelist))
  #   session$doBookmark()
  # })

  #session$onSessionEnded(function() { stopApp() })

}
