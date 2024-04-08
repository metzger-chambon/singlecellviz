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

  # A series of callModule() created with golem:add_module()
  mod_dropdownmenu_server("message")

  mod_common_server("common", COMMON_DATA, r)

  mod_dataset_server("dataset", COMMON_DATA, r)

  mod_explore_server("explore", COMMON_DATA, r)

  mod_markers_server("markers", COMMON_DATA, r)

  mod_differential_server("differential", COMMON_DATA, r)

  mod_bookmark_server("bookmark")


}
