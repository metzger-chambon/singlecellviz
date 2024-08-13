#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import shiny.telemetry
#' @importFrom golem get_golem_options
#' @noRd
#'

app_server <- function(input, output, session) {

  telemetry <- Telemetry$new(
    app_name = "singlecellviz",
    data_storage = DataStorageLogFile$new(file.path(get_golem_options("log_path"),"telemetry.txt"))
  )

  telemetry$start_session(track_values = TRUE,
                          track_inputs = FALSE)
  telemetry$log_input(c("dataset-study",
                        "markers-cell_annotation",
                        "differential-comparison"
                        ),
                      track_value = TRUE)


  # La stratégie du petit r et du grand R
  # https://engineering-shiny.org/structuring-project.html?q=strat%C3%A9#communication-between-modules
  COMMON_DATA <- DATA$new()

  r <- reactiveValues()
  r$tabs <- reactive(
    {input$tabs}
  )

  singlecellplot_theme()

  # https://github.com/rstudio/shiny-server/issues/480
  # cat(file=stderr(), paste0("\nStarting application: ", Sys.time(), "\n"))

  # A series of callModule() created with golem:add_module()

  if (!is.null(get_golem_options("authr_file"))){
    mod_authr_server("authr_file", telemetry)
  }

  mod_dataset_server("dataset", COMMON_DATA, r)

  mod_information_server("information", COMMON_DATA, r)

  mod_explore_server("explore", COMMON_DATA, r, telemetry)

  mod_markers_server("markers", COMMON_DATA, r)

  mod_differential_server("differential", COMMON_DATA, r)

  mod_download_server("download", COMMON_DATA, r, telemetry)

}
