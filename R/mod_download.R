#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_ui <- function(id){
  ns <- NS(id)
  downloadButton(ns("downloadData"), "Download", style = "color: #333333 ; margin-left: 14px; margin-top: 5px")
}

#' download Server Functions
#'
#' @noRd

mod_download_server <- function(id, COMMON_DATA, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$downloadData <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0(gsub("[[:space:]|[:punct:]]", "_", studies[r$selected_study,, drop = F]$title), ".rds")
      },
      content = function(file) {
        # TODO: Understand why The first time it takes time then no
        showModal(modalDialog(tags$div(
          HTML('<i class="fas fa-spinner fa-spin"></i> Preparing file for downloading...'),
          style = "text-align: center;")
        , footer = NULL))
        file.copy(studies[r$selected_study,, drop = F]$rds, file)
        on.exit(removeModal())
      }
    )

  })
}

## To be copied in the UI
# mod_download_ui("download_1")

## To be copied in the server
# mod_download_server("download_1")
