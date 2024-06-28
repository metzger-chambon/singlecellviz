#' dataset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats setNames
mod_dataset_ui <- function(id){
  studies <- get_golem_options("studies")

  ns <- NS(id)
  tagList(
    selectInput(ns("study"), "Select a dataset:",
                choices = setNames(object = 1:nrow(studies), studies$title)
    )
  )
}

#' dataset Server Functions
#'
#' @noRd
mod_dataset_server <- function(id, COMMON_DATA, r){

  moduleServer( id, function(input, output, session){

    ns <- session$ns
    studies <- get_golem_options("studies")

    observeEvent(input$study, {
      study <- studies[input$study,, drop = F]
      COMMON_DATA$output <- study$output
      COMMON_DATA$title <- study$title

      # Set r$selected_study to be able to pass it to other modules
      r$selected_study <- input$study
      stopifnot("Study selected matches with more than one, or none, of the studies title" = length(r$selected_study) == 1)

      # Recalls that the homepage and information page have been update accordingly to the new study
      COMMON_DATA$tabs_updated['homepage'] <- r$selected_study
      COMMON_DATA$tabs_updated['information'] <- r$selected_study
    })
  })
}

## To be copied in the UI
# mod_dataset_ui("dataset_1")

## To be copied in the server
# mod_dataset_server("dataset_1")
