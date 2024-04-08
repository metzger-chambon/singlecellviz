#' common UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats setNames
mod_common_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("study"), "Select a study:",
                choices = setNames(object = 1:nrow(studies), studies$title)
    )
  )
}

#' common Server Functions
#'
#' @noRd
mod_common_server <- function(id, COMMON_DATA, r){
  moduleServer( id, function(input, output, session){
    #ns <- session$ns

    # TO DO: understand why this is needed but not updating the homepage ?
    # Is it just bc other modules depend on COMMON_DATA and r$selected_study?
    onRestore(function(state) {
      r$selected_study <- state$input$study
      study <- studies[input$study,, drop = F]
      COMMON_DATA$output <- study$output
      COMMON_DATA$title <- study$title
    })

    observeEvent(input$study, {
      study <- studies[input$study,, drop = F]
      COMMON_DATA$output <- study$output
      COMMON_DATA$title <- study$title
      # COMMON_DATA$description <- study$description
      # COMMON_DATA$date <- study$date
      # COMMON_DATA$doi <- study$doi
      # COMMON_DATA$ncells <- study$ncells
      # COMMON_DATA$nfeatures <- study$nfeatures
      # COMMON_DATA$nsamples <- study$nsamples

      # set r$selected_study to be able to pass it to other modules
      r$selected_study <- input$study
      stopifnot("Study selected matches with more than one, or none, of the studies title" = length(r$selected_study) == 1)

      # Recalls that the homepage and information page have been update accordingly to the new study
      COMMON_DATA$tabs_updated['homepage'] <- r$selected_study
      COMMON_DATA$tabs_updated['information'] <- r$selected_study
    })
  })
}

## To be copied in the UI
# mod_common_ui("common_1")

## To be copied in the server
# mod_common_server("common_1")
