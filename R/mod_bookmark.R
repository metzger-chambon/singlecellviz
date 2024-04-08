#' bookmark UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bookmark_ui <- function(id){
  ns <- NS(id)
  bookmarkButton(label = "Create sharing link", id = ns("bookmark1"))
}

#' bookmark Server Functions
#'
#' @noRd
mod_bookmark_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

