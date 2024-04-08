#' dropdownmenu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_dropdownmenu_ui <- function(id){
  ns <- NS(id)
  shinydashboard::dropdownMenuOutput(ns("messageMenu"))
}

#' dropdownmenu Server Functions
#'
#' @noRd
mod_dropdownmenu_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$messageMenu <- renderMenu({
      # Code to generate each of the messageItems here, in a list.
      msgs <- apply(studies[which(studies$new),], 1, function(row) {
        if(row[["new"]] == TRUE){
          shinydashboard::messageItem(from = "News",
                                      message = paste(row[["title"]], 'is now available!'),
                                      icon = icon(NULL),
                                      # WARNING:
                                      # https://github.com/rstudio/shinydashboard/issues/373
                                      # The `name` provided ('clock-o') does not correspond to a known icon
                                      # time = row[['time']]
                                      )
        }
      })

      # This is equivalent to calling:
      #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
      dropdownMenu(type = "notifications", .list = msgs,
                   badgeStatus = "warning", icon = icon("clipboard"))
    })

  })
}

## To be copied in the UI
# mod_dropdownmenu_ui("dropdownmenu_1")

## To be copied in the server
# mod_dropdownmenu_server("dropdownmenu_1")
