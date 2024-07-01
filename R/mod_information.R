#' information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom tidyr %>%
mod_information_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = htmlOutput(ns("study_title")) %>% tagAppendAttributes(class = 'study-title'),
      tagList(
        htmlOutput(ns("study_description")) %>%
          tagAppendAttributes(class = 'study-description', style = 'min-height:100px; max-height:200px'),
      ),
      footer = span(htmlOutput(ns("study_date")) %>% tagAppendAttributes(class = 'study-date'),
                    htmlOutput(ns("study_doi")) %>% tagAppendAttributes(class = 'study-doi'))
    ),
    div(
      infoBoxOutput(ns("ncells")),
      infoBoxOutput(ns("nfeatures")),
      infoBoxOutput(ns("nsamples"))
    )
  )
}

#' information Server Functions
#'
#' @noRd
mod_information_server <- function(id, COMMON_DATA, r) {
  moduleServer(id, function(input, output, session) {
    studies <- get_golem_options("studies")

    output$study_title <- renderText({
      HTML(studies[r$selected_study,, drop = F]$title)
    })
    output$study_description <- renderText({
      HTML(studies[r$selected_study,, drop = F]$description)
    })
    output$study_date <- renderText({
      HTML(studies[r$selected_study,, drop = F]$date)
    })
    output$study_doi <-  renderText({
      doi <- studies[r$selected_study,, drop = F]$doi
      if (!is.null(doi) & doi != ""){
        url <- a(doi, href=paste0("https://doi.org/", doi), target="_blank", rel="noopener") #rel is for security reasons
        return(HTML(paste(HTML("<span style='font-weight:bold'>DOI: </span>"), url)))
      } else {
        return("")
      }
    })
    output$ncells <- renderInfoBox({
      ncells <- studies[r$selected_study,, drop = F]$ncells
      infoBox(
        "Cells",
        ncells,
        icon = icon("droplet"),
        color = "teal"
      )
    })
    output$nfeatures <- renderInfoBox({
      nfeatures <- studies[r$selected_study,, drop = F]$nfeatures
      infoBox(
        "Features",
        nfeatures,
        icon = icon("dna"),
        color = "teal"
      )
    })
    output$nsamples <- renderInfoBox({
      nsamples <- studies[r$selected_study,, drop = F]$nsamples
      infoBox(
        "Samples",
        nsamples,
        icon = icon("vial"),
        color = "teal"
      )
    })

  })
}

## To be copied in the UI
# mod_information_ui("information_1")

## To be copied in the server
# mod_information_server("information_1")
