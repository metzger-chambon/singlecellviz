#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom utils packageVersion
#' @importFrom golem get_golem_options get_golem_version
#' @noRd
#'
#'
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      skin = "black",
      dashboardHeader(title = "SingleCellViz"),
      dashboardSidebar(
        sidebarMenu(
          id = "tabs",
          shiny.telemetry::use_telemetry(),
          if (get_golem_options("tabs")$homepage){
            menuItem("Homepage", tabName = "homepage", icon = icon("house")) # must match the tabItem(tabName = "") in dashboardBody()
          },
          mod_dataset_ui("dataset"),
          if (get_golem_options("tabs")$information){
            menuItem("General information", tabName = "information", icon = icon("list"))
          },
          if (get_golem_options("tabs")$explore){
            menuItem("Explore", tabName = "explore", icon = icon("magnifying-glass"))
          },
          if (get_golem_options("tabs")$markers){
            menuItem("Markers", tabName = "markers", icon = icon("tags"))
          },
          if (get_golem_options("tabs")$differential){
            menuItem("Differential expression", tabName = "differential", icon = icon("plus-minus"))
          },
          if (get_golem_options("tabs")$download){
            mod_download_ui("download")
          },

          div(
            style = "margin-top: auto; padding: 20px 14px 14px 14px; font-size: 12px;",
            # Separator line
            tags$hr(style = "border-top: 1px solid #666; margin-bottom: 10px;"),
            div(style="",
                a(icon("github"),
                  href = "https://github.com/metzger-chambon/singlecellviz",
                  target="_blank", rel="noopener"),
                paste0("SingleCellViz v.", packageVersion("singlecellviz"))),
            div(style="margin-top: 10px;",
                paste0("Provided by "),
                a(paste0("vgilbart \U1F33B"),
                  href = "https://github.com/vgilbart",
                  target="_blank", rel="noopener"),
            ),
            div(
              style = "margin-top: 10px; word-wrap: break-word; width: 100%; font-size: 10px;",
              HTML(paste0("Anonymous usage statistics are collected <br>",
                     "using ",
                          a(paste0("shiny.telemetry"),
                            href = "https://github.com/Appsilon/shiny.telemetry",
                            target="_blank", rel="noopener"),
                     " package. <br>",
                     "No personal or identifying data is collected.<br>",
                     "The data is used exclusively for internal analysis <br>",
                     "of app usage, and stored locally."
              )
            ))
        ))
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "homepage", # must match the menuItem(tabName = "") in dashboardSidebar()
                  h2("Homepage"),
                  mod_homepage_ui()
          ),
          tabItem(tabName = "information",
                  h2("General information about the dataset"),
                  mod_information_ui("information")
          ),
          tabItem(tabName = "explore",
                  h2("Explore the dataset"),
                  mod_explore_ui("explore")
          ),
          tabItem(tabName = "markers",
                  h2("Marker features"),
                  mod_markers_ui("markers")
          ),
          tabItem(tabName = "differential",
                  h2("Differentially expressed features"),
                  mod_differential_ui("differential")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SingleCellViz"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
