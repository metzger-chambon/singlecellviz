#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom waiter autoWaiter
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
          menuItem("Homepage", tabName = "homepage", icon = icon("house")), # must match the tabItem(tabName = "") in dashboardBody()
          mod_dataset_ui("dataset"),
          menuItem("General information", tabName = "information", icon = icon("list")),
          menuItem("Explore", tabName = "explore", icon = icon("magnifying-glass")),
          menuItem("Markers", tabName = "markers", icon = icon("tags")),
          menuItem("Differential expression", tabName = "differential", icon = icon("plus-minus")),
          mod_download_ui("download"),

          div(style="margin-left: 14px; margin-top: 14px;",
              a(icon("github"),
                href = "https://github.com/metzger-chambon/singlecellviz",
                target="_blank", rel="noopener"),
              paste0("SingleCellViz v.", packageVersion("singlecellviz")))
        )
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
