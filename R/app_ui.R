#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom waiter autoWaiter
#' @noRd
#'
#'
app_ui <- function(request) {
  js <- HTML(paste("$(function() {",
                   "$('body').on('shown.bs.modal', function() {",
                   # TODO modify X months
                   "$('.modal-dialog .modal-body > span:first').text('This link stores the current state of this application. The validity of the link is not guaranteed for more than X months.')",
                   "})",
                   "})",
                   sep = "\n"))
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      skin = "black",
      dashboardHeader(title = "SingleCellViz",
                      mod_dropdownmenu_ui("message")
                      ),
      dashboardSidebar(
        sidebarMenu(
          id = "tabs",
          menuItem("Homepage", tabName = "homepage", icon = icon("house")), # must match the tabItem(tabName = "") in dashboardBody()
          mod_common_ui("common"),
          menuItem("General information", tabName = "information", icon = icon("list")),
          menuItem("Explore", tabName = "explore", icon = icon("magnifying-glass")),
          menuItem("Markers", tabName = "markers", icon = icon("tags")),
          menuItem("Differential expression", tabName = "differential", icon = icon("plus-minus")),

          tags$head(tags$script(js, type = "text/javascript")),
          mod_bookmark_ui("bookmark")

        )
        # https://stackoverflow.com/questions/68452272/how-to-prevent-user-from-doing-anything-on-shiny-app-when-app-is-busy
        # conditionalPanel(
        #   condition = "$(\'html\').hasClass(\'shiny-busy\')",
        #   tags$div(class = "loader"),
        #   tags$div(class = "prevent_click")
        # )
      ),
      dashboardBody(
        #shinyjs::useShinyjs(),
        waiter::autoWaiter(#id = c("explore-plotDim2"),
                           color = waiter::transparent(0),
                           html = waiter::spin_throbber()),
        #waiter::waiterShowOnLoad(waiter::spin_fading_circles()), # shows before anything else

        tabItems(
          tabItem(tabName = "homepage", # must match the menuItem(tabName = "") in dashboardSidebar()
                  h2("Welcome to the homepage!"),
                  mod_homepage_ui()
             ),
          tabItem(tabName = "information",
                  h2("General information about the study"),
                  mod_dataset_ui("dataset")


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
