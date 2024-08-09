# dataframe that holds usernames, passwords and other user data
# user_base <- tibble::tibble(
#   user = c("user1", "user2"),
#   password = c("pass1", "pass2"),
#   #password_hash = sapply(c("pass1", "pass2"), sodium::password_store),
#   permissions = c("admin", "standard"),
#   name = c("User One", "User Two")
# )

#' authr Server Functions
#'
#' @noRd
mod_authr_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # To create new passwords use sodium::password_store() and store it
    user_base <- read.table(get_golem_options("authr_file"),
                            header = TRUE, sep = "\t")

    start_up_modal <- function(ns) {
      modalDialog(easyClose = F,
                  shinyauthr::loginUI(id = ns("login")),
                  footer = NULL)
    }
    showModal(start_up_modal(ns))


    credentials <- shinyauthr::loginServer(
      id = "login",
      data = user_base,
      user_col = "user",
      pwd_col = "password_hash",
      sodium_hashed = TRUE,
      log_out = reactive(logout_init())
    )

    logout_init <- shinyauthr::logoutServer(id = "logout",
                                            active = reactive(credentials()$user_auth))


    user_info <- reactive({
      credentials()$info
    })

    observe({
      if (credentials()$user_auth) {
        removeModal()
        shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
        # TODO track user connections (use a db?)
        # cat(paste(user_info()$user, as.character(lubridate::now())))
      }
      else{
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      }
    })

  })
}

## To be copied in the UI
# mod_authr_ui("authr_1")

## To be copied in the server
# mod_authr_server("authr_1")
