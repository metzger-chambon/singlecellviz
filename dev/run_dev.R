run_dev <- function(port = NULL){
  # Sass code compilation
  sass::sass(input = sass::sass_file("inst/app/www/custom.sass"), output = "inst/app/www/custom.css", cache = NULL)

  # Set options here
  options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

  # Comment this if you don't want the app to be served on a random port
  if (is.null(port)){port = httpuv::randomPort()}
  options(shiny.port = port)

  # Detach all loaded packages and clean your environment
  golem::detach_all_attached()
  # rm(list=ls(all.names = TRUE))

  # Document and reload your package
  golem::document_and_reload()

  # Run the application
  run_app()
}

port = 9560
run_dev(port)

source('R/fct_dataset.R')
source('R/R6.R')

