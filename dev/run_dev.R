run_dev <- function(port = NULL, ...) {
  # Sass code compilation
  sass::sass(
    input = sass::sass_file("inst/app/www/custom.sass"),
    output = "inst/app/www/custom.css",
    cache = NULL
  )

  # Set options here
  options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

  # Comment this if you don't want the app to be served on a random port
  if (is.null(port)) {
    port = httpuv::randomPort()
  }
  options(shiny.port = port)

  # Detach all loaded packages and clean your environment
  golem::detach_all_attached()
  # rm(list=ls(all.names = TRUE))

  # Document and reload your package
  golem::document_and_reload()

  run_app(...)
}

port = 9560

mystudy <- read.table("../singlecelldatabase/dataset_summary.txt",
                      header = TRUE,
                      sep = "\t")
mystudy$output <- file.path("../singlecelldatabase", mystudy$output)
mystudy$rds <- file.path("../singlecelldatabase", mystudy$rds)
# mystudy <- NULL # to test on the example dataset

run_dev(port,
        studies = mystudy,
        cache_path = "./cache",
        authr_file = "inst/extdata/users.txt")
