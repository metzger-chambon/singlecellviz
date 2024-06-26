# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

mystudies <- read.table(file.path("../singlecelldatabase", "dataset_summary.txt"),
                      header = TRUE, sep = "\t")
mystudies$output <- file.path("../singlecelldatabase", mystudies$output)
mystudies$rds <- file.path("../singlecelldatabase", mystudies$rds)

singlecellviz::run_app(studies = mystudies,
                       cache_path = "./cache") # add parameters here (if any)
