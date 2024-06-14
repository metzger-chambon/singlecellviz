# Add the studies options usually passed to the app
# with run_app(studies = studies)
# so that it can be found when running tests

studies <- read.table(system.file("extdata",
                                  "dataset_summary.txt",
                                  package = "singlecellviz",
                                  mustWork = TRUE),
                      header = TRUE, sep = "\t")

shinyOptions("golem_options" = list("studies" = studies))
