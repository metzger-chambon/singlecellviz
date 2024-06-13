#' dataset
#'
#' @description A fct function
#'
#' @return A dataframe containing informations about datasets.
#' @importFrom utils read.table
#' @noRd

# Read current values in yaml config files
get_studies <- function(file_summary = system.file("extdata",
                                                   "dataset_summary.txt",
                                                   package = "singlecellviz")){
  studies <- read.table(file_summary, header = TRUE, sep = "\t")
  return(studies)
}
studies <- get_studies()
