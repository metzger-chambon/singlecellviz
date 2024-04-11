#' dataset
#'
#' @description A fct function
#'
#' @return A dataframe containing informations about datasets.
#' @noRd

# Read current values in yaml config files
get_studies <- function(file_summary = "data/dataset_summary.txt"){
  studies <- read.table(file_summary, header = TRUE, sep = "\t")
  return(studies)
}
studies <- get_studies()
