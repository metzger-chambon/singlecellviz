library(yaml)
library(tiledbsoma)
#library(tiledb)

# Read current values in yaml config files
get_studies <- function(){
  studies <- list()
  dir = "data/db/"
  files <- list.files(path = dir, pattern = "config.yaml", recursive = TRUE, full.names = TRUE)
  for (file in files){
    x <- read_yaml(file, eval.expr=TRUE)
    uri <- file.path(x$output_folder)
    experiment <- SOMAOpen(uri)
    query <- SOMAExperimentAxisQuery$new(
      experiment = experiment,
      # TODO deal with other than RNA
      measurement_name = "RNA"
    )
    x$ncells <- query$n_obs
    x$nfeatures <- query$n_vars
    x$nsamples <- nrow(unique(experiment$obs$read(column_names = c("orig.ident"))$concat()$to_data_frame()))

    studies[x$title] <- list(x)
  }

  studies <- data.frame(title = sapply(studies, '[[', "title"),
                        output = sapply(studies, '[[', "output_folder"),
                        description = sapply(studies, '[[', "description"),
                        doi = sapply(studies, '[[', "doi"),
                        date = sapply(studies, '[[', "date"),
                        output = sapply(studies, '[[', "output_folder"),
                        nsamples = sapply(studies, '[[', "nsamples"),
                        nfeatures = sapply(studies, '[[', "nfeatures"),
                        ncells = sapply(studies, '[[', "ncells"))


  #studies$output <- file.path("data/", studies$output)
  date_threshold <- as.Date('01/01/2020', tryFormats = c("%d/%m/%Y"))
  studies$new <- ifelse(as.Date(studies$date, tryFormats = c("%d/%m/%Y")) > date_threshold, TRUE, FALSE)

  rownames(studies) <- NULL
  return(studies)
}

studies <- get_studies()

file_summary = "data/dataset_summary.txt"

write.table(studies, file = file_summary, sep = '\t', quote = FALSE,
            row.names = FALSE, col.names = TRUE)

