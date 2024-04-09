#' dataset
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @noRd

# TODO deal with studies summary better
# Maybe a good way to deal with that would be to have a package for singlecelldatabase
# That has a function that output "studies"
# And then this can be easily used both in the UI (for the notification and choice of studies)
# and the Server part (to get the description, date, doi etc...)

# Get all choices of studies
# file_summary = "data-raw/dataset_summary.txt"
# studies <- read.table(file_summary, header = TRUE, sep = "\t")
studies <- structure(list(title = c("Massively parallel digital transcriptional profiling of single cells",
                                    "HIF.SCT"), output = c("experiment-pbmc3k_final", "HIF.SCT"),
                          seurat_file = c("data/db/seurat/pbmc3k_final.rds", "data/db/seurat/HIF.SCT.rds"
                          ), description = c("Dataset of Peripheral Blood Mononuclear Cells (PBMC) freely available from 10X Genomics.",
                                             "Hypoxia-mediated stabilization of HIF1A in prostatic intraepithelial neoplasia promotes cell plasticity and malignant progression."
                          ), doi = c("10.1038/ncomms14049", "10.1126/sciadv.abo2295"
                          ), date = c("16/01/2017", "22/07/2022"), nsamples = 1:2,
                          ncells = c(2638L, 15119L), nfeatures = c(13714L, 17151L)), class = "data.frame", row.names = c(NA,
                                                                                                                         -2L))

studies$output <- file.path("data/db/tiledb", studies$output)
date_threshold <- as.Date('01/01/2020', tryFormats = c("%d/%m/%Y"))
studies$new <- ifelse(as.Date(studies$date, tryFormats = c("%d/%m/%Y")) > date_threshold, TRUE, FALSE)
