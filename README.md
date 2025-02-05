# SingleCellViz

<!-- badges: start -->

<!-- badges: end -->

SingleCellViz is a RShiny app allowing an easy exploration of single cell datasets. It can be called with the function `singlecellviz::run_app()`.

## Installation

You can install the singlecellviz package via:

```r
install.packages("remotes")
remotes::install_github("metzger-chambon/singlecellviz")
```

## Running the app 

### Basic 

You can then run the app like so:

```r
singlecellviz::run_app()
```

By default, the app will be run on a very small subset on pbmc3k.

### With your data
 
If you have your own database of tiledb in the correct format for the app (see [Database vignette](articles/database.html) for more info), you can run the app like so:

```r
mystudies <- read.table("path/to/data_summary.txt",
                      header = TRUE,
                      sep = "\t")
singlecellviz::run_dev(studies = mystudies)
        
```

### With user authentication

To test the user authentication feature, run the following:

```r
singlecellviz::run_app(authr_file = system.file("extdata", "users.txt",
                                      package = "singlecellviz",
                                      mustWork = TRUE))
```

You can try with username `user1` and password `pass1`.

See [Database vignette](articles/deployment.html) to create your own user authentication.

