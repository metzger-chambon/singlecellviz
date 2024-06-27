
# SingleCellViz

SingleCellViz is a RShiny app allowing an easy exploration of single cell datasets. It can be called with the function `singlecellviz::run_app()`.

## Installation

You can install the singlecellviz package via:

```r
remotes::install_github("metzger-chambon/singlecellviz")
```

## Running the app 

You can then run the app like so:

```r
singlecellviz::run_app()
```

By default, the app will be run on a very small subset on pbmc3k.

If have your own database of tiledb in the correct format for the app (see [Database vignette](articles/database.html) for more info), you can run the app like so:

```r
singlecellviz::run_app(studies = "path/to/data_summary.txt")
```

