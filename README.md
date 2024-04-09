
<!-- README.md is generated from README.Rmd. Please edit README.Rmd and run devtools::build_readme() to update README.md -->

# singlecellviz

Singlecellviz is a RShiny app allowing an easy exploration of single
cell datasets.

## Installation

You can install the development version of singlecellviz like so:

``` bash
git clone git@github.com:metzger-chambon/si nglecellviz.git
```

You will need to install the required packages and their dependencies.
They are listed in the renv.lock file.

``` r
renv::init() # Answer 3: Activate the project without snapshotting or installing any packages.
renv::restore()
```

You can also find the necessary and suggested packages in the
DESCRIPTION file.
