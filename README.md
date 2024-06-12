
# SingleCellViz

SingleCellViz is a RShiny app allowing an easy exploration of single cell datasets. 

## Installation

You can install the development version of SingleCellViz like so:

```bash
git clone git@github.com:metzger-chambon/singlecellviz.git
```

You should also get the same version of R (written at the very top of `renv.lock` file`), as this is not handled by renv. 

You will need to install the required packages and their dependencies. This can easily be done using renv R package, by running the following:

```r
renv::init() 
# Select Answer 3: Activate the project without snapshotting or installing any packages.

renv::restore()
```

The packages and their versions are listed in the renv.lock file. 
You can also find necessary and suggested packages in the DESCRIPTION file. In particular, suggested packages are useful when developing the app.

## Usage, developement and deployment

For more information, check the [documentation](https://metzger-chambon.igbmc.science/SingleCellViz-doc/) of the app. 

