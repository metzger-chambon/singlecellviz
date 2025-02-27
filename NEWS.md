# SingleCellViz 1.1.3

**2025-02-05**

## Implement 
* Remove any annotation name starting with "soma_" (due to tiledbsoma 1.15.* changes for Assay5 support, for more information read paragraph "Writing v5 Assays" of `tiledbsoma::write_soma.Seurat()`)

# SingleCellViz 1.1.2

**2025-02-05**

## Implement 
* Allow for choices of tab(s) to show with the `tabs` parameter in `run_app()`

# SingleCellViz 1.1.1

**2024-08-13**

## Implement 
* Add username to `shiny.telemetry` log when using authentication with `shinyauthr`

# SingleCellViz 1.1.0

**2024-08-09**

## Implement 
* Basic user / password authorization with `shinyauthr`
* Track user activity with `shiny.telemetry`

## Other
* Modify UI of information tab and allow HTML text 
* `r$selected_study` is now the title of the dataset

## Debug
* Use `PseudobulkExpression` instead of `AggregateExpression` for heatmaps
* Fix `update_summary()` to only look in folders with a config.yaml file


# SingleCellViz 1.0.1 

**2024-06-28**

## Implement 
* cache_path is a parameter in `run_app()`

## Debug 
* Remove hard coded ordering of datasets in `update_summary()`
* Change parameter in `populate_tiledb()` from yaml to dataset_dir
* Better handle of cache keys

# SingleCellViz 1.0.0 

**2024-06-20**

* First public release
