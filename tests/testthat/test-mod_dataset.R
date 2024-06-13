testServer(
  mod_dataset_server,
  # Add here your module params
  args = list(r = reactiveValues(selected_study = 1),
              COMMON_DATA = DATA$new())
  , {
    # Test provided by golem
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )

})

test_that("module ui works", {
  ui <- mod_dataset_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_dataset_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

