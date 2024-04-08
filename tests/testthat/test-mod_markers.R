testServer(
  mod_markers_server,
  # Add here your module params
  args = list(reactiveValues(selected_study = 1),
              COMMON_DATA = DATA$new())
  , {
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
    # Cannot test much as long as COMMON_DATA cannot be tested with a real SOMAExperiment

})

test_that("module ui works", {
  ui <- mod_markers_ui(id = "test")
  golem::expect_shinytag(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_markers_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

