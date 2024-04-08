testServer(
  mod_common_server,
  # Add here your module params
  args = list(r = reactiveValues(),
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

    session$setInputs(study = 1)

    # Test r object update
    expect_equal(r$selected_study, input$study)

    # Test COMMON_DATA update
    expect_equal(COMMON_DATA$output, studies[r$selected_study, "output"])
    expect_equal(COMMON_DATA$title, studies[r$selected_study, "title"])

})

test_that("module ui works", {
  ui <- mod_common_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_common_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

