testServer(
  mod_dropdownmenu_server,
  # Add here your module params
  args = list()
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

    # Test the creation of a dropdown menu
    expect_named(output$messageMenu, c("html", "deps"))
    expect_output(cat(output$messageMenu$html),
                  '<li class="dropdown notifications-menu">',
                  fixed = TRUE)

})

test_that("module ui works", {
  ui <- mod_dropdownmenu_ui(id = "test")
  golem::expect_shinytag(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_dropdownmenu_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

