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

    # Test the selection of study characteristics
    expect_equal(output$study_title, studies[r$selected_study,"title"])
    expect_equal(output$study_description, studies[r$selected_study,"description"])
    expect_equal(output$study_date, studies[r$selected_study,"date"])
    expect_true(grepl(sprintf('<a href=\"https://doi.org/%1$s\" target=\"_blank\" rel=\"noopener\">%1$s</a>', studies[r$selected_study,"doi"]), output$study_doi))

    expect_infoBox <- function(output, text = NULL, number = NULL){
      expect_named(output, c("html", "deps"))

      expect_output(cat(output$html),
                    '<div class="info-box">',
                    fixed = TRUE)

      if (!is.null(text)){
        expect_output(cat(output$html),
                      sprintf('<span class="info-box-text">%s</span>', text),
                      fixed = TRUE)
      }
      if (!is.null(number)){
        expect_output(cat(output$html),
                      sprintf('<span class="info-box-number">%s</span>', number),
                      fixed = TRUE)
      }

    }

    expect_infoBox(output$ncells, number = studies[r$selected_study,"ncells"])
    expect_infoBox(output$nfeatures, number = studies[r$selected_study,"nfeatures"])
    expect_infoBox(output$nsamples, number = studies[r$selected_study,"nsamples"])

})

test_that("module ui works", {
  ui <- mod_dataset_ui(id = "test")
  golem::expect_shinytag(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_dataset_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

