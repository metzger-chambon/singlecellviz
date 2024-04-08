#' differential UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom plotly plotlyOutput
mod_differential_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    box(
      title = NULL,
      width = 12,
      fluidRow(
        column(
          width = 12,
          selectInput(ns("comparison"), "Select a comparison:", choices = NULL)

        )
      ),
      fluidRow(
        column(
          width = 6,
          numericInput(ns("top"), "Top genes to show in heatmap:", 15, min = 1, max = 1000),
          # TO DO: add tool tips change for a solution not using shinyBS
          # shinyBS::bsTooltip(ns("top"), paste("Only the top X genes are shown. Selection is based on p_val_adj",
          #                            "(and avg_log2FC in case of equality),",
          #                            "of each group in a selected annotation."),
          #           placement = "bottom")
          #
          # are shown on the heatmap
        ),
        column(
          width = 6,
          div(style = "margin-top: 28px;",
              #shinyjs::hidden(
              checkboxInput(ns("split"), "Split heatmap by conditions (if applicable)", value = FALSE)
              #)
          )
        )
      ),
      # fluidRow(
      #   column(
      #     width = 8,
      #     selectizeInput(ns("cell_label"), "Show marker features for:",
      #                    choices = NULL, multiple = TRUE,
      #                    options = list(
      #                      placeholder = "Select options",
      #                      plugins = list("remove_button")
      #                    ))
      #   ),
      #   column(
      #     width = 4,
      #     actionButton(inputId = ns("selectAll"), label = "Select All", style = 'margin-top:26px'),
      #
      #   )
      # ),
      plotlyOutput(ns("heatmap"), height = "600px"),
      DT::dataTableOutput(ns("comparison_table"))
    )
  )

}

#' differential Server Functions
#'
#' @noRd
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom DT dataTableProxy updateSearch

mod_differential_server <- function(id, COMMON_DATA, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    comparison_table <- reactive({
      req(input$comparison)
      comparison <- remove_suffix(input$comparison, isolate(r$selected_study))
      comparison_table <- COMMON_DATA$experiment$get('comparison')$get(comparison)$get("result")$read()$concat()$to_data_frame()
      comparison_table <- clean_result(comparison_table)
      return(comparison_table)
    }) %>% bindCache(c(input$comparison))#, cache = "session")

    aggrexpression_table <- reactive({
      req(input$comparison)
      comparison <- remove_suffix(input$comparison, isolate(r$selected_study))
      aggrexpression_table <- COMMON_DATA$experiment$get("comparison")$get(comparison)$get('aggrexpression')$read()$concat()$to_data_frame()
      aggrexpression_table <- clean_aggrexpression(aggrexpression_table)
      return(aggrexpression_table)
    }) %>% bindCache(c(input$comparison))#, cache = "session")

    # Update cell_annotation_choices with study specificity
    comparison_choices <- reactive(
      {
        req(r$selected_study)
        comparison_choices <- COMMON_DATA$comparison
        comparison_choices <- add_suffix(comparison_choices, isolate(r$selected_study))
        return(comparison_choices)
      })

    # Changes genes and cell annotations options in UI
    # TODO : is this necessary of is calling r$tabs() and comparison_choices (that depends on r$selected study) is enough?
    observeEvent(c(r$tabs(), r$selected_study), {
      # Recalculate annotation_choices and genes_choices everytime:
      #   the current tab is explore AND the selected study has changed
      # TODO : homogénéise differential et comparison names
      if (r$tabs() == "differential" &
          COMMON_DATA$tabs_updated['differential'] != r$selected_study){

        updateSelectInput(session,
                          inputId = "comparison",
                          choices = comparison_choices())
        COMMON_DATA$tabs_updated['differential'] <- r$selected_study
      }
    })

    # Update the tab with saved options in bookmark
    # (and sets COMMON_DATA$tabs_updated['differential'] to not update
    # a second time in the observer later on,
    # which would cause the updateSelectizeInput(selected = ) to be set to NULL)
    onRestore(function(state) {
      updateSelectizeInput(session,
                           inputId = "comparison",
                           selected = ifelse(is.null(state$input$comparison), 1, state$input$comparison),
                           choices = comparison_choices())
      COMMON_DATA$tabs_updated['differential'] <- r$selected_study
    })

    # Deal with DT table and Bookmark
    # https://stackoverflow.com/questions/41900515/shiny-dt-bookmarking-state
    # TODO remove also the plotly inputs
    # setBookmarkExclude(names = c("comparison_table_rows_all", "comparison_table_rows_selected",
    #                              "comparison_table_columns_selected", "comparison_table_cells_selected",
    #                              "comparison_table_rows_current", "comparison_table_state",
    #                              "comparison_table_cell_clicked"
    # ))

    comparison_table_proxy <- dataTableProxy("comparison_table")

    # Restore table selection and search
    # TODO: can I put that in onRestore? instead of onRestored?
    onRestored(function(state) {
      DT::updateSearch(comparison_table_proxy,
                       keywords = list(global = state$input$comparison_table_search,
                                       columns = state$input$comparison_table_search_columns))
    })

    output$comparison_table <- DT::renderDT({
      # freeze prevents the heatmap to update twice (first when study changes, then when rows_all changes)
      # https://github.com/rstudio/shiny/pull/3055
      freezeReactiveValue(input, "comparison_table_rows_all")
      data <- comparison_table()
      DT::datatable(data,
                    rownames = FALSE,
                    filter = list(position = 'top', clear = TRUE),
                    selection = 'none'#,
                    #options = list(autoWidth = TRUE, bAutoWidth = FALSE)
                    #options = list(columnDefs = list(list(width = '20px', targets = "_all")))
      ) %>%
        DT::formatSignif(1:5, digits=4)

    })

    # Heatmap where each row is a marker gene and each colon is a cluster
    # inpiration: https://www.ebi.ac.uk/gxa/sc/experiments/E-MTAB-9954/results/marker-genes?plotType=umap&plotOption=3&geneId=ENSMUSG00000032554&colourBy=cell+type
    # Heatmap table creation
    heatmap_table <- reactive({
      req(length(input$comparison_table_rows_all) > 0, input$top, comparison_table(), aggrexpression_table())
      #cat(length(input$markers_table_rows_all), '\n')
      comparison_table <- comparison_table()[input$comparison_table_rows_all,]
      comparison_table <- comparison_table %>%
        arrange(.data$p_val_adj, abs(.data$avg_log2FC)) %>%
        slice_head(n = input$top)

      #slice_max(order_by = p_val_adj, n = input$top, with_ties = FALSE)
      data <- aggrexpression_table() %>%
        filter(.data$gene %in% comparison_table$gene) %>%
        # order genes by the cluster they correspond to
        mutate(gene = factor(.data$gene, levels=unique(comparison_table$gene))) %>%
        pivot_longer(!.data$gene, names_to = 'group', values_to = "expression") %>%
        mutate(group = as.character(.data$group))

      # Split group column
      # Check if data can be split
      # TO DO: keep in mind in the db the fact that you can split or not
      aggregation_labels <- colnames(aggrexpression_table())[-1]
      # TRUE = you can split ; FALSE = you cannot split
      condition <- all(sapply(c(aggregation_labels), FUN= function(x){grepl("_", x)}))
      split <- input$split & condition
      if (condition){
        data <- data %>% separate(.data$group, c('group', 'group2'))
        if (!input$split) {
          data <- data %>%
            group_by(.data$gene, .data$group) %>%
            summarise(expression = mean(expression), .groups = "keep")
        }
      }

      # order genes by the cluster they correspond to
      #data$gene <- factor(data$gene, levels = unique(levels(markers$gene)))
      data <- data %>% mutate(text = paste0('Gene: ', .data$gene, '\n',
                                            'Expression: ', .data$expression, ''))
      # exemple of a marker for two groups :  CST7 (if you take top 9 gene in experiment pbmc3k markers gens of seurat clusters )
      # TODO: highlight selected row?
      #cat("plot: ", input$data_table_rows_selected, "\n")

      return(list(data = data, split = split))
    }) #%>% bindCache(c(input$comparison), cache = "session")


    #observe({
     output$heatmap <- renderPlotly({
        p <- HeatmapPlot(table = heatmap_table()$data, split = heatmap_table()$split)
        ggplotly(p, tooltip=c("text"))
      })
   # })



  })
}

## To be copied in the UI
# mod_differential_ui("differential_1")

## To be copied in the server
# mod_differential_server("differential_1")
