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
        ),
        column(
          width = 6,
          div(style = "margin-top: 28px;",
              checkboxInput(ns("split"), "Split heatmap by conditions (if applicable)", value = FALSE)
          )
        )
      ),
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
      comparison_table <- clean_findmarkers_result(comparison_table)
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
    observeEvent(c(r$tabs(), r$selected_study), {
      # Recalculate annotation_choices and genes_choices everytime:
      #   the current tab is explore AND the selected study has changed
      if (r$tabs() == "differential" &
          COMMON_DATA$tabs_updated['differential'] != r$selected_study){

        updateSelectInput(session,
                          inputId = "comparison",
                          choices = comparison_choices())
        COMMON_DATA$tabs_updated['differential'] <- r$selected_study
      }
    })

    output$comparison_table <- DT::renderDT({
      # freeze prevents the heatmap to update twice (first when study changes, then when rows_all changes)
      # https://github.com/rstudio/shiny/pull/3055
      freezeReactiveValue(input, "comparison_table_rows_all")
      data <- comparison_table() %>%
        show_findmarkers_result()

    })

    # Heatmap where each row is a marker gene and each colon is a cluster
    # inpiration: https://www.ebi.ac.uk/gxa/sc/experiments/E-MTAB-9954/results/marker-genes?plotType=umap&plotOption=3&geneId=ENSMUSG00000032554&colourBy=cell+type
    # Heatmap table creation
    heatmap_table <- reactive({
      req(length(input$comparison_table_rows_all) > 0, input$top, comparison_table(), aggrexpression_table())
      # cat(length(input$markers_table_rows_all), '\n')
      comparison_table <- comparison_table()[input$comparison_table_rows_all,]
      comparison_table <- comparison_table %>%
        arrange(.data$p_val, -abs(.data$avg_log2FC)) %>%
        slice_head(n = input$top)

      data <- aggrexpression_table() %>%
        filter(.data$gene %in% comparison_table$gene) %>%
        # order genes by the cluster they correspond to
        mutate(gene = factor(.data$gene, levels = unique(comparison_table$gene))) %>%
        pivot_longer(!.data$gene, names_to = 'group', values_to = "expression") %>%
        mutate(group = as.character(.data$group))

      # Split group column
      # Check if data can be split
      aggregation_labels <- colnames(aggrexpression_table())[-1]
      # TRUE = you can split ; FALSE = you cannot split
      condition <- all(sapply(c(aggregation_labels), FUN= function(x){grepl("_", x)}))
      split <- input$split & condition
      if (condition){
        data <- data %>% separate(.data$group, c('group', 'group2'), sep = '_')%>%
          # To keep the same order of labels
          mutate(group = factor(group, levels = unique(group)),
                 group2 = factor(group2, levels = unique(group2)))
        if (!input$split) {
          data <- data %>%
            group_by(.data$gene, .data$group) %>%
            summarise(expression = mean(expression), .groups = "keep")
        }
      } else {
        data <- data %>% mutate(group = factor(group, levels = unique(group)))
      }

      # order genes by the cluster they correspond to
      data <- data %>% mutate(text = paste0('Gene: ', .data$gene, '\n',
                                            'Expression: ', .data$expression, ''))

      # exemple of a marker for two groups : CST7 (if you take top 9 gene in experiment pbmc3k markers gens of seurat clusters )

      return(list(data = data, split = split))
    })

    output$heatmap <- renderPlotly({
        p <- HeatmapPlot(table = heatmap_table()$data, split = heatmap_table()$split)
        ggplotly(p, tooltip=c("text"))
      })
  })
}

## To be copied in the UI
# mod_differential_ui("differential_1")

## To be copied in the server
# mod_differential_server("differential_1")
