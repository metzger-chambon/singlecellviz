#' markers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom plotly plotlyOutput

mod_markers_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    box(
      title = NULL,
      width = 12,
      fluidRow(
        column(
          width = 12,
          selectInput(ns("cell_annotation"), "Select an annotation:", choices = NULL)
        )
      ),
      fluidRow(
        column(
          width = 6,
          numericInput(ns("top"), "Top genes to show in heatmap:", 5, min = 1, max = 1000),
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
              checkboxInput(ns("split"), "Split heatmap by conditions (if applicable)", value = FALSE)
          )
        )
      ),
      plotlyOutput(ns("heatmap"), height = "600px"),
      DT::dataTableOutput(ns("markers_table"))
    )
  )
}

#' markers Server Functions
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom DT dataTableProxy updateSearch
#' @noRd
mod_markers_server <- function(id, COMMON_DATA, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    markers_table <- reactive({
      req(input$cell_annotation)
      cell_annotation <- remove_suffix(input$cell_annotation, isolate(r$selected_study))
      markers_table <- COMMON_DATA$experiment$get('markers')$get(cell_annotation)$get("result")$read()$concat()$to_data_frame()
      markers_table <- clean_findmarkers_result(markers_table) %>%
        # This filter is useful to define "what is a marker"
        filter(.data$p_val_adj < 0.05)
      # If a marker is marker of several clusters, collapse the information
      marker_of <- markers_table  %>%
        group_by(.data$gene) %>%
        summarize(groups = paste(.data$cluster, collapse = ', ')) %>%
        pull(groups, name = .data$gene)
      markers_table$marker_of <- marker_of[match(markers_table$gene, names(marker_of))]
      return(markers_table)
    }) %>% bindCache(c(input$cell_annotation, COMMON_DATA$title))#, cache = "session")

    aggrexpression_table <- reactive({
      req(input$cell_annotation)
      cell_annotation <- remove_suffix(input$cell_annotation, isolate(r$selected_study))
      aggrexpression_table <- COMMON_DATA$experiment$get("markers")$get(cell_annotation)$get('aggrexpression')$read()$concat()$to_data_frame()
      aggrexpression_table <- clean_aggrexpression(aggrexpression_table)
      return(aggrexpression_table)
    }) %>% bindCache(c(input$cell_annotation, COMMON_DATA$title))#, cache = "session")

    cell_annotation_choices <- reactive(
      {
        req(r$selected_study)
        cell_annotation_choices <- COMMON_DATA$markers
        cell_annotation_choices <- add_suffix(cell_annotation_choices, isolate(r$selected_study))
        return(cell_annotation_choices)
      })

    # Changes genes and cell annotations options in UI
    observeEvent(c(r$tabs(), r$selected_study), {
      # Recalculate annotation_choices and genes_choices everytime:
      #   the current tab is explore AND the selected study has changed
      if (r$tabs() == "markers" &
          COMMON_DATA$tabs_updated['markers'] != r$selected_study){
        updateSelectInput(session,
                          inputId = "cell_annotation",
                          choices = cell_annotation_choices())
        COMMON_DATA$tabs_updated['markers'] <- r$selected_study
      }
    })

    output$markers_table <- DT::renderDT({
      # freeze prevents the heatmap to update twice (first when study changes, then when rows_all changes)
      # https://github.com/rstudio/shiny/pull/3055
      freezeReactiveValue(input, "markers_table_rows_all")
      data <- markers_table() %>%
        show_findmarkers_result()

    })

    # Heatmap creation
    heatmap_table <- reactive({
      req(length(input$markers_table_rows_all) > 0, input$top, markers_table(), aggrexpression_table())
      # cat(length(input$markers_table_rows_all), '\n')
      markers_table <- markers_table()[input$markers_table_rows_all,]
      markers <- markers_table %>% group_by(.data$cluster) %>%
        arrange(.data$p_val, -abs(.data$avg_log2FC)) %>%
        slice_head(n = input$top) %>%
        select(.data$gene, .data$cluster, .data$marker_of)

      data <- aggrexpression_table() %>%
        filter(.data$gene %in% markers$gene) %>%
        # order genes by the cluster they correspond to
        mutate(gene = factor(.data$gene, levels=unique(markers$gene))) %>%
        pivot_longer(!.data$gene, names_to = 'group', values_to = "expression") %>%
        mutate(group = as.character(.data$group))

      data$marker_of <- markers$marker_of[match(data$gene, names(markers$marker_of))]


      # Split group column
      # Check if data can be split
      cluster_labels <- unique(markers$cluster)
      aggregation_labels <- colnames(aggrexpression_table())[-1]
      # TRUE = you can split ; FALSE = you cannot split
      condition <- !all(cluster_labels %in% aggregation_labels)
      split <- input$split & condition
      if (condition){
        data <- data %>% separate(.data$group, c('group', 'group2'), sep = '_') %>%
          # To keep the same order of labels
          mutate(group = factor(.data$group, levels = unique(.data$group)),
                 group2 = factor(.data$group2, levels = unique(.data$group2)))
        if (!input$split) {
          data <- data %>%
            group_by(.data$gene, .data$group, .data$marker_of) %>%
            summarise(expression = mean(.data$expression), .groups = "keep")
        }
       } else {
        data <- data %>% mutate(group = factor(.data$group, levels = unique(.data$group)))
      }

      # order genes by the cluster they correspond to
      # data$gene <- factor(data$gene, levels = unique(levels(markers$gene)))
      data <- data %>% mutate(text = paste0('Gene: ', .data$gene, '\n',
                                            'Expression: ', .data$expression, '\n',
                                            'Marker of group: ', .data$marker_of, '' ),
                              group = factor(.data$group, levels = levels(markers$cluster)))
      # exemple of a marker for two groups :  CST7 (if you take top 9 gene in experiment pbmc3k markers gens of seurat clusters )
      return(list(data = data, split = split))
    })

    #observe({
    output$heatmap <- renderPlotly({
      p <- HeatmapPlot(table = heatmap_table()$data, split = heatmap_table()$split)
      ggplotly(p, tooltip=c("text"))
    })
  })
}

## To be copied in the UI
# mod_markers_ui("markers_1")

## To be copied in the server
# mod_markers_server("markers_1")
