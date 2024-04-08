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
      markers_table <- clean_result(markers_table)
      return(markers_table)
    }) %>% bindCache(c(input$cell_annotation))#, cache = "session")

    aggrexpression_table <- reactive({
      req(input$cell_annotation)
      cell_annotation <- remove_suffix(input$cell_annotation, isolate(r$selected_study))
      aggrexpression_table <- COMMON_DATA$experiment$get("markers")$get(cell_annotation)$get('aggrexpression')$read()$concat()$to_data_frame()
      aggrexpression_table <- clean_aggrexpression(aggrexpression_table)
      return(aggrexpression_table)
    }) %>% bindCache(c(input$cell_annotation))#, cache = "session")

    cell_annotation_choices <- reactive(
      {
        req(r$selected_study)
        cell_annotation_choices <- COMMON_DATA$markers
        cell_annotation_choices <- add_suffix(cell_annotation_choices, isolate(r$selected_study))
        return(cell_annotation_choices)
      })

    # Update the tab with saved options in bookmark
    # (and sets COMMON_DATA$tabs_updated['markers'] to not update
    # a second time in the observer later on,
    # which would cause the updateSelectizeInput(selected = ) to be set to NULL)
    onRestore(function(state) {
      updateSelectizeInput(session,
                           inputId = "cell_annotation",
                           selected = ifelse(is.null(state$input$cell_annotation), 1, state$input$cell_annotation),
                           choices = cell_annotation_choices())
      COMMON_DATA$tabs_updated['markers'] <- r$selected_study
    })

    # Deal with DT table and Bookmark
    # https://stackoverflow.com/questions/41900515/shiny-dt-bookmarking-state
    # setBookmarkExclude(names = c("markers_table_rows_all", "markers_table_rows_selected",
    #                              "markers_table_columns_selected", "markers_table_cells_selected",
    #                              "markers_table_rows_current", "markers_table_state",
    #                              "markers_table_cell_clicked"
    #                              ))

    markers_table_proxy <- dataTableProxy("markers_table")

    # Restore table selection and search
    # TODO: can I put that in onRestore? instead of onRestored?
    onRestored(function(state) {
      DT::updateSearch(markers_table_proxy,
                       keywords = list(global = state$input$markers_table_search,
                                       columns = state$input$markers_table_search_columns))
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
      data <- markers_table()
      DT::datatable(data,
                    rownames = FALSE,
                    filter = list(position = 'top', clear = TRUE),
                    selection = 'none'#,
                    #options = list(autoWidth = TRUE, bAutoWidth = FALSE)
                    #options = list(columnDefs = list(list(width = '20px', targets = "_all")))
      ) %>%
        DT::formatSignif(1:5, digits=4)

    })

    # Heatmap creation
    # TODO not make it a genereal observe but at least give some values to check otherwise it comes here
    # even when nothing in this tab is happening
    heatmap_table <- reactive({
      req(length(input$markers_table_rows_all) > 0, input$top, markers_table(), aggrexpression_table())
      #cat(length(input$markers_table_rows_all), '\n')
      markers_table <- markers_table()[input$markers_table_rows_all,]
      markers <- markers_table %>% group_by(.data$cluster) %>%
        arrange(.data$p_val_adj, .data$avg_log2FC) %>%
        slice_head(n = input$top) %>%
        select(.data$gene, .data$cluster)
      # If a marker is marker of several clusters, collapse the information
      marker_of <- markers %>% group_by(.data$gene) %>%
        summarize(groups = paste(.data$cluster, collapse = ', ')) %>%
        pull(groups, name = .data$gene)

      data <- aggrexpression_table() %>%
        filter(.data$gene %in% markers$gene) %>%
        # order genes by the cluster they correspond to
        mutate(gene = factor(.data$gene, levels=unique(markers$gene))) %>%
        pivot_longer(!.data$gene, names_to = 'group', values_to = "expression") %>%
        mutate(group = as.character(.data$group))
      # TO DO find a better way to do this ?
      data$marker_of <- marker_of[match(data$gene, names(marker_of))]

      # Split group column
      # Check if data can be split
      cluster_labels <- unique(markers$cluster)
      aggregation_labels <- colnames(aggrexpression_table())[-1]
      # TRUE = you can split ; FALSE = you cannot split
      condition <- !all(cluster_labels %in% aggregation_labels)
      split <- input$split & condition
      if (condition){
        data <- data %>% separate(.data$group, c('group', 'group2'))
        if (!input$split) {
          data <- data %>%
            group_by(.data$gene, .data$group, marker_of) %>%
            summarise(expression = mean(expression), .groups = "keep")
        }
      }

      # order genes by the cluster they correspond to
      #data$gene <- factor(data$gene, levels = unique(levels(markers$gene)))
      data <- data %>% mutate(text = paste0('Gene: ', .data$gene, '\n',
                                            'Expression: ', expression, '\n',
                                            'Marker of group: ', marker_of, '' ))
      # exemple of a marker for two groups :  CST7 (if you take top 9 gene in experiment pbmc3k markers gens of seurat clusters )
      # TODO: highlight selected row?
      return(list(data = data, split = split))
    }) #%>% bindCache(c(input$comparison), cache = "session")


    #observe({
    output$heatmap <- renderPlotly({
      p <- HeatmapPlot(table = heatmap_table()$data, split = heatmap_table()$split)
      ggplotly(p, tooltip=c("text"))
    })

    # Heatmap where each row is a marker gene and each colon is a cluster
    # inpiration: https://www.ebi.ac.uk/gxa/sc/experiments/E-MTAB-9954/results/marker-genes?plotType=umap&plotOption=3&geneId=ENSMUSG00000032554&colourBy=cell+type
    # output$heatmap <- renderPlotly({
    #   random_ggplotly(type = "bin2d")
    # })


    #
    #     observeEvent(input$selectAll, {
    #       updateSelectizeInput(
    #         session,
    #         inputId = "cell_label",
    #         selected = r$cell_label_choices()
    #       )
    #     })


  })
}

## To be copied in the UI
# mod_markers_ui("markers_1")

## To be copied in the server
# mod_markers_server("markers_1")
