#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd

mod_explore_ui <- function(id){
  ns <- NS(id)
  div(
    fluidRow(
      column(
        width = 4,
        selectInput(ns("cell_annotation"), "Select an annotation:", choices = NULL)
      ),
      column(
        width = 5,
        selectizeInput(ns("gene_annotation"), "Select genes:", multiple = TRUE,
                       choices =  NULL, options = list(maxItems = 40,
                                                       placeholder = "Type gene name"
                       ))
        #options = list(maxItems = 12))
      ),
      column(
        width = 3,
        actionButton(ns("validate_gene_annotation"), "Validate selection",
                     style = 'margin-top:26px')
        #options = list(maxItems = 12))
      ),

      tabBox(
        title = "Plots",
        width = 12, # The overall width of a region is 12
        tabPanel(
          "DimPlot",
          selectInput(ns("dimtype"), "Select a plot type:", choices = NULL),
          fluidRow(
            column(
              width = 6,
              plotOutput(ns("plotDim1"))
            ),
            column(
              width = 6,
              plotOutput(ns("plotDim2"))
            )
          )
        ),
        tabPanel(
          "VlnPlot",
          selectInput(ns("split_violin"), "Select an annotation to split by:", choices = NULL),
          # x = annotations ; y = genes expression (if several, uses all genes to compute?)
          # inspo : singlecell portal
          plotOutput(ns("violin"))
        ),
        tabPanel(
          # Typical dotplot for one or more features
          # inspo : singlecell portal
          "DotPlot",
          plotOutput(ns("dotplot"))
        )
      )
    )
  )
}


#' explore Server Functions
#' @noRd
mod_explore_server <- function(id, COMMON_DATA, r, telemetry){
  moduleServer(id, function(input, output, session){

    # Calculate genes_choices options
    genes_choices <- reactive(
      {
        req(r$selected_study)
        # cat("genes: ", print(r$selected_study, collapse = " "), "\n")
        experiment <- COMMON_DATA$experiment
        group <- COMMON_DATA$groups[1] # usually RNA
        res <- experiment$ms$get(group)$get("var")$read(column_names = c("var_id"))$concat()$var_id$as_vector()
        # Adding a study-specific extension to invalidate the fact that
        # annotation_choices is the same when changing a study
        # This helps in reactivity and cache of annotation_choices dependent outputs
        genes_choices <- add_suffix(res, isolate(r$selected_study))

        return(genes_choices)
      }) %>% bindCache(c(r$selected_study, COMMON_DATA$title))

    # Calculate annotation_choices options
    annotation_choices <- reactive(
      {
        req(r$selected_study)
        # r$selected_study() # need to be explicitly used to get reactivity
        experiment <- COMMON_DATA$experiment
        # Adding a study-specific extension to invalidate the fact that
        # annotation_choices is the same when changing a study
        # This helps in reactivity and cache of annotation_choices dependent outputs
        annotation_choices <- experiment$obs$attrnames()
        annotation_choices <- add_suffix(annotation_choices, isolate(r$selected_study))
        annotation_choices <- annotation_choices[which(names(annotation_choices) != "obs_id")]
        return(annotation_choices)
      })


    # # Calculate dimtype_choices options
    dimtype_choices <- reactive(
      {
        req(r$selected_study)
        experiment <- COMMON_DATA$experiment
        group <- COMMON_DATA$groups[1]
        dimtype_choices <- experiment$ms$get(group)$get("obsm")$names()
        order <- c("UMAP" = "X_umap", "TSNE" = "X_tsne", "PCA" = "X_pca")
        dimtype_choices <- order[which(order %in% dimtype_choices)]
        # Adding a study-specific extension to invalidate the fact that
        # dimtype_choices is the same when changing a study
        # This helps in reactivity and cache of dimtype_choices dependent outputs
        dimtype_choices <- add_suffix(dimtype_choices, isolate(r$selected_study))
        return(dimtype_choices)
      })

    # This reactiveVal is a work-around of the fact that I need a virtual
    # "click" on validate_gene_annotation each time a selected study is used
    validate_gene_annotation <- reactiveVal(0)
    observeEvent(input$validate_gene_annotation,
                 {
                 newValue <- validate_gene_annotation() + 1
                 validate_gene_annotation(newValue)

                 telemetry$log_custom_event(
                   "input",
                   details = list("id" = "validate_gene_annotation",
                                  "value" = input$gene_annotation)
                 )
                 }
    )

    # Changes genes and cell annotations options in UI
    observeEvent(c(r$tabs(), r$selected_study), {
      # Recalculate annotation_choices and genes_choices everytime:
      #   the current tab is explore AND the selected study has changed
      if (r$tabs() == "explore" &
        COMMON_DATA$tabs_updated['explore'] != r$selected_study ){

        # freeze prevents the gene annotation to be used when it does not match the study (when changing study)
        # https://github.com/rstudio/shiny/pull/3055
        freezeReactiveValue(input, "gene_annotation")
        # Do necessary changes
        updateSelectizeInput(
          session,
          inputId = "gene_annotation",
          choices =  genes_choices(),
          selected = NULL,
          options = list(clear = FALSE), # Needed to remove previously selected choices (with the server side option)
          server = TRUE
        )

        updateSelectInput(
          session,
          inputId = "cell_annotation",
          choices =  annotation_choices()
        )

        updateSelectInput(
          session,
          inputId = "split_violin",
          choices =  annotation_choices()
        )

        updateSelectInput(
          session,
          inputId = "dimtype",
          choices = dimtype_choices())
        # cat('observeEvent: ', print(r$dimtype_choices(), collapse = " "), "\n")
        newValue <- validate_gene_annotation() + 1     # newValue <- rv$value + 1
        validate_gene_annotation(newValue)

        # Recalls that the explore page has been update accordingly to the new study
        COMMON_DATA$tabs_updated['explore'] <- r$selected_study
      }
    })


    # Retrieve table for dimension plot
    dimtable <- reactive(
      {
        req(input$dimtype)
        # Removes the study specific extension (which is useful for reactivity and cache purposes)
        dimtype <- remove_suffix(input$dimtype, isolate(r$selected_study))
        # cat("dimtype : ", dimtype, "\n")
        experiment <- COMMON_DATA$experiment
        group <- COMMON_DATA$groups[1]
        # Retrieve the first 2 columns of the matrix
        dimtable <- experiment$ms$get(group)$get("obsm")$get(dimtype)$read(coords = list(soma_dim_1 = 0:1))$sparse_matrix()$concat()[, 1:2] # e.g. RNA
        dimtable <- as.matrix(dimtable)
        dimtable <- as.data.frame(dimtable) %>% select(c(.data$V1, .data$V2))
        return(dimtable)
      }) %>% bindCache(c(input$dimtype, COMMON_DATA$title)) #, cache = "session")

    cell_annotation <- reactive({
      req(input$cell_annotation)
      experiment <- COMMON_DATA$experiment
      annotation <- remove_suffix(input$cell_annotation, isolate(r$selected_study))

      cell_annotation <- experiment$obs$read(column_names = annotation)$concat()$to_data_frame() %>%
        as.data.frame()
      return(cell_annotation)
    }) %>% bindCache(c(input$cell_annotation, COMMON_DATA$title)) #, cache = "session")

    split_violin <- reactive({
      req(input$split_violin)
      experiment <- COMMON_DATA$experiment
      annotation <- remove_suffix(input$split_violin, isolate(r$selected_study))

      cell_annotation <- experiment$obs$read(column_names = annotation)$concat()$to_data_frame() %>%
        as.data.frame()
      return(cell_annotation)
    }) %>% bindCache(c(input$split_violin, COMMON_DATA$title)) #, cache = "session")


    gene_annotation <- reactive({
      # cat(file=stderr(), paste0("\nGene_annotation begin: ", Sys.time(), "\n"))
      req(input$gene_annotation)
      experiment <- COMMON_DATA$experiment
      group <- COMMON_DATA$groups[1]
      array <- ifelse("data" %in% COMMON_DATA$arrays, "data", "count")
      annotation <- remove_suffix(input$gene_annotation, isolate(r$selected_study))

      var_query <- tiledbsoma::SOMAAxisQuery$new(
        value_filter = paste("var_id %in% c('",
                             paste(annotation,  collapse = "', '", sep = '')
                             ,"')", sep = '')
        #value_filter = "var_id %in% c('NOCL2','AGRN', 'SDF4')"
        #value_filter = "var_id %in% c('Epcam', 'Cd75', 'Cd45')"
      )

      expt_query <- tiledbsoma::SOMAExperimentAxisQuery$new(
        experiment, group,
        var_query = var_query
      )

      expt_query <- expt_query$to_sparse_matrix(
        collection = "X",
        layer_name = array,
        obs_index =  "obs_id",
        var_index = "var_id"
      )

      gene_annotation <- expt_query %>% as.matrix() %>%
        as.data.frame() %>%
        # reorder columns to the input selection
        select(annotation)

      # cat(file=stderr(), paste0("\nGene_annotation ends: ", Sys.time(), "\n"))

      return(gene_annotation)
    }) %>% bindCache(c(input$gene_annotation, COMMON_DATA$title)) %>%#, cache = "session") %>%
      bindEvent(validate_gene_annotation())


    output$plotDim1 <- renderPlot({
      annotation <- cell_annotation()
      table <- cbind(dimtable(), annotation)
      DimPlot(table = table, features = colnames(annotation))
    })

    output$plotDim2 <- renderPlot({
      annotation <- gene_annotation()
      table <- cbind(dimtable(), annotation)
      plot <- DimPlot(table = table, features = colnames(annotation))
      return(plot)
    })

    output$violin <- renderPlot({
      gene <- gene_annotation()
      features <- colnames(gene)

      cell <- cell_annotation() %>% numeric_to_factor()
      group <- colnames(cell)
      table <- cbind(gene, cell)

      split_violin <- split_violin() %>% numeric_to_factor()

      if (isolate(input$split_violin) != isolate(input$cell_annotation)){
        split.by <- colnames(split_violin)
        table <- cbind(table, split_violin)
      } else {
        split.by <- group
      }

      plot <- VlnPlot(table = table, features = features, group = group, split.by = split.by)
      return(plot)
    })

    output$dotplot <- renderPlot({
      gene <- gene_annotation()
      cell <- cell_annotation() %>% numeric_to_factor()
      table <- cbind(gene, cell)
      plot <- DotPlot(table = table, features = colnames(gene), group = colnames(cell))
      return(plot)
    })


  })
}

## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
