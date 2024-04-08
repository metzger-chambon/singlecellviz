#' plots
#'
#' @description Creates dimension plot(s).
#'
#' @param table a data.frame where each row corresponds to a cell, and each column to a characteristic.
#' V1 (x axis), V2 (y axis), and any value given in "features" parameters must be a column name.
#' @param features a vector of features name to be used as color
#'
#' @return a patchwork of dimension plots
#' #' @examples
#' # Table
#' table <- structure(list(V1 = c(10.5895051956177, 15.6335277557373, 13.046275138855,
#' 26.6680164337158, 13.3903284072876, 10.344783782959), V2 = c(14.850136756897,
#' 26.4807929992676, 17.4865455627441, 14.7715759277344, 9.01217460632324,
#' 16.3305702209473), seurat_clusters = structure(c(2L, 4L, 2L,
#' 3L, 7L, 2L), levels = c("0", "1", "2", "3", "4", "5", "6", "7",
#' "8"), class = "factor")), row.names = c(NA, 6L), class = "data.frame")
#' features <- c("seurat_clusters")
#' DimPlot(table, features)
#'
#' @noRd
#' @importFrom patchwork wrap_plots

DimPlot <- function(table, features){
  # TODO: verify order of the barcodes to make sure we are aggregating data correctly?
  # .data is used to handle R CMD check
  # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  plots <- lapply(features, function(x){
    ggplot(table) + geom_point(aes(x = .data$V1, y = .data$V2, col = .data[[x]])) +
      labs(color = x)
  })
  plot <- wrap_plots(plots)
  return(plot)
}

#' @description Creates violin plot(s).
#'
#' @param table a data.frame where each row corresponds to a cell, and each column to a characteristic.
#' Any value given in "features" and "group" parameters must be a column name.
#' @param features a vector of features name to be used as y-axis
#' @param group a vector of group (~ annotation of cells) name to be used as x-axis and fill
#'
#' @return a patchwork of violin plots
#'
#' @noRd
#' @importFrom patchwork wrap_plots

VlnPlot <- function(table, features, group){
  plots <- lapply(features, function(x){
    ggplot(table) + geom_violin(aes(x = .data[[group]], y = .data[[x]], fill = .data[[group]])) +
      labs(title = x,
           x = group,
           fill = group,
           y = "Expression Level") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
  })
  plot <- wrap_plots(plots)
  return(plot)
}


#' @description Creates dot plot.
#'
#' @param table a data.frame where each row corresponds to a cell, and each column to a characteristic.
#' Any value given in "features" and "group" parameters must be a column name.
#' @param features a vector of features name to be used as x-axis
#' @param group a vector of group (~ annotation of cells) name to be used as y-axis
#'
#' @return a ggplot representing a dotplot
#'
#' @noRd

DotPlot <- function(table, features, group,
                    scale = TRUE,
                    col.min = -2.5, col.max = 2.5,
                    cols = c("lightgrey", "blue")){
  # The size of the dot encodes the percentage of cells within a class,
  # while the color encodes the AverageExpression level across all cells
  # within a class (blue is high)

  # Small useful functions
  PercentAbove <- function(x, threshold) {
    return(length(x = x[x > threshold]) / length(x = x))
  }

  MinMax <- function(data, min, max) {
    data2 <- data
    data2[data2 > max] <- max
    data2[data2 < min] <- min
    return(data2)
  }


  # data.features$id = table$group
  # TO DO checks that results are the same as the ones in seurat

  ordered_groups <- if(is.factor(table[,group])) {levels(table[,group])} else {unique(x = table[,group])}
  data.plot <- lapply(
    X = ordered_groups, #,
    FUN = function(ident) {
      data.use <- table[table[,group] == ident, 1:(ncol(x = table) - 1), drop = FALSE]
      avg.exp <- apply(
        X = data.use,
        MARGIN = 2,
        FUN = function(x) {
          return(mean(x = expm1(x = x)))
        }
      )
      pct.exp <- apply(X = data.use, MARGIN = 2, FUN = PercentAbove, threshold = 0)
      return(list(avg.exp = avg.exp, pct.exp = pct.exp))
    }
  )
  names(data.plot) <- ordered_groups

  data.plot <- lapply(
    X = names(x = data.plot),
    FUN = function(x) {
      data.use <- as.data.frame(x = data.plot[[x]])
      data.use$features.plot <- rownames(x = data.use)
      data.use$id <- x
      return(data.use)
    }
  )
  data.plot <- do.call(what = 'rbind', args = data.plot)

  ngroup <- length(x = unique(x = data.plot$id))
  if (ngroup == 1) {
    scale <- FALSE
    warning(
      "Only one identity present, the expression values will be not scaled",
      call. = FALSE,
      immediate. = TRUE
    )
  } else if (ngroup < 5 & scale) {
    warning(
      "Scaling data with a low number of groups may produce misleading results",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  avg.exp.scaled <- sapply(
    X = unique(x = data.plot$features.plot),
    FUN = function(x) {
      data.use <- data.plot[data.plot$features.plot == x, 'avg.exp']
      if (scale) {
        data.use <- scale(x = log1p(data.use))
        data.use <- MinMax(data = data.use, min = col.min, max = col.max)
      } else {
        data.use <- log1p(x = data.use)
      }
      return(data.use)
    }
  )

  avg.exp.scaled <- as.vector(x = t(x = avg.exp.scaled))
  data.plot$avg.exp.scaled <- avg.exp.scaled

  data.plot$features.plot <- factor(
    x = data.plot$features.plot,
    levels = features
  )
  data.plot$pct.exp <- data.plot$pct.exp * 100
  data.plot$id <- as.factor(data.plot$id)
  # reorder if group are factors
  if (is.factor(table[,group])) {levels(data.plot$id) <- ordered_groups}

  plot <- ggplot(data.plot, aes(x = .data$features.plot, y = .data$id)) +
    geom_point(aes(size = .data$pct.exp, color = .data$avg.exp.scaled)) +
    #scale.func(range = c(0, dot.scale), limits = c(scale.min, scale.max)) +
    guides(size = guide_legend(title = 'Percent Expressed')) +
    labs(
      x = '',
      y = 'Annotation',
    ) + scale_color_gradient(low = cols[1], high = cols[2]) +
    scale_y_discrete(limits=rev) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))


  return(plot)
}



#' @description Creates heatmap plot.
#'
#' @param table a pivot_longer data.frame where each row corresponds to a
#' gene x group of the cells combination, associated with an expression value.
#' @param split a boolean indicating whether there is group2 variable to split the plot
#' @return a ggplot representing a heatmap
#'
#' @noRd

HeatmapPlot <- function(table, split = FALSE){

  p <- ggplot(table, aes(x=.data$group, y=.data$gene,
                         fill = .data$expression,
                         text = .data$text)) +
    geom_tile() +
    scale_y_discrete(limits = rev) +
    scale_fill_viridis_c() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  # Split heatmap
  if (split){
    p <- p + facet_grid(. ~ group2)
  }
  return(p)
}
