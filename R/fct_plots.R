#' plots
#'
#'

#'
#' @description Creates theme of plot(s).
singlecellplot_theme <- function(){
  theme_set(theme(panel.background = element_rect(fill = "white", color = NA),
                  panel.grid.major = element_line(color = 'lightgrey'),#element_blank(), #
                  panel.grid.minor = element_blank(),
                  legend.key = element_blank(),
                  text = element_text(size = 16)
                  )
  )
}

#'
#' @description Creates dimension plot(s).
#'
#' @param table a data.frame where each row corresponds to a cell, and each column to a characteristic.
#' V1 (x axis), V2 (y axis), and any value given in "features" parameters must be a column name.
#' @param features a vector of features name to be used as color
#'
#' @return a patchwork of dimension plots
#' @examples
#' Table
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

DimPlot <- function(table, features,
                    gradient_cols = c("lightgrey", "blue")){
  # .data is used to handle R CMD check
  # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  plots <- lapply(features, function(x){
    gg <- ggplot(table) +
      geom_point(aes(x = .data$V1, y = .data$V2, col = .data[[x]])) +
      labs(x = "Dim1", y = "Dim2", color = x) +
      theme(panel.grid.major = element_blank())
    if (is.numeric(table[[x]])) {
      gg <- gg + scale_color_gradient(low = gradient_cols[1], high = gradient_cols[2])
    } else {
      #gg <- gg + scale_color_manual(values = cols)
    }
    return(gg)
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
#' @examples
#' structure(list(Epcam = c(0, 0, 0, 0, 0, 0, 0, 0, 2.80717076108235,
#' 0, 1.78347441769395, 0, 0, 0, 0, 2.73443906513021, 0, 0, 0, 0
#' ), Krt8 = c(0, 0, 0, 0, 0, 0, 0, 0, 3.77959331014652, 0, 0, 0,
#'             0, 0, 0, 1.79327148482397, 0, 0, 1.37038283208645, 0), orig.ident = structure(c(1L,
#'             2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L,
#'             2L, 2L, 2L), levels = c("Control", "Gem72"), class = "factor"),
#'             group = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L,
#'                                 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), levels = c("1","2"), class = "factor")),
#'                                 row.names = c("GTTCATTGTCACTGGC-1_1",
#'                                "CGGACGTAGGGCATGT-1_2", "GCAAACTCATTCACTT-1_2", "GCAATCACACGCGAAA-1_2",
#'                                "CACACCTCAGTCGATT-1_2", "CATATGGGTAAACGCG-1_2", "ATTACTCTCACAGTAC-1_2",
#'                                "GGTATTGCACTGCCAG-1_2", "CCTAAAGCATGTCCTC-1_1", "GGCAATTCAATGGATA-1_1",
#'                                "CGATGGCAGCGAGAAA-1_1", "CGAGCACAGCCACCTG-1_2", "CGGACGTAGAGGGCTT-1_2",
#'                                "AGTGGGATCTAAGCCA-1_1", "TTCCCAGTCACTCCTG-1_1", "CCCAGTTGTAGCGTAG-1_2",
#'                                "GATCGTACATGCTGGC-1_2", "ATTTCTGGTCGCGAAA-1_2", "GGCTGGTAGGGTCTCC-1_2",
#'                                "CAGCATAAGCACCGTC-1_2"), class = "data.frame")
#' VlnPlot(table = table, features = c("Epcam", "Krt8"), group = "group", split.by = "orig.ident")
#' @noRd
#' @importFrom patchwork wrap_plots
#' @importFrom stats rnorm

VlnPlot <- function(table, features, group, split.by){
  # Add noise ?
  noise <- rnorm(n = length(x = table[, features])) / 100000
  table[, features] <- table[, features] + noise

  plots <- lapply(features, function(x){
    ggplot(table, aes(x = .data[[group]], y = .data[[x]], fill = .data[[split.by]])) +
      geom_violin(scale = 'width', adjust = 1, trim = TRUE) +
      #geom_jitter(height = 0, size = 0.1, alpha = 1, show.legend = FALSE) +
      labs(title = x,
           x = group,
           fill = split.by,
           y = "Expression Level") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
  })
  plot <- wrap_plots(plots, guides = "collect")
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
  #avg.exp.scaled <- data.frame(avg.exp.scaled = avg.exp.scaled, id = ordered_groups)

  data.plot <- cbind(data.plot, avg.exp.scaled)#, by = "id")

  data.plot$features.plot <- factor(
    x = data.plot$features.plot,
    levels = features
  )
  data.plot$pct.exp <- data.plot$pct.exp * 100
  data.plot$id <- factor(data.plot$id, levels = ordered_groups)

  plot <- ggplot(data.plot, aes(x = .data$features.plot, y = .data$id)) +
    geom_point(aes(size = .data$pct.exp, color = .data$avg.exp.scaled)) +
    #scale.func(range = c(0, dot.scale), limits = c(scale.min, scale.max)) +
    guides(size = guide_legend(title = 'Percent Expressed')) +
    labs(
      x = '',
      y = 'Annotation'
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
#' @examples
#'
#' table <- structure(list(gene = structure(c(27L, 27L, 27L, 27L, 27L, 27L
#' ), levels = c("RPS12", "RPS27", "RPS6", "RPL32", "RPS14", "IL32",
#'               "LTB", "CD3D", "IL7R", "LDHB", "FCN1", "LGALS2", "S100A8", "S100A9",
#'               "CD14", "MS4A1", "CD79A", "CD79B", "LINC00926", "TCL1A", "CCL5",
#'               "NKG7", "GZMA", "CST7", "GZMK", "CDKN1C", "HES4", "RP11-290F20.3",
#'               "MS4A7", "FCGR3A", "GZMB", "FGFBP2", "SPON2", "PRF1", "GNLY",
#'               "FCER1A", "SERPINF1", "CLEC10A", "ENHO", "CLIC2", "LY6G6F", "AP001189.4",
#'               "TMEM40", "ITGA2B", "GP9"), class = "factor"), group = c("0",
#'               "1", "2", "3", "4", "5"), expression = c(12.6, 14.4, 147, 23.99,
#'               0, 435.6), marker_of = c(HES4 = "5", HES4 = "5", HES4 = "5",
#'               HES4 = "5", HES4 = "5", HES4 = "5"), text = c("Gene: HES4\nExpression: 12.6\nMarker of group: 5",
#'                                                              "Gene: HES4\nExpression: 14.4\nMarker of group: 5", "Gene: HES4\nExpression: 147\nMarker of group: 5",
#'                                                              "Gene: HES4\nExpression: 23.99\nMarker of group: 5", "Gene: HES4\nExpression: 0\nMarker of group: 5",
#'                                                              "Gene: HES4\nExpression: 435.6\nMarker of group: 5")), row.names = c(NA,
#'                                                                  -6L), class = c("tbl_df", "tbl", "data.frame"))
#' HeatmapPlot(table)
#' @noRd

HeatmapPlot <- function(table, split = FALSE, threshold_value = 50){
  n_x <- length(unique(table$group))
  n_y <- length(unique(table$gene))
  fct_size <- function(x){
    val <- 15-3*log(0.239876*x+8)
    min <- 1
    return(ifelse(val > min, val, min))
  }
  p <- ggplot(table, aes(x=.data$group, y=.data$gene,
                         fill = .data$expression,
                         text = .data$text)) +
    geom_tile() +
    scale_y_discrete(limits = rev) +
    scale_fill_viridis_c() +
    theme(text = element_text(size = 12),
          #legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5,
                                     hjust=1, size = fct_size(n_x)),
          #axis.title.x = element_text(vjust = -2), # does not work with ggplotly
          axis.text.y = element_text(size = fct_size(n_y)) ) +
    labs(x = 'Group',
         y = 'Gene',
         fill = "Expression")
  # Check the number of y-axis values
  # if( n_y > threshold_value){
  #   p <- p + theme(#axis.title.y=element_blank(),
  #     axis.text.y=element_blank(),
  #     axis.ticks.y=element_blank()
  #   )
  # }

  # Split heatmap
  if (split){
    p <- p + facet_grid(. ~ group2)
  }
  return(p)
}
