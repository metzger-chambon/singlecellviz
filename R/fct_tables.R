#' tables
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
clean_findmarkers_result <- function(table){
  # If FindMarkers was run, the column gene does not exist (it is the rownames i.e. obs_id)
  if (!"gene" %in% colnames(table)){
    table <- table %>% rename("gene" = "obs_id")
  } else { #If FindAllMarkers was run, "gene" column exist but "obs_id" is not usful
    table <- table %>% select(-c( "obs_id"))
  }
  table <- table %>%
    select(-c("soma_joinid")) %>%
    mutate_if(is.numeric, signif, 4) %>%
    mutate(gene = as.factor(.data$gene))
  return(table)
}

#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'

clean_expression <- function(table){
  table <- table %>%
    select(-c("soma_joinid")) %>%
    mutate_if(is.numeric, signif, 4) %>%
    rename('gene' = 'obs_id') %>%
    relocate('gene', .before = 1)
  return(table)
}


#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom stats quantile
#'
numeric_to_factor <- function(table, n = 10){
  # convert numerical values into factor
  need_conversion <- sapply(table, is.numeric)
  if (any(need_conversion)){
    table[need_conversion] <- lapply(table[need_conversion], FUN = function(x){
      qv <- round(quantile(x, probs = seq(0, 1, 1/n)))
      cut(x, breaks = qv[!duplicated(qv)], include.lowest=TRUE)
    })
  }
  return(table)
}

#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
string_to_factor <- function(table){
  # convert string values into factor
  need_conversion <- sapply(table, is.character)
  if (any(need_conversion)){
    table[need_conversion] <- lapply(table[need_conversion], FUN = function(x){
      as.factor(x)
    })
  }
  return(table)
}


#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom DT datatable formatSignif

show_findmarkers_result <- function(table){
  order_num <- c("avg_log2FC", "p_val", "p_val_adj", "pct.1", "pct.2")
  col_num <- order_num[order_num %in% colnames(table)]

  order <- c("cluster", "gene", col_num)
  col <- order[order %in% colnames(table)]

  table <- table %>%
    select(col)
  table <- DT::datatable(table,
                rownames = FALSE,
                filter = list(position = 'top', clear = TRUE),
                selection = 'none'#,
                #options = list(autoWidth = TRUE, bAutoWidth = FALSE)
                #options = list(columnDefs = list(list(width = '20px', targets = "_all")))
  ) %>%
    DT::formatSignif(col_num, digits=4)
  return(table)
}
