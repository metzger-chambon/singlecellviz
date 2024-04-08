#' Add suffix to input choices
#'
#' @param input a vector containing inputs
#' @param suffix a character representing the suffix
#' @param sep a character representing the separator
#'
#' @return a named vector
#' @noRd
#'
#' @examples
#' add_suffix(c("aaaa", "bb", "c"), "1")
add_suffix <- function(input, suffix, sep = ""){
  if (is.null(names(input))){
    names(input) <- input
  }
  input[] <- paste(input, suffix, sep = sep)
  return(input)
}

#' Remove suffix to input choices
#'
#' @param input a vector containing inputs with suffix
#' @param suffix a character representing the suffix
#' @param sep a character representing the separator
#'
#' @return a named vector
#' @noRd
#'
#' @examples
#' remove_suffix(c("aaaa1", "bb1", "c1"), "1")
remove_suffix <- function(input, suffix, sep = ""){
  n <- nchar(input) - nchar(suffix) + nchar(sep)
  input <- substr(input, start = 1, stop = n)
  return(input)
}
