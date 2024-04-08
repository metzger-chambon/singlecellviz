#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' @importFrom shiny tags
enurl <- function(url, text) {
  tags$a(href = url, text)
}
