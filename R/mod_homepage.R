#' homepage UI Function
#'
#' @description The homepage of the App as html.
#'
#' @return A html object
#'
#' @noRd
#'
mod_homepage_ui <- function(){
  HTML(paste0("
  <p> In ",
  enurl(url = "https://www.igbmc.fr/equipes/roles-physiopathologiques-des-voies-de-signalisation-des-recepteurs-nucleaires",
        text = "Metzger Lab"),
  ", we are particularly interested in ... </p>
  <p> This tool will help you explore some of our single cell datasets. </p>
  <p> Start by selecting a study on the left panel, and move to the other tabs to explore.</p>
  "))

}

## To be copied in the UI
# mod_homepage_ui()

