#' Check if package cyjShiny from cytoscape github has been installed
#'
#' @return NULL
#'
#' @importFrom utils packageVersion
#' @export
#'
check_install_requirements <- function() {
  if (requireNamespace("cyjShiny")) {
    stopifnot(packageVersion("cyjShiny") >= "0.99.40")
  } else {
    stop("cyjShiny not installed")
  }
}
