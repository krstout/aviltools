#' Percent Function
#'
#' This function creates percentages.
#' @param x The numerator.
#' @param y The denominator.
#' @keywords simple stats
#' @export
#' @examples
#' per(15, 100)

per <- function(x, y) {
  p <- round(((x/y)*100), 2)
  return(p)
}
