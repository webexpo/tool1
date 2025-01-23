#' Convert numeric values to percentages
#'
#' Convert [numeric] values ([integer] and [double] values) to a character
#' vector. Each element represents a percentage (a number ending with a '%'
#' symbol). 3 significant digits are kept.
#'
#' @param values A [numeric] vector, possibly empty. `NA` values are disallowed.
#'
#' @returns
#' A character vector having the same length as `values`. Names are preserved.
#'
#' @examples
#' as_percentage(c(a = 1.34452, b = 56.31))
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
as_percentage <- function(values = numeric()) {
    stopifnot(
        is.numeric(values),
        !anyNA(values))

    return(structure(sprintf("%.3f%%", values), names = names(values)))
}
