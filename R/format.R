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
as_percentage <- function(values = numeric(), digits = 3L) {
    if (!is.numeric(values) || anyNA(values)) {
        stop(
            "'values' must be a numeric vector of non-NA values.",
            call. = FALSE)
    }
    if (!is.integer(digits) ||
        length(digits) != 1L ||
        is.na(digits) ||
        !is.finite(digits) ||
        digits < 0L) {
        stop(
            "'digits' must be a finite and non-NA integer value greater than 0.",
            call. = FALSE)
    }

    return(structure(sprintf("%.*f%%", digits, values), names = names(values)))
}
