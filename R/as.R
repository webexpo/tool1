#' Format Numeric Values as Percentages
#'
#' Coerce numeric values to characters, keeping a certain number of digits
#' and appending a percentage sign to the output.
#'
#' @param x Passed as is to [signif()].
#'
#' @param digits Passed as is to [signif()].
#'
#' @returns A character string.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname helpers-misc-percentages
#'
#' @rdname as
#' @export
as_percentage <- function(x, digits = default_n_digits) {
    return(paste0(signif(x, digits), "%"))
}
