#' Validate Values
#'
#' Check whether values meets certain criteria.
#'
#' @param x Any \R object.
#'
#' @param allow_empty A logical value. Are empty character strings valid values?
#'
#' @returns A logical value.
#'
#' @examples
#' is_num1(1L)
#' is_num1(1.0)
#' is_num1(c(1.3, 2.4, 3.5))
#' is_num1("Hello")
#'
#' is_chr1("")
#' is_chr1("Hello")
#' is_chr1(c("Hello", "world"))
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname helpers-misc-is
#'
#' @export
is_num1 <- function(x) {
    return(is.numeric(x) && length(x) == 1L && !is.na(x))
}

#' @rdname helpers-misc-is
#' @export
is_chr1 <- function(x, allow_empty = FALSE) {
    return(
        is.character(x) &&
        length(x) == 1L &&
        (nzchar(x) || allow_empty) &&
        !is.na(x)
    )
}

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
#' @export
as_percentage <- function(x, digits = default_n_digits) {
    return(paste0(signif(x, digits), "%"))
}
