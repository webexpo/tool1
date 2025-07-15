#' Format Numeric Values as Percentages
#'
#' Round numeric values keeping `digits` significant digits, coerce them to
#' characters, and append a percentage sign to the output.
#'
#' @param x A numeric value.
#'
#' @param digits An integer value. The number of significant digits to retain.
#'
#' @returns A character string.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname as-percentage
#' @export
as_percentage <- function(x = 0, digits = getOption("app_n_signif_digits")) {
    stopifnot(is_int1(digits))
    return(paste0(signif(x, digits), "%"))
}

#' Format Numeric Estimates as Confidence Intervals
#'
#' Create a standardized character string from estimates and their corresponding
#' lower and upper confidence limits for displaying purposes. The format is
#' `<x> [<lower> - <upper>]`.
#'
#' @param x A numeric value.
#'
#' @param lower A numeric value. The lower limit for the underlying interval.
#'
#' @param upper A numeric value. The upper limit for the underlying interval.
#'
#' @param digits An integer value. The number of significant digits to retain.
#'
#' @returns A character string.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname as-interval
#' @export
as_interval <- function(x, ...) {
    UseMethod(("as_interval"))
}

#' @rdname as-interval
#' @export
as_interval.numeric <- function(
    x      = 0,
    lower  = 0,
    upper  = 0,
    digits = getOption("app_n_signif_digits"))
{
    stopifnot(exprs = {
        is_num1(lower)
        is_num1(upper)
        is_int1(digits)
    })

    x <- signif(x, digits)
    lower <- signif(lower, digits)
    upper <- signif(upper, digits)
    return(NextMethod())
}

#' @rdname as-interval
#' @export
as_interval.default <- function(x, lower, upper) {
    return(sprintf("%s [%s - %s]", x, lower, upper))
}
