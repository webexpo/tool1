#' Convert numeric values to percentages
#'
#' Convert [numeric] values ([integer] and [double] values) to a character
#' vector. Each element represents a percentage (a number ending with a '%'
#' symbol). 3 significant digits are kept.
#'
#' @param values A [numeric] vector, possibly empty. `NA` values are disallowed.
#'
#' @param digits A non-`NA` [integer] value greater than 0. Values are rounded
#'   up to a precision of `digits`.
#'
#' @param verbose A non-`NA` [logical] value. Should `values` and formatted
#'   values be reported to the console?
#'
#' @returns
#' A character vector having the same length as `values`. Names are preserved.
#'
#' @examples
#' as_percentage(c(a = 1.34452, b = 56.31))
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
as_percentage <- function(
    values  = numeric(),
    digits  = 3L,
    verbose = getOption("tool1.verbose.percentage", FALSE))
{
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
    if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
        stop(
            "'verbose' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').",
            call. = FALSE)
    }

    out <- structure(sprintf("%.*f%%", digits, values), names = names(values))

    if (verbose) {
        mapply(input = values, output = out, \(input, output) {
            cat("[NOTE]", "[as_percentage()]", input, "->", output, "\n",
                sep = " ")
        })
    }

    return(out)
}

percText <- function(perc) {
    rem <- perc %% 10
    if ( rem == 1) {
        suff <- gett("st")
    } else
        if ( rem == 2 ) {
        suff <- gett("nd")
    } else
    if ( rem == 3 ) {
        suff <- gett("rd")
    } else {
        suff <- gett("th")
    }
    return (paste0(perc, suff, " ", gett("percentile") ) )
}
