#' Format Values
#'
#' Formats displayed values.
#'
#' @param value A numeric (integer or numeric) vector of length 1. NA values
#'   are disallowed.
#'
#' @param suffixes A character of length 10. The suffix to use for each
#'   reminder (in base 10). The first element is the suffix to use when there
#'   is no reminder, and the last element is the one to use when the reminder
#'   is equal to 9.
#'
#' @param exceptions A named character vector. Exceptions that overwrite
#'   `suffixes` for specific integer values.
#'
#'   * Names must represent valid integer values.
#'   * Values must be suffixes to use for the underlying integer values.
#'
#' @returns
#' [ordinal_number_suffix()] A character of length 1.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @examples
#' ordinal_number_suffix(1)
#' ordinal_number_suffix(1.01)
#' ordinal_number_suffix(5)
#' ordinal_number_suffix(5.01)
#' ordinal_number_suffix(11.01)
#' ordinal_number_suffix(12.01)
#' ordinal_number_suffix(13.01)
#'
#' @export
ordinal_number_suffix <- function(
    value    = numeric(1L),
    suffixes = c(
        translate("th"),   # Reminder = 0.
        translate("st"),   # Reminder = 1.
        translate("nd"),   # Reminder = 2.
        translate("rd"),   # Reminder = 3.
        translate("th"),   # Reminder = 4.
        translate("th"),   # Reminder = 5.
        translate("th"),   # Reminder = 6.
        translate("th"),   # Reminder = 7.
        translate("th"),   # Reminder = 8.
        translate("th")),  # Reminder = 9.
    exceptions = c(
        "11" = translate("th"),
        "12" = translate("th"),
        "13" = translate("th")))
{
    if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
        stop("'value' must a non-NA numeric value of length 1.", call. = FALSE)
    }
    if (!is.character(suffixes) || length(suffixes) != 10L || anyNA(suffixes)) {
        stop(
            "'suffixes' must a character vector of length 10.",
            " It cannot contain NA values.",
            call. = FALSE)
    }
    if (!is.character(exceptions) ||
        anyNA(exceptions) ||
        is.null(names(exceptions)) ||
        !all(nzchar(names(exceptions)))) {
        stop(
            "'exceptions' must a character vector.\n",
            "It cannot contain NA values.\n",
            "It must be named. Names cannot be empty and must represent integer values.",
            call. = FALSE)
    }

    # values is coerced to an integer to drop decimals
    # and implicitly coerced to a character by match()
    # for matching purposes.
    if (m <- match(as.integer(value), names(exceptions), 0L)[[1L]]) {
        return(exceptions[[m]])
    }

    return(suffixes[[(value %% 10L) + 1L]])
}
