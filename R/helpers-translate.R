#' Translate Source Text
#'
#' Alias to method `$translate()` of the main [transltr::Translator] object.
#'
#' @rdname helpers-translate
#' @export
translate <- tr$translate

#' Ordinal Numbers (1st, 2nd, 3rd, 4th, etc.)
#'
#' Display values as ordinal numbers (e.g. 1st, 2nd, 3rd, 4th, etc.). Built-in
#' rules are provided for English and French.
#'
#' ## Rulesets
#'
#' Grammar rules are implemented in dedicated `ordinal_rules_*()` functions.
#' These functions return two components.
#'
#'   - `indicators` hold abbreviated suffixes to use for each reminder (0 to 9)
#'     of `x` when it is divided by 10 (`x mod 10`).
#'   - `exceptions` is a list of *special* suffixes to use for specific values
#'     only.
#'
#' Each supported language requires its own `ordinal_rules_*()` function.
#'
#' @param x A numeric value.
#'
#' @param lang A character string. The underlying language code.
#'
#' @param format A character string equal to one of the values below and
#'   controlling the output's format.
#'
#'   - `html`: a `shiny.tag` object.
#'   - `string`: a character string is returned.
#'
#' @param ... Further arguments passed to [ordinal_rules_english()] and
#'   [ordinal_rules_french()].
#'
#' @param gender A character string equal to `masculin` or `feminin`. The
#'   gender of French ordinals.
#'
#' @param plural A logical value. Should plural form of French ordinals be
#'   used?
#'
#' @returns
#' [ordinal()] and [ordinal_abbr()] return a character string.
#'
#' [ordinal_rules()],
#' [ordinal_rules_english()] and
#' [ordinal_rules_french()] return a named list of length containing the
#' following elements.
#'
#' \describe{
#'   \item{`indicators`}{
#'     A named character vector of length 10. Names represent remainders 0 to 9
#'     (as character strings) and values are ordinal abbrevations/suffix to use.
#'   }
#'   \item{`exceptions`}{
#'     A named character vector. It can be empty. Names represent specific
#'     integer values (as character strings) and value are special ordinal
#'     abbreviations/suffix to use for them.
#'   }
#' }
#'
#' @examples
#' ordinal(1)
#' ordinal_abbr(1, gender = "feminin", plural = TRUE)
#' ordinal(1, "fr")
#' ordinal_abbr(1, "fr")
#' ordinal(2.34, "fr", gender = "feminin", plural = TRUE)
#' ordinal_abbr(2.34, "fr", gender = "feminin", plural = TRUE)
#'
#' @seealso
#' [The scales package](https://scales.r-lib.org/). It (very loosely) inspired
#' the current design.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname helpers-translate
#' @export
ordinal <- function(
    x      = numeric(1L),
    lang   = "en",
    format = c("html", "string"),
    ...)
{
    format <- match.arg(format)
    x_int <- as.integer(x)
    abbr <- ordinal_abbr(x_int, lang, ...)

    return(
        switch(format,
            string = sprintf("%i%s", x_int, abbr),
            html   = tags$span(
                x_int,
                tags$sup(
                    abbr,
                    .noWS = c(
                        "before",
                        "after",
                        "outside",
                        "after-begin",
                        "before-end"
                    )
                ),
                .noWS = c(
                    "before",
                    "after-begin",
                    "before-end"
                )
            )
        )
    )
}

#' @rdname helpers-translate
#' @export
ordinal_abbr <- function(x = numeric(1L), lang = "en", ...) {
    rules <- ordinal_rules(lang, ...)

    # Check if x is an exception first.
    # values is coerced to an integer to drop decimals
    # and implicitly coerced to a character by match()
    # for matching purposes.
    if (m <- match(as.integer(x), names(rules$exceptions), 0L)[[1L]]) {
        return(rules$exceptions[[m]])
    }

    # Oterwise, return the expected indicator.
    return(rules$indicators[[(x %% 10L) + 1L]])
}

#' @rdname helpers-translate
#' @export
ordinal_rules <- function(lang = "en", ...) {
    return(
        switch(lang,
            en = ordinal_rules_english(),
            fr = ordinal_rules_french(...),
            stop("no support for ordinal numbers in the current language.")))
}

#' @rdname helpers-translate
#' @export
ordinal_rules_english <- function() {
    return(
        list(
            # remainder when integer is divided by 10 = abbrevation to use.
            indicators = c(
                "0" = "th",
                "1" = "st",
                "2" = "nd",
                "3" = "rd",
                "4" = "th",
                "5" = "th",
                "6" = "th",
                "7" = "th",
                "8" = "th",
                "9" = "th"),
            # value of integer = abbreviation to use.
            exceptions <- c(
                "11" = "th",
                "12" = "th",
                "13" = "th")))
}

#' @rdname helpers-translate
#' @export
ordinal_rules_french <- function(
    gender = c("masculin", "feminin"),
    plural = FALSE)
{
    gender <- match.arg(gender)

    # remainder when integer is divided by 10 = abbrevation to use.
    indicators <- c(
        "0" = "e",
        "1" = "e",
        "2" = "e",
        "3" = "e",
        "4" = "e",
        "5" = "e",
        "6" = "e",
        "7" = "e",
        "8" = "e",
        "9" = "e"
    )

    exceptions <- switch(gender,
        # value of integer = abbreviation to use.
        masculin = c(
            "1" = "er",
            "2" = "d"),
        feminin = c(
            "1" = "re",
            "2" = "de"))

    if (plural) {
        indicators <- structure(paste0(indicators, "s"), names = names(indicators))
        exceptions <- structure(paste0(exceptions, "s"), names = names(exceptions))
    }

    return(list(indicators = indicators, exceptions = exceptions))
}
