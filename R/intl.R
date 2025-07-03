#' Translatable Uniform Resource Locators
#'
#' Mark a URL as such and attach alternative URLs targeting other languages
#' to it.
#'
#' @param x A character string. The default URL corresponding to `default_lang`.
#'
#' @param ... Usage depends on the underlying function.
#'
#'   - For [url()], further optional named character vectors. The URLs for
#'     languages other than `default_lang`. Names must correspond to language
#'     codes.
#'   - For `[[`, further arguments passed to the default `[[` S3 method.
#'
#' @param i A character string. A language code. If unavailable, a default
#'   value is returned.
#'
#' @details
#' S3 class `url` has a `[[` S3 method ensuring that a default value is always
#' returned if `i` is undefined.
#'
#' @returns
#' [url()] returns a named character vector of S3 class `url`. It has a
#' `default` attribute corresponding to the default language code of `x`.
#'
#' `[[` returns a character string.
#'
#' @examples
#' default_lang <- "en"
#'
#' url <- url(
#'   "https://lavoue.shinyapps.io/Tool3v3En/",
#'   fr = "https://lavoue.shinyapps.io/Tool3v3Fr/"
#' )
#'
#' url[["en"]]  ## Returns "https://lavoue.shinyapps.io/Tool3v3En/".
#' url[["fr"]]  ## Returns "https://lavoue.shinyapps.io/Tool3v3Fr/".
#' url[["es"]]  ## Returns "https://lavoue.shinyapps.io/Tool3v3En/".
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname intl-url
#' @export
url <- function(x = "", ...) {
    names(x) <- default_lang
    urls <- c(x, ...)
    langs <- names(urls)

    stopifnot(exprs = {
        is_chr(urls)
        is_chr(langs)
        all(nzchar(urls))
        all(nzchar(langs))
    })

    return(
        structure(urls,
            default = default_lang,
            class   = c("url", "character")
        )
    )
}

#' @rdname intl-url
#' @export
`[[.url` <- function(x, i, ...) {
    if (is.na(match(i, names(x)))) {
        i <- attr(x, "default", TRUE)
    }

    return(NextMethod())
}

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
#' @param gender A character string equal to `masculine` or `feminine`. The
#'   gender of ordinal numbers, if any.
#'
#' @param plural A logical value. Should plural form of ordinals (if any) be
#'   used?
#'
#' @returns
#' [ordinal()] and [ordinal_abbr()] return a character string.
#'
#' [ordinal_rules()],
#' [ordinal_rules_english()],
#' [ordinal_rules_french()], and
#' [ordinal_rules_spanish()] return a named list of length containing the
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
#' ordinal_abbr(1, gender = "feminine", plural = TRUE)
#' ordinal(1, "fr")
#' ordinal_abbr(1, "fr")
#' ordinal(2.34, "fr", gender = "feminine", plural = TRUE)
#' ordinal_abbr(2.34, "fr", gender = "feminine", plural = TRUE)
#'
#' @seealso
#' [The scales package](https://scales.r-lib.org/). It (very loosely) inspired
#' the current design.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname intl-ordinal
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

#' @rdname intl-ordinal
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

#' @rdname intl-ordinal
#' @export
ordinal_rules <- function(lang = "en", ...) {
    return(
        switch(lang,
            en = ordinal_rules_english(),
            fr = ordinal_rules_french(...),
            es = ordinal_rules_spanish(...),
            "{no support for ordinal numbers in the current language.}"
        )
    )
}

#' @rdname intl-ordinal
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
                "9" = "th"
            ),
            # value of integer = abbreviation to use.
            exceptions <- c(
                "11" = "th",
                "12" = "th",
                "13" = "th"
            )
        )
    )
}

#' @rdname intl-ordinal
#' @export
ordinal_rules_french <- function(
    gender = c("masculine", "feminine"),
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
        masculine = c(
            "1" = "er",
            "2" = "d"),
        feminine = c(
            "1" = "re",
            "2" = "de"))

    if (plural) {
        indicators <- structure(paste0(indicators, "s"), names = names(indicators))
        exceptions <- structure(paste0(exceptions, "s"), names = names(exceptions))
    }

    return(list(indicators = indicators, exceptions = exceptions))
}

#' @rdname intl-ordinal
#' @export
ordinal_rules_spanish <- function(gender = c("masculine", "feminine")) {
    gender <- match.arg(gender)

    # remainder when integer is divided by 10 = abbrevation to use.
    # Formally, \U00BA ("º") is used for masculine, and \U00AA ("ª")
    # for feminine. These are superscript characters by nature, so
    # "o" and "a" are used instead. This is because ordinal() wraps
    # them in a <sup> tag.
    indicators <- switch(gender,
        masculine = c(
            "0" = "o",
            "1" = "o",
            "2" = "o",
            "3" = "o",
            "4" = "o",
            "5" = "o",
            "6" = "o",
            "7" = "o",
            "8" = "o",
            "9" = "o"
        ),
        feminine = c(
            "0" = "a",
            "1" = "a",
            "2" = "a",
            "3" = "a",
            "4" = "a",
            "5" = "a",
            "6" = "a",
            "7" = "a",
            "8" = "a",
            "9" = "a"
        )
    )

    return(list(indicators = indicators, exceptions = character()))
}
