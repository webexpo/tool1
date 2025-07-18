#' AIHA Risk Levels
#'
#' Constants used to describe the risk levels of AIHA. Each level has a name,
#' a threshold, a color (a Bootstrap 5 standard color), and an icon (a
#' Bootstrap icon).
#'
#' Risk levels are encoded as integers. Level 1 is the lowest one and level 3
#' is (currently) the highest one.
#'
#' For convenience, names (`acceptable`, `tolerable`, and `problematic`) can be
#' used in the source text without having to call [get_risk_level_info()].
#'
#' @param level An optional integer value equal to 1, 2, or 3. It is mandatory
#'   for [get_risk_level_info()].
#'
#' @template param-lang-str
#'
#' @returns
#' [get_risk_level_colors()] returns a character vector if `level` is missing
#' and a character string otherwise.
#'
#' [get_risk_level_thresholds()] returns an integer vector if `level` is missing
#' and a single integer otherwise.
#'
#' [get_risk_level_info()] returns a named list containing the following
#' elements:
#'
#' \describe{
#'   \item{level}{An integer value. The value passed to `level`.}
#'   \item{name}{A character string. The name of the `level`.}
#'   \item{name_caps}{A character string. The *capitalized* version of `name`.}
#'   \item{threshold}{An integer value. The upper threshold of the `level`.}
#'   \item{color}{A character string. The displayed color for the `level`. This
#'     must be the name of a Bootstrap 5 color.
#'   }
#'   \item{icon}{A character string of class `html` containing an `<svg>`
#'     HTML tag. The icon to display for the `level`.
#'   }
#' }
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso
#' [Bootstrap 5 Colors](https://getbootstrap.com/docs/5.3/customize/color/)
#'
#' @rdname risk-levels
#' @export
get_risk_level_colors <- function(level) {
    colors <- c(
        "success",  # Level 1.
        "warning",  # Level 2.
        "danger"    # Level 3.
    )

    return(if (missing(level)) colors else colors[[level]])
}

#' @rdname risk-levels
#' @export
get_risk_level_thresholds <- function(level) {
    thresholds <- c(
        0L,  # Level 1: [0, 5L).
        5L,  # Level 2: [5L, 30L).
        30L  # Level 3: [30L, ∞).
    )

    return(if (missing(level)) thresholds else thresholds[[level]])
}

#' @rdname risk-levels
#' @export
get_risk_level_info <- function(level = 1L, lang = "en") {
    stopifnot(exprs = {
        is_int1(level)
        level > 0L
        level < 4L
    })

    color <- get_risk_level_colors(level)
    threshold <- get_risk_level_thresholds(level)

    info <- switch(level,
        list(
            level     = 1L,
            name      = translate(lang = lang, "acceptable"),
            name_caps = translate(lang = lang, "Acceptable"),
            threshold = threshold,
            color     = color,
            icon      = bsicons::bs_icon("check-circle-fill")
        ),
        list(
            level     = 2L,
            name      = translate(lang = lang, "tolerable"),
            name_caps = translate(lang = lang, "Tolerable"),
            threshold = threshold,
            color     = color,
            icon      = bsicons::bs_icon("exclamation-triangle-fill")
        ),
        list(
            level     = 3L,
            name      = translate(lang = lang, "problematic"),
            name_caps = translate(lang = lang, "Problematic"),
            threshold = threshold,
            color     = color,
            icon      = bsicons::bs_icon("exclamation-octagon-fill")
        )
    )

    attr(info$icon, "browsable_html") <- FALSE
    return(info)
}
