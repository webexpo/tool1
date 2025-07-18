#' Miscellaneous Actions
#'
#' Perform actions (as part of Tool 1's initialization process) once all other
#' R scripts stored in R/ are loaded.
#'
#' @format
#' `urls` is an environment containing `i18n_url` objects created with
#' [i18n_url()] and treated as a global constant. The URLs it contains
#' are defined once (here) and used multiple times.
#'
#' `tr` is a [transltr::Translator] object holding all available translations
#' (including the source text itself). It is a representation of the contents
#' of i18n/.
#'
#' `translate` is an alias defined for convenience and performance purposes. It
#' avoids a massive number of calls to `$`.
#'
#' `tags` is an alias for defined for convenience.
#'
#' @note
#' This script is used when the logic requires other R objects.
#'
#' @seealso [transltr::transltr]
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname zzz
#' @export
urls <- local({
    code          <- i18n_url("https://github.com/webexpo/tool1")
    ununoctium    <- i18n_url("https://ununoctium.dev")
    jerome_lavoue <- i18n_url("https://orcid.org/0000-0003-4950-5475")
    aiha_videos   <- i18n_url("https://www.aiha.org/education/elearning/online-courses/making-accurate-exposure-risk-decisions")
    ndexpo        <- i18n_url("https://www.expostats.ca/site/app-local/NDExpo")
    expostats = i18n_url(
        "https://www.expostats.ca/site/en/info.html",
        fr = "https://www.expostats.ca/site/info.html"
    )

    return(environment())
})

#' @rdname zzz
#' @export
tr <- transltr::translator_read()
tr$set_default_value(getOption("app_missing_translation_msg"))

#' @rdname zzz
#' @export
translate <- tr$translate

#' @rdname zzz
#' @export
tags <- htmltools::tags
