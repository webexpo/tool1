#' Find Text That Requires Translations
#'
#' Prepare translations files and manage translations. This project uses package
#' [transltr] for internationalization purposes.
#'
#' This script should be ran whenever at least one source text wrapped in a
#' call to [translate()] changes.
#'
#' @note
#' [transltr] does not yet have a robust mechanism to easily update existing
#' translations. This should be done by Jean-Mathieu Potvin only until further
#' notice. Argument `overwrite` below is explicitly set to `FALSE` to avoid
#' unintended consequences.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

transltr::language_source_set("en")

tr <- transltr::find_source(
    id = "tool1",

    # Source text is identifiable via its underlying SHA-1 hash.
    algorithm = "sha1",

    # Allow transltr to find any call to translate() function(s),
    # not only those to transltr::translate(). This is necessary
    # because we use a wrapper function (see R/translate.R).
    strict = FALSE,

    # Register languages that must be supported by the application.
    native_languages = c(
        en = "English"
    )
)

# Write translations files.
transltr::translator_write(tr, overwrite = FALSE)
