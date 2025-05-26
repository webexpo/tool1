#' Find Text That Requires Translations
#'
#' Prepare translations files. This creates and updates the contents of
#' directory intl/.
#'
#' @usage
#' ## In interactive sessions
#' .intl()
#'
#' @note
#' transltr does not yet have a robust mechanism to easily update existing
#' translations. This should be done by Jean-Mathieu Potvin until further
#' notice. Argument `overwrite` below is explicitly set to `FALSE` to avoid
#' unintended consequences.
#'
#' @seealso
#' [The transltr package](https://cran.r-project.org/web/packages/transltr/index.html)
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

# Use the existing Translator object, or create one if it does not exist.
tr <- if (utils::file_test("-f", getOption("transltr.path"))) {
    transltr::translator_read()
} else {
    transltr::translator(id = "expostats:tool1")
}

# Register languages that must be supported.
tr$set_native_languages(
    en = "English",
    fr = "Français"
)

# Extract source text to translate from source scripts.
transltr::find_source(tr = tr, interface = quote(translate))

# Export source text and translations.
# This populates the contents of intl/.
transltr::translator_write(tr, overwrite = TRUE)
