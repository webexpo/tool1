#' Find Text That Requires Translations
#'
#' Prepare translations files and manage translations.
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

transltr::language_source_set("en")

# Create a new Translator object.
# Source text is identifiable via SHA-1 hashes.
tr <- transltr::translator(id = "expostats:tool1", algorithm = "sha1")

# Register languages that must be supported (including the source language).
tr$set_native_languages(
    en = "English",
    fr = "Français")

# Extract source text to translate from source scripts.
transltr::find_source(tr = tr, interface = quote(translate))

# Export translations.
# They are imported whenever the application is launched. See R/global.R.
transltr::translator_write(tr, overwrite = TRUE)
