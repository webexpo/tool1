#' Find Text That Requires Translations
#'
#' Prepare translations files and manage translations. This script creates
#' and updates the contents of directory intl/.
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

# Create a new Translator object.
# Source text is identifiable via SHA-1 hashes.
tr <- transltr::translator(id = "expostats:tool1", algorithm = "sha1")

# Register languages that must be supported (including the source language).
tr$set_native_languages(
    en = "English",
    fr = "Français")

# Extract source text to translate from source scripts.
transltr::find_source(tr = tr, interface = quote(intl))

# Export translations.
# They are imported whenever the application is launched. See R/global.R.
transltr::translator_write(tr, overwrite = TRUE)
