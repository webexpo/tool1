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

# Create a new (empty) Translator object.
tr <- transltr::translator(id = "expostats:tool1")

# Register languages that must be supported.
# Each named element is interpreted as <code> = <displayed name>.
tr$set_native_languages(
    en = "English",
    fr = "Français"
)

# Extract source text to translate from source scripts
# and update the Translator object (by reference).
transltr::find_source(tr = tr, interface = quote(translate))

# The following steps are done in a local environment to avoid
# polluting the current environment with temporary variables.
local({
    # Get the directory holding translations.
    dir <- dirname(getOption("transltr.path"))

    # Extract non-source language codes that may
    # have an existing translation file already.
    langs <- names(tr$native_languages)
    langs <- langs[langs != transltr::language_source_get()]

    # Only keep files that exists.
    files <- file.path(dir, sprintf("%s.txt", langs))
    files <- files[utils::file_test("-f", files)]

    # Read existing translations and import them
    # back into the Translator object created above.
    # The latter is updated by reference.
    lapply(files, transltr::translations_read, tr = tr)
})

# Export source text and translations.
# This populates the contents of intl/.
transltr::translator_write(tr, overwrite = TRUE)
