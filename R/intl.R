#' Internationalization
#'
#' Import translations and define constants that are strictly used for
#' internationalization purposes.
#'
#' This script is sourced automatically by [shiny::runApp()].
#'
#' @note
#' This script expects existing files stored in `getOption("transltr.path")`.
#' See .Rprofile to change this location and .scripts/find-source-text.R to
#' create all expected translations files.
#'
#' @seealso
#' [The transltr package](https://cran.r-project.org/web/packages/transltr/index.html)
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

tr <- transltr::translator_read()

# Set default value when a translation is not available.
# This is mostly useful for development and debugging purposes.
tr$set_default_value("{no translation}")

# Get default language.
# $source_langs returns a single element here.
default_lang <- tr$source_langs

# List available languages.
# To pass supported_langs as is to shiny::selectInput(),
# names (language codes) and values (language names) must
# be inverted. The former displays names and returns values.
supported_langs <- names(tr$native_languages)
names(supported_langs) <- tr$native_languages

# Define a wrapper function that calls tr$translate().
# It avoids having to pass values such as input$lang to each
# call to tr$translate().
intl <- function(..., source_lang = default_lang) {
    return(tr$translate(..., lang = input$lang, source_lang = default_lang))
}
