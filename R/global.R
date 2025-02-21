#' Application Setup
#'
#' Load libraries, scripts, and constants.
#'
#' @details
#' This script is sourced automatically by [shiny::runApp()].
#'
#' @note
#' Packages below must be attached to the search path with [library()] until
#' all scripts stored in `scripts/` are revamped (they do not include reference
#' to namespaces).
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

# Load libraries required by external scripts.
# rjags returns annoying internal warnings over
# which we have no control.
suppressMessages({
    suppressWarnings(library(rjags))
    library(randtoolbox)
    library(ggplot2)
    library(ggimage)
})

# External Scripts -------------------------------------------------------------

source(file.path("scripts", "SEG",    "Data formatting functions_SEG.R"))
source(file.path("scripts", "Common", "Simple censored imputation functions.R"))
source(file.path("scripts", "Common", "Descriptive numerical output functions.R"))
source(file.path("scripts", "Common", "Descriptive graphs functions.R"))
source(file.path("scripts", "Common", "Bayesian engine functions.R"))
source(file.path("scripts", "Common", "Numerical output functions.R"))
source(file.path("scripts", "Common", "Main graph functions.R"))

# Constants --------------------------------------------------------------------

TAGS <- shiny::tags

# Current version in production (release). Shown in footer.
VERSION <- "4.0.0"

# Current year. Shown in footer.
YEAR <- format(Sys.time(), tz = "EST", format = "(%Y)")

# Where to store images.
IMAGES_DIR_REL_PATH <- file.path("www", "images")

# Default height of plots.
PLOT_DEFAULT_HEIGHT <- "600px"

# Default height of risk meters.
# Using a lower height for these specific plots is preferable.
PLOT_RISK_METER_DEFAULT_HEIGHT <- "500px"

# Internationalization ---------------------------------------------------------

TR <- transltr::translator_read()

# Default value when a translation is not available.
TR$set_default_value("{no translation}")

# List available languages.
# Names are language codes and values are native language names.
# This matches what shiny::selectInput() expects (langs can be
# passed as is to argument choices).
SUPPORTED_LANGS <- structure(
    names(TR$native_languages),
    names = TR$native_languages)

# Default language.
# $source_langs returns a single element here.
DEFAULT_LANG <- TR$source_langs
