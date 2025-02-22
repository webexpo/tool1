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

tags <- shiny::tags

# Current version in production (release). Shown in footer.
version <- "4.0.0"

# Current year. Shown in footer.
year <- format(Sys.time(), tz = "EST", format = "(%Y)")

# Where to store images.
images_dir_rel_path <- file.path("www", "images")

# Default height of plots.
plot_default_height <- "600px"

# Default height of risk meters.
# Using a lower height for these specific plots is preferable.
plot_risk_meter_default_height <- "500px"

# Internationalization ---------------------------------------------------------

tr <- transltr::translator_read()

# Default value when a translation is not available.
tr$set_default_value("{no translation}")

# List available languages.
# Names are language codes and values are native language names.
# This matches what shiny::selectInput() expects (langs can be
# passed as is to argument choices).
supported_langs <- structure(
    names(tr$native_languages),
    names = tr$native_languages)

# Default language.
# $source_langs returns a single element here.
default_lang <- tr$source_langs

# Uniform Resource Locators ----------------------------------------------------

# Language codes used below must match SUPPORTED_LANGS.
urls <- list(
    code             = "https://github.com/webexpo/tool1",
    aiha             = "https://www.aiha.org",
    dennis_helsel    = "https://www.practicalstats.com/info2use/books.html",
    jerome_lavoue    = "https://orcid.org/0000-0003-4950-5475",
    expostats_ndexpo = "https://www.expostats.ca/site/app-local/NDExpo",
    expostats_paper  = "https://doi.org/10.1093/annweh/wxy100",
    expostats = c(
        en = "http://www.expostats.ca/site/en/info.html",
        fr = "https://www.expostats.ca/site/info.html"
    ),
    epsum = c(
        en = "https://espum.umontreal.ca/english/home",
        fr = "https://espum.umontreal.ca/accueil"
    ),
    udm = c(
        en = "https://www.umontreal.ca/en",
        fr = "https://www.umontreal.ca"
    )
)
