#' Setup
#'
#' Load libraries, scripts and global constants.
#'
#' @note
#' Since scripts stored in `scripts/` do not include namespace references,
#' packages below must be (explicitly) attached to the search path until
#' they are all revamped.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

# Load libraries required by external scripts stored in scripts/.
# rjags returns internal warnings over which we have no control.
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

# Internationalization ---------------------------------------------------------

intl_tr <- transltr::translator_read()

# Default language.
intl_default_lang <- transltr::language_source_get()

# Message to show when there is no available translation.
intl_missing_msg <- "{no translation}"
intl_tr$set_default_value(intl_missing_msg)

# Constants --------------------------------------------------------------------

# Shortcut to usual Shiny's list of HTML <tag> functions.
tags <- htmltools::tags

# Current version in production (release). Shown in footer.
version_number <- "4.0.0-rc2"
version_date <- "2025-02-28"

# Current year. Shown in footer.
year <- format(Sys.time(), tz = "EST", format = "%Y")

# Where to store images.
images_dir_rel_path <- file.path("www", "images")

# Default height of plots.
plot_default_height <- "600px"

# Default height of risk meters.
# Using a lower height for these specific plots is preferable.
plot_risk_meter_default_height <- "500px"

# Default number of Bayesian iterations.
n_bayes_iter <- 25000L

# Default number of significant digits to keep.
n_digits <- 3L

# Default URLs to various resources.
# Language codes used below must match entries of tr$native_languages.
urls <- list(
    code             = "https://github.com/webexpo/tool1",
    aiha             = "https://www.aiha.org",
    dennis_helsel    = "https://www.practicalstats.com/info2use/books.html",
    jerome_lavoue    = "https://orcid.org/0000-0003-4950-5475",
    expostats_ndexpo = "https://www.expostats.ca/site/app-local/NDExpo",
    expostats_paper  = "https://doi.org/10.1093/annweh/wxy100",
    expostats = c(
        en = "http://www.expostats.ca/site/en/info.html",
        fr = "https://www.expostats.ca/site/info.html"),
    epsum = c(
        en = "https://espum.umontreal.ca/english/home",
        fr = "https://espum.umontreal.ca/accueil"),
    udm = c(
        en = "https://www.umontreal.ca/en",
        fr = "https://www.umontreal.ca"))
