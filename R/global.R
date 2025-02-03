#' Constants and Dependencies
#'
#' Load required constants, libraries, aliases, and scripts.
#'
#' @details
#' This script is sourced automatically by [shiny::runApp()] before launching
#' the application. This function also sources all R scripts stored in `R/`.
#'
#' @note
#' Packages below must still be attached to the search path with [library()]
#' until all scripts stored in `scripts/` are revamped.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)


# Some libraries must be loaded because namespaces are not
# explicit in R scripts stored in subdirectory scripts/.
library(rjags)
library(ggplot2)
library(ggimage)


# External Scripts -------------------------------------------------------------


source(file.path("scripts", "SEG",    "Data formatting functions_SEG.R"))
source(file.path("scripts", "Common", "Simple censored imputation functions.R"))
source(file.path("scripts", "Common", "Descriptive numerical output functions.R"))
source(file.path("scripts", "Common", "Descriptive graphs functions.R"))
source(file.path("scripts", "Common", "Bayesian engine functions.R"))
source(file.path("scripts", "Common", "Numerical output functions.R"))
source(file.path("scripts", "Common", "Main graph functions.R"))


# Constants --------------------------------------------------------------------


# Where to store images.
images_dir <- file.path("www", "images")

# Current version in production (release). Shown in footer.
current_version <- "4.0.0-RC1"

# Current year. Shown in footer.
current_year <- format(Sys.time(), tz = "EST", format = "%Y")

# Default height of plots.
plot_height <- "600px"

# Default height of risk meters.
# Using a lower height for these specific plots is preferable.
plot_risk_meter_height <- "500px"

# Alias to shiny' list of functions used to create HTML tags.
html <- shiny::tags


# Internationalization ---------------------------------------------------------


# NOTE: (JMP) Temporary alias to be able to launch the application while
# integrating source text back into it.
# TODO: (JMP) Remove alias and replace calls by transltr::translate().
translate <- \(str) transltr:::normalize(str)


# Static UI elements -----------------------------------------------------------


static <- list(
    # <i> tags representing icons stored as shiny.tag objects.
    icons = list(
        bottom = shiny::icon(
            lib   = "glyphicon",
            name  = "triangle-bottom",
            style = "padding-right: 10px;"),
        top = shiny::icon(
            lib   = "glyphicon",
            name  = "triangle-top",
            style = "padding-right: 10px;")),
    # <a> tags stored as shiny.tag objects.
    a = list(
        aiha = shiny::tags$a(
            "AIHA",
            href   = "https://www.aiha.org",
            target = "_blank"),
        expostats_ndexpo = shiny::tags$a(
            "NDexpo",
            href   = "http://www.expostats.ca/site/app-local/NDExpo/",
            target = "_blank"),
        expostats_paper = shiny::tags$a(
            "https://doi.org/10.1093/annweh/wxy100",
            target = "_blank",
            href   = "https://doi.org/10.1093/annweh/wxy100"),
        expostats_info_fr = shiny::tags$a(
            "expostats.ca",
            target = "_blank",
            href   = "http://www.expostats.ca/site/info.html"),
        expostats_info_en = shiny::tags$a(
            "expostats.ca",
            target = "_blank",
            href   = "http://www.expostats.ca/site/en/info.html"),
        epsum_en = shiny::tags$a(
            "School of Public Health",
            href   = "https://espum.umontreal.ca/english/home",
            target = "_blank"),
        epsum_fr = shiny::tags$a(
            "École de santé publique",
            href   = "https://espum.umontreal.ca/accueil",
            target = "_blank"),
        udm_en = shiny::tags$a(
            "Université de Montréal",
            href   = "https://www.umontreal.ca/en",
            target = "_blank"),
        udm_fr = shiny::tags$a(
            "Université de Montréal",
            href   = "https://www.umontreal.ca",
            target = "_blank"),
        dennis_helsel = shiny::tags$a(
            "Dennis Helsel",
            href   = "http://www.practicalstats.com/info2use/books.html",
            target = "_blank"),
        source = shiny::tags$a(
            "GitHub",
            href   = "https://github.com/webexpo/app-tool1",
            target = "_blank"),
        jerome_lavoue = shiny::tags$a(
            "Jérôme Lavoué",
            href   = "https://orcid.org/0000-0003-4950-5475",
            target = "_blank"),
        ununoctium = shiny::tags$a(
            "Ununoctium",
            href   = "https://ununoctium.dev",
            target = "_blank")))

# List of <a> tags stored as character strings.
# These strings can be passed to sprintf_html().
a_strs <- vapply(static$a, as.character, NA_character_)
