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
#' @author Jérôme Lavoué (<jerome.lavoue@@umontreal.ca>)
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)


library(rjags)
library(ggplot2)
library(ggimage)


# Constants --------------------------------------------------------------------


# Where to store images.
images_dir <- file.path("www", "images")

# Standard width for all inputs.
input_width <- "110px"

# Alias to htmltools' list containing
# functions used to create HTML5 tags.
html <- htmltools::tags


# Internationalization ---------------------------------------------------------


# NOTE: (JMP) Temporary alias to be able to launch the application while
# integrating source text back into it.
# TODO: (JMP) Remove alias and replace calls by transltr::translate().
translate <- \(str) transltr:::normalize(str)


# Scripts ----------------------------------------------------------------------


source(file.path("scripts", "SEG",    "Data formatting functions_SEG.R"))
source(file.path("scripts", "Common", "Simple censored imputation functions.R"))
source(file.path("scripts", "Common", "Descriptive numerical output functions.R"))
source(file.path("scripts", "Common", "Descriptive graphs functions.R"))
source(file.path("scripts", "Common", "Bayesian engine functions.R"))
source(file.path("scripts", "Common", "Numerical output functions.R"))
source(file.path("scripts", "Common", "Main graph functions.R"))
