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


# Scripts ----------------------------------------------------------------------


source("scripts/SEG/Data formatting functions_SEG.R")
source("scripts/Common/Simple censored imputation functions.R")
source("scripts/Common/Descriptive numerical output functions.R")
source("scripts/Common/Descriptive graphs functions.R")
source("scripts/Common/Bayesian engine functions.R")
source("scripts/Common/Numerical output functions.R")
source("scripts/Common/Main graph functions.R")
