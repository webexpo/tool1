#' Global Setup
#'
#' Load libraries and scripts. Define options scoped to the application (not
#' to the underlying process / R session).
#'
#' @note
#' Shiny sources R files stored in R/ automatically (in alphabetical order).
#' R/global.R is special and always sourced before any other file.
#'
#' @seealso
#' [shiny::loadSupport()]
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

shiny::shinyOptions(
    # Default in-memory cache for truly static UI elements.
    cache = cachem::cache_mem(
        max_size = 100L * 1024L ^ 2L,  # Max size is 100MB.
        max_age  = Inf,                # Cache never expires (until new release).
        max_n    = Inf,                # Cache can store as many objects as needed.
        evict    = "lru",              # Replace Least Recently Used (LRU) objects.
        logfile  = NULL
    )
)

# Libraries --------------------------------------------------------------------

suppressMessages({
    suppressWarnings(library(rjags)) # rjags returns partial matching warnings.
    library(randtoolbox)
    library(ggplot2)
    library(ggimage)
})

# Scripts ----------------------------------------------------------------------

source(file.path("scripts", "SEG",    "Data formatting functions_SEG.R"))
source(file.path("scripts", "Common", "Simple censored imputation functions.R"))
source(file.path("scripts", "Common", "Descriptive numerical output functions.R"))
source(file.path("scripts", "Common", "Descriptive graphs functions.R"))
source(file.path("scripts", "Common", "Bayesian engine functions.R"))
source(file.path("scripts", "Common", "Numerical output functions.R"))
source(file.path("scripts", "Common", "Main graph functions.R"))
