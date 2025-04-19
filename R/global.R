#' Setup
#'
#' Load libraries, scripts and global constants.
#'
#' @note
#' For historical reasons, scripts stored in `scripts/` do not reference the
#' namespaces of the functions they call, and the packages they use must be
#' attached to the search path.
#'
#' This is considered to be a bad practice. The intent should always be clear
#' and consistent.
#'
#' ```
#' # Good
#' transltr::language_source_get()
#'
#' # Bad
#' language_source_get()
#' ```
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

# Libraries --------------------------------------------------------------------

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

# Translations -----------------------------------------------------------------

tr <- transltr::translator_read()

# Global Persistent Cache ------------------------------------------------------

# Default in-memory cache for truly static UI elements.
shiny::shinyOptions(
    cache = cachem::cache_mem(
        max_size = 100L * 1024L ^ 2L,  # Max size is 100MB.
        max_age  = Inf,                # Cache never expires (until new release).
        max_n    = Inf,                # Cache can store as many objects as needed.
        evict    = "lru",              # Replace Least Recently Used (LRU) objects.
        logfile  = NULL
    )
)

# Constants --------------------------------------------------------------------

# Default version/release to display in footers.
default_version <- c(number = "5.0.0", release_date = "2025-04-30")

# Default language.
default_lang <- transltr::language_source_get()

# Default height of cards.
default_card_height <- "600px"
default_card_height_text_only <- "425px"

# Default number of Bayesian iterations.
default_n_bayes_iter <- 25000L

# Default number of significant digits to keep.
default_n_digits <- 3L

# Default relative path to images' directory.
default_images_dir <- file.path("www", "images")

# Default message to show when
#   - there is no available translation, OR
#   - the underlying source text has not yet been registered by object tr
#     using transltr::find_source(). Call .find() whenever you change the
#     text passed to translate().
tr$set_default_value(default_missing_translation_msg <- "{no translation}")

# Default maintainers' emails.
default_maintainers_emails <- c(
    jerome_lavoue = "jerome.lavoue@umontreal.ca",
    ununoctium    = "jeanmathieupotvin@ununoctium.dev"
)

# Default URLs to various resources.
default_urls <- list(
    news            = "https://github.com/webexpo/tool1/blob/main/NEWS.md",
    code            = "https://github.com/webexpo/tool1",
    aiha            = "https://www.aiha.org",
    ununoctium      = "https://ununoctium.dev",
    dennis_helsel   = "https://www.practicalstats.com/info2use/books.html",
    jerome_lavoue   = "https://orcid.org/0000-0003-4950-5475",
    nist_j032       = "https://www.nist.gov/system/files/documents/2023/09/26/J-032%20Writing%20with%20the%20SI.pdf",
    ndexpo          = "https://www.expostats.ca/site/app-local/NDExpo",
    expostats_paper = "https://doi.org/10.1093/annweh/wxy100",
    expostats = c(
        en = "http://www.expostats.ca/site/en/info.html",
        fr = "https://www.expostats.ca/site/info.html"
    ),
    tool2 = c(
        en = "https://lavoue.shinyapps.io/Tool2v3En/",
        fr = "https://lavoue.shinyapps.io/Tool2v3Fr/"
    ),
    tool3 = c(
        en = "https://lavoue.shinyapps.io/Tool3v3En/",
        fr = "https://lavoue.shinyapps.io/Tool3v3Fr/"
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
