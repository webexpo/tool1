#' Setup
#'
#' Load libraries, scripts and global constants.
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

# Scripts ----------------------------------------------------------------------

source(file.path("scripts", "SEG",    "Data formatting functions_SEG.R"))
source(file.path("scripts", "Common", "Simple censored imputation functions.R"))
source(file.path("scripts", "Common", "Descriptive numerical output functions.R"))
source(file.path("scripts", "Common", "Descriptive graphs functions.R"))
source(file.path("scripts", "Common", "Bayesian engine functions.R"))
source(file.path("scripts", "Common", "Numerical output functions.R"))
source(file.path("scripts", "Common", "Main graph functions.R"))

# Translations -----------------------------------------------------------------

tr <- transltr::translator_read()

# Cache ------------------------------------------------------------------------

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
default_version <- c(number = "5.2.0", release_date = "2025-06-10")

# Default language.
default_lang <- transltr::language_source_get()

# Default language names.
default_lang_names <- tr$native_languages

# Default height of cards.
default_card_height <- "600px"
default_card_height_text_only <- "425px"

# Default number of Bayesian iterations.
default_n_bayes_iter <- 25000L

# Default number of significant digits to keep.
default_n_digits <- 3L

# Default internal values for inputs in express mode.
# Names must match names of inputs defined in the Sidebar Module.
default_express_inputs <- list(
    oel_multiplier = 1,
    conf           = 90,
    psi            = 30,
    frac_threshold = 5,
    target_perc    = 95
)

# Default relative path to images' directory.
default_images_dir <- file.path("www", "images")

# Default relative path to other static assets.
# These are non-standard assets that should not
# be stored directly in www/ (at top-level).
default_assets_dir <- file.path("www", "assets")

# Default message to show when
#  - there is no available translation, OR
#  - the underlying source text has not yet been registered by object tr
#    using transltr::find_source(). Call .find() whenever you change the
#    text passed to translate().
tr$set_default_value(default_missing_translation_msg <- "{no translation}")

# Default maintainers' emails.
default_maintainers_emails <- c(
    jerome_lavoue = "jerome.lavoue@umontreal.ca",
    ununoctium    = "jeanmathieupotvin@ununoctium.dev"
)

# URLs used more than once in the code.
# Always mark URLs as such with url().
# urls <- list(
#     code          = url("https://github.com/webexpo/tool1"),
#     ununoctium    = url("https://ununoctium.dev"),
#     jerome_lavoue = url("https://orcid.org/0000-0003-4950-5475"),
#     aiha_videos   = url("https://www.aiha.org/education/elearning/online-courses/making-accurate-exposure-risk-decisions"),
#     ndexpo        = url("https://www.expostats.ca/site/app-local/NDExpo"),
#     tool2 = url(
#         "https://lavoue.shinyapps.io/Tool2v3En/",
#         fr = "https://lavoue.shinyapps.io/Tool2v3Fr/"
#     ),
#     tool3 = url(
#         "https://lavoue.shinyapps.io/Tool3v3En/",
#         fr = "https://lavoue.shinyapps.io/Tool3v3Fr/"
#     ),
#     expostats = url(
#         "https://www.expostats.ca/site/en/info.html",
#         fr = "https://www.expostats.ca/site/info.html"
#     )
# )

# Standard AIHA discrete risk levels and related metadata.
# Names of levels and thresholds should never change.
aiha_risk_levels <- list(
    # Changing thresholds requires additional changes
    # in the risk_assessment() reactive value defined
    # in server_panel_simplified().
    thresholds = c(
        acceptable  = 0L,  # [0, 5L)
        tolerable   = 5L,  # [5L, 30)
        problematic = 30L  # [30L, ∞)
    ),
    # $get_text() is a way to prevent early evaluation of
    # lang(). The latter is a reactive value that can only
    # be called in a reactive context. It is written in a
    # way that works with transltr mechanisms: each source
    # text requiring translation must be a literal string
    # wrapped by translate().
    metadata = list(
        acceptable = list(
            level = "acceptable",
            color = "success",
            icon  = bsicons::bs_icon(
                name = "check-circle-fill",
                a11y = "deco"
            ),
            get_text = \(lang, capitalize = FALSE) {
                if (capitalize) {
                    return(translate(lang = lang, "Acceptable"))
                }

                return(translate(lang = lang, "acceptable"))
            }
        ),
        tolerable = list(
            level = "tolerable",
            color = "warning",
            icon  = bsicons::bs_icon(
                name = "exclamation-triangle-fill",
                a11y = "deco"
            ),
            get_text = \(lang, capitalize = FALSE) {
                if (capitalize) {
                    return(translate(lang = lang, "Tolerable"))
                }

                return(translate(lang = lang, "tolerable"))
            }
        ),
        problematic = list(
            level = "problematic",
            color = "danger",
            icon  = bsicons::bs_icon(
                name = "exclamation-octagon-fill",
                a11y = "deco"
            ),
            get_text = \(lang, capitalize = FALSE) {
                if (capitalize) {
                    return(translate(lang = lang, "Problematic"))
                }

                return(translate(lang = lang, "problematic"))
            }
        )
    )
)
