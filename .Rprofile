#' R Session's Entry Point
#'
#' This is automatically executed by R whenever a new session is started.
#'
#' @note
#' [interactive()] is used to prevent this script from being executed in remote
#' non-interactive instances (such as shinyapps.io). Options below are always
#' set no matter the underlying environment.
#'
#' @seealso
#' [Startup process](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html)
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

# Set the default source language.
transltr::language_source_set("en")

# Set global options.
# Options specific to Tool 1 are prefixed by app_.
options(
    app_version                 = "5.3.0-rc1",
    app_release_date            = "2025-07-18",
    app_shinyapps_meta_dev      = list(id = 14521166L, name = "tool1-beta"),
    app_shinyapps_meta_prod     = list(id = 13889847L, name = "tool1"),
    app_card_height_md          = "600px",
    app_card_height_sm          = "425px",
    app_card_height_xs          = "300px",
    app_path_dir_assets         = file.path("www", "assets"),
    app_path_dir_images         = file.path("www", "assets", "images"),
    app_missing_translation_msg = "{no translation}",
    app_n_signif_digits         = 3L,
    app_number_bayes_iter       = 25000L,
    app_express_oel_multiplier  = 1,
    app_express_conf            = 90,
    app_express_psi             = 30,
    app_express_frac_threshold  = 5,
    app_express_target_perc     = 95,
    shiny.autoload.r            = TRUE,
    transltr.path               = file.path("i18n", "_translator.yml"),
    transltr.verbose            = TRUE,
    warnPartialMatchArgs        = TRUE,
    warnPartialMatchDollar      = TRUE,
    warnPartialMatchAttr        = TRUE
)

# Development Tools ------------------------------------------------------------

if (interactive()) {
    cat("R session is interactive. Attaching development tools.\n")

    # Attach development packages.
    suppressMessages({
        require(microbenchmark)
    })

    # Attach aliases and small dev tools.
    # Names are as small as possible by design.
    attach(name = "tools:dev", what = local({
        # Define aliases.
        .mb <- microbenchmark::microbenchmark

        # Clear the global environment.
        .rm <- \() rm(list = ls(name = globalenv()), pos = globalenv())

        # Source everything stored in
        # R/ (in the global environment).
        .src <- \() invisible(lapply(list.files("R", full.names = TRUE), source))
        .src()

        # Source development functions stored in .scripts/.
        source(file.path(".scripts", "run.R"), TRUE)
        source(file.path(".scripts", "publish.R"), TRUE)
        source(file.path(".scripts", "find-text.R"), TRUE)

        # Force .pub() to source everything
        # in R/ before doing anything else.
        .pub_no_source <- .pub
        .pub <- \(...) {
            .src()
            .pub_no_source(...)
        }

        # Return this local environment
        # (to attach it to the search path).
        environment()
    }))
}
