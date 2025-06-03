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

# Set the default source language.
transltr::language_source_set("en")

options(
    shiny.autoload.r       = TRUE,
    transltr.path          = file.path("intl", "_translator.yml"),
    transltr.verbose       = TRUE,
    warnPartialMatchArgs   = TRUE,
    warnPartialMatchDollar = TRUE,
    warnPartialMatchAttr   = TRUE
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
