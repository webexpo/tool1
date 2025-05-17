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
        # Aliases.
        .mb <- microbenchmark::microbenchmark

        # Shortcuts to run development scripts.
        .find <- \() invisible(source(file.path(".scripts", "find-text.R")))
        .run  <- \() invisible(source(file.path(".scripts", "run.R")))
        .pub  <- \(...) {
            .src()
            source(file.path(".scripts", "publish.R"), TRUE)
            return(invisible(publish(...)))
        }

        # Clear the global environment.
        .rm <- \() rm(list = ls(name = globalenv()), pos = globalenv())

        # Source everything in R/.
        .src <- \() invisible(lapply(list.files("R", full.names = TRUE), source))
        .src()

        # Return this local environment
        # (to attach it to the search path).
        environment()
    }))
}
