#' R Session's Entry Point
#'
#' This script is automatically executed by R whenever a new session is started.
#'
#' @note
#' [interactive()] is used to prevent this script from being executed in remote
#' non-interactive instances (such as shinyapps.io). Options below are always
#' set no matter the underlying environment.
#'
#' @seealso
#' [The .Rprofile file of package transltr](https://github.com/jeanmathieupotvin/transltr/blob/main/.Rprofile),
#' [Startup process](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html)

# Set default language.
# This also sets the default language at runtime.
transltr::language_source_set("en")

options(
    transltr.path          = file.path("intl", "_translator.yml"),
    transltr.verbose       = TRUE,
    warnPartialMatchArgs   = TRUE,
    warnPartialMatchDollar = TRUE,
    warnPartialMatchAttr   = TRUE)

# Development Tools and Utility Functions --------------------------------------

if (interactive()) {
    cat("R session is interactive. Loading development tools.", sep = "\n")

    # Attach development packages.
    suppressMessages({
        require(microbenchmark)
    })

    # Attach aliases and small dev tools.
    # Names are as small as possible by design.
    attach(name = "tools:dev", what = local({
        # Shorter aliases.
        .mb <- microbenchmark::microbenchmark

        # Source everything in R/.
        .src <- \() invisible(lapply(list.files("R", full.names = TRUE), source))
        .src()

        # Shortcuts to run development scripts.
        .run  <- \() invisible(source(file.path(".scripts", "entrypoint.R")))
        .pub  <- \() invisible(source(file.path(".scripts", "publish.R")))
        .intl <- \() invisible(source(file.path(".scripts", "internationalize.R")))

        # Clear the global environment.
        .rm <- \() rm(list = ls(name = globalenv()), pos = globalenv())

        # Return this local environment
        # (to attach it to the search path).
        environment()
    }))
}
