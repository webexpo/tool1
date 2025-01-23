#' Developer's entry point
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


# Global options ---------------------------------------------------------------


options(
    warnPartialMatchArgs   = TRUE,
    warnPartialMatchDollar = TRUE,
    warnPartialMatchAttr   = TRUE)


# Development tools and utility functions --------------------------------------


if (interactive()) {
    # Attach development packages.
    suppressMessages({
        require(microbenchmark)
    })

    # Attach aliases and small dev tools.
    # Names are as small as possible by design.
    attach(name = "tools:dev", what = local({
        # Shorter aliases.
        .mb <- microbenchmark::microbenchmark

        # Clear global environment.
        .rm <- \() rm(list = ls(name = globalenv()), pos = globalenv())

        # Return this local environment
        # (to attach it to the search path).
        environment()
    }))
}
