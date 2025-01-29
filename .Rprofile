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
    warnPartialMatchArgs     = TRUE,
    warnPartialMatchDollar   = TRUE,
    warnPartialMatchAttr     = TRUE)


# Development tools and utility functions --------------------------------------


if (interactive()) {
    # Attach development packages.
    suppressMessages({
        require(microbenchmark)
    })

    # Load global constants.
    # This can be useful for debugging purposes.
    source(file.path("R", "global.R"))

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

    # shinyBS package requires external CSS and JS files
    # at runtime. Shiny expects these files to be stored
    # in www/sbs, and serves them as static assests under
    # /sbs.
    if (identical(Sys.getenv("INCLUDE_SHINYBS_FILES"), "true")) {
        require(shinyBS)

        # Create a temporary subdirectory.
        shinybs_dir <- file.path("www", "sbs")
        dir.create(shinybs_dir, FALSE, TRUE)

        # Register a callback function that removes the subdirectory
        # when the R process exits gracefully (with q()). Since it is
        # ignored by Git, it does not matter (much) if this function
        # fails to remove it.
        reg.finalizer(onexit = TRUE, e = globalenv(), f = \(e) {
            return(
                unlink(shinybs_dir,
                    recursive = TRUE,
                    force     = TRUE))
        })

        # Get absolute paths to required files.
        shinybs_files <- c(
            system.file(file.path("www", "shinyBS.js"),  package = "shinyBS"),
            system.file(file.path("www", "shinyBS.css"), package = "shinyBS"))

        # Copy required files in the temporary subdirectory.
        file.copy(
            from      = shinybs_files,
            to        = file.path(shinybs_dir, basename(shinybs_files)),
            overwrite = TRUE)
    }
}
