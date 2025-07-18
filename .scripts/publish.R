#' Deploy to shinyapps.io Programmatically
#'
#' Bundle the application and deploy it to <https://shinyapps.io>.
#'
#' @details
#' [.pub()] also creates required static HTML files in www/static from
#' Markdown files before bundling the application.
#'
#' Three environment variables are required:
#'
#'   * `RSCONNECT_ACCOUNT_NAME`,
#'   * `RSCONNECT_ACCOUNT_TOKEN`, and
#'   * `RSCONNECT_ACCOUNT_SECRET`.
#'
#' Store them in an untracked top-level .Renviron file.
#'
#' @param region A character string. It must be equal to `"prod"`, or `"dev"`.
#'
#' @returns The output of [rsconnect::deployApp()].
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso
#' [rsconnect::deployApp()],
#' [rsconnect::setAccountInfo()]
#'
#' @examples
#' .pub()
#' .pub("dev")
#' .pub("prod")
.pub <- function(region = c("dev", "prod")) {
    region <- match.arg(region)
    path_dir_assets <- getOption("app_path_dir_assets")
    region_meta <- switch(region,
        dev  = getOption("app_shinyapps_meta_dev"),
        prod = getOption("app_shinyapps_meta_prod")
    )

    cat("Setting account info from environment variables.", sep = "\n")

    rsconnect::setAccountInfo(
        server = "shinyapps.io",
        name   = Sys.getenv("RSCONNECT_ACCOUNT_NAME"),
        token  = Sys.getenv("RSCONNECT_ACCOUNT_TOKEN"),
        secret = Sys.getenv("RSCONNECT_ACCOUNT_SECRET")
    )

    cat("Generating HTML files from source Markdown files.", sep = "\n")

    # Local function that encapsulates common rmarkdown parameters.
    # File paths must be relative to the input of rmarkdown::render().
    html_document <- \(title = "", ...) {
        return(
            rmarkdown::html_document(
                ...,
                toc         = TRUE,
                toc_float   = list(collapsed = TRUE, smooth_scroll = FALSE),
                mathjax     = NULL,
                theme       = bslib::bs_theme(5L, "shiny"),
                css         = file.path(path_dir_assets, "_main.css"),
                pandoc_args = c("--metadata", sprintf("title=%s", title)),
                includes    = rmarkdown::includes(
                    in_header = file.path(path_dir_assets, "_head.html")
                )
            )
        )
    }

    # Generate www/assets/news.html from NEWS.md.
    # File paths must be relative to the input.
    rmarkdown::render(
        input         = "NEWS.md",
        runtime       = "static",
        quiet         = TRUE,
        output_file   = file.path(path_dir_assets, "news.html"),
        output_format = html_document("Expostats - Tool 1 Changelog")
    )

    # Generate www/assets/translations.html from TRANSLATIONS.md.
    rmarkdown::render(
        input         = "TRANSLATIONS.md",
        runtime       = "static",
        quiet         = TRUE,
        output_file   = file.path(path_dir_assets, "translations.html"),
        output_format = html_document("Expostats - Tool 1 Translations")
    )

    cat(sprintf("Deploying app to the '%s' region.", region), sep = "\n")

    return(
        invisible(
            rsconnect::deployApp(
                account        = Sys.getenv("RSCONNECT_ACCOUNT_NAME"),
                appId          = region_meta$id,
                appName        = region_meta$meta,
                appTitle       = "Tool1: Data Interpretation for One Similar Exposure Group (SEG)",
                appMode        = "shiny",
                appVisibility  = "public",
                logLevel       = "normal",
                launch.browser = FALSE,
                lint           = FALSE,
                forceUpdate    = FALSE
            )
        )
    )
}
