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
#' @param account A non-empty and non-NA character string.
#'
#' @param version A non-empty and non-NA character string.
#'
#' @param release_date A non-empty and non-NA character string.
#'
#' @param assets_dir A non-empty and non-NA character string. The
#'   location of further static assets such as HTML documents.
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
.pub <- function(
    region       = c("dev", "prod"),
    account      = "lavoue",
    version      = default_version[["number"]],
    release_date = default_version[["release_date"]],
    assets_dir   = default_assets_dir)
{
    region <- match.arg(region)

    stopifnot(exprs = {
        is_chr1(account)
        is_chr1(version)
        is_chr1(release_date)
    })

    cat(sprintf("Generating HTML files from source Markdown files."), sep = "\n")

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
                css         = file.path(assets_dir, "_main.css"),
                pandoc_args = c("--metadata", sprintf("title=%s", title)),
                includes    = rmarkdown::includes(
                    in_header = file.path(assets_dir, "_head.html")
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
        output_file   = file.path(assets_dir, "news.html"),
        output_format = html_document("Expostats - Tool 1 Changelog")
    )

    # Generate www/assets/translations.html from TRANSLATIONS.md.
    rmarkdown::render(
        input         = "TRANSLATIONS.md",
        runtime       = "static",
        quiet         = TRUE,
        output_file   = file.path(assets_dir, "translations.html"),
        output_format = html_document("Expostats - Tool 1 Translations")
    )

    cat(sprintf("Deploying app to the '%s' region.", region), sep = "\n")

    rsconnect::setAccountInfo(
        server = "shinyapps.io",
        name   = Sys.getenv("RSCONNECT_ACCOUNT_NAME"),
        token  = Sys.getenv("RSCONNECT_ACCOUNT_TOKEN"),
        secret = Sys.getenv("RSCONNECT_ACCOUNT_SECRET")
    )

    # Determine the remote instance to replace.
    app_to_deploy <- switch(region,
        prod = c(id = 13889847L, name = "tool1"),
        dev  = c(id = 14521166L, name = "tool1-beta")
    )

    return(
        invisible(
            rsconnect::deployApp(
                account        = account,
                appId          = app_to_deploy[["id"]],
                appName        = app_to_deploy[["name"]],
                appTitle       = "Tool1: Data Interpretation for One Similar Exposure Group (SEG)",
                appMode        = "shiny",
                appVisibility  = "public",
                logLevel       = "normal",
                launch.browser = FALSE,
                lint           = FALSE,
                forceUpdate    = FALSE,
                metadata       = list(
                    region               = region,
                    version_number       = version,
                    version_release_date = release_date,
                    license              = "MIT + file LICENSE",
                    bug_reports          = "https://github.com/webexpo/app-tool1/issues",
                    encoding             = "UTF-8",
                    language             = "en"
                )
            )
        )
    )
}
