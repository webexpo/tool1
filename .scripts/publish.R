#' Deploy to shinyapps.io Programmatically
#'
#' Bundle the application and deploy it to <https://shinyapps.io>.
#'
#' @details
#' [publish()] also creates required static HTML files in www/static from
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
#' @usage
#' ## In interactive sessions
#' .pub(...)
#'
#' ## In non-interactive session
#' publish(
#'   region       = c("dev", "prod"),
#'   account      = "lavoue",
#'   version      = default_version[["number"]],
#'   release_date = default_version[["release_date"]]
#' )
#'
#' @param region A character string. It must be equal to `"prod"`, or `"dev"`.
#'
#' @param account A non-empty and non-NA character string.
#'
#' @param version A non-empty and non-NA character string.
#'
#' @param release_date A non-empty and non-NA character string.
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
#' publish("dev")
#' publish("prod")
#'
#' @export
publish <- function(
    region       = c("dev", "prod"),
    account      = "lavoue",
    version      = default_version[["number"]],
    release_date = default_version[["release_date"]])
{
    region <- match.arg(region)

    stopifnot(exprs = {
        is_chr1(account)
        is_chr1(version)
        is_chr1(release_date)
    })

    cat(sprintf("Generating static HTML static files."), sep = "\n")

    # Local function that encapsulates common rmarkdown parameters.
    html_document <- \(title = "", ...) {
        return(
            rmarkdown::html_document(
                ...,
                toc         = TRUE,
                toc_float   = list(collapsed = TRUE, smooth_scroll = FALSE),
                mathjax     = NULL,
                theme       = bslib::bs_theme(5L, "shiny"),
                pandoc_args = c("--metadata", sprintf("title=%s", title))
            )
        )
    }

    # Generate www/static/news.html from NEWS.md.
    rmarkdown::render(
        input         = "NEWS.md",
        runtime       = "static",
        quiet         = TRUE,
        output_file   = file.path("www", "static", "news.html"),
        output_format = html_document(
            title = "Expostats - Tool 1 Changelog",
            css   = file.path("www", "static", "_static.css"),
        )
    )

    # Generate www/static/translations.html from intl/README.md.
    # File paths must be relative to the input.
    rmarkdown::render(
        input         = file.path("intl", "README.md"),
        runtime       = "static",
        quiet         = TRUE,
        output_file   = file.path("..", "www", "static", "translations.html"),
        output_format = html_document(
            title = "Expostats - Tool 1 Translations",
            css   = file.path("..", "www", "static", "_static.css"),
        )
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
}
