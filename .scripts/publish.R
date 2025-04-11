#' Deploy to shinyapps.io Programmatically
#'
#' Bundle the application and deploy it to <https://shinyapps.io>.
#'
#' @usage
#' ## In interactive sessions
#' .pub()
#'
#' Three environment variables are required (see below). Store them in an
#' untracked top-level .Renviron file.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

rsconnect::setAccountInfo(
    server = "shinyapps.io",
    name   = Sys.getenv("RSCONNECT_ACCOUNT_NAME"),
    token  = Sys.getenv("RSCONNECT_ACCOUNT_TOKEN"),
    secret = Sys.getenv("RSCONNECT_ACCOUNT_SECRET"))

rsconnect::deployApp(
    appId          = 13889847L,
    appName        = "tool1",
    appTitle       = "Tool1: Data Interpretation for One Similar Exposure Group (SEG)",
    appMode        = "shiny",
    appVisibility  = "public",
    logLevel       = "verbose",
    launch.browser = FALSE,
    lint           = FALSE,
    forceUpdate    = FALSE,
    metadata       = list(
        version_number       = default_version[["number"]],
        version_release_date = default_version[["release_date"]],
        license              = "MIT + file LICENSE",
        bug_reports          = "https://github.com/webexpo/app-tool1/issues",
        encoding             = "UTF-8",
        language             = "en"
    )
)
