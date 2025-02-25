#' Deploy to shinyapps.io Programmatically
#'
#' Bundle the application and deploy it to <https://shinyapps.io>.
#'
#' Four environment variables are required. Store them in an untracked
#' top-level .Renviron file.
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
    appTitle       = "Tool1: Data Interpretation for One Similarly Exposed Group",
    appMode        = "shiny",
    appVisibility  = Sys.getenv("APP_VISIBILITY_ON_SHINYAPPS"),
    launch.browser = FALSE,
    logLevel       = "verbose",
    lint           = FALSE,
    forceUpdate    = FALSE,
    metadata       = list(
        appVersion = version,
        license    = "MIT + file LICENSE",
        bugReports = "https://github.com/webexpo/app-tool1/issues",
        encoding   = "UTF-8",
        language   = "en"
    )
)
