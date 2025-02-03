#' Deploy to shinyapps.io Programmatically
#'
#' Use this script to bundle the application and deploy it to
#' <https://shinyapps.io>.
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
    metadata       = list(current_version = current_version),
    forceUpdate    = FALSE)
