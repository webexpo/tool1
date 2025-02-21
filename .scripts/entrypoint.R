#' Application's Entry Point
#'
#' Use this script to launch the application locally for development purposes.
#'
#' @usage
#' ## In interactive sessions
#' .src()
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

shiny::runApp(
    # You may use any other port value (aside from standard unsafe ports).
    port           = 3001L,
    launch.browser = FALSE)
