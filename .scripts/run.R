#' Run the Application Locally and Programmatically
#'
#' Use this script to launch the application locally for development
#' purposes.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

shiny::runApp(
    # You may use any other port value (aside from standard unsafe ports).
    port           = 3001L,
    launch.browser = FALSE)
