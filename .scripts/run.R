#' Local Entry Point
#'
#' Launch the application locally for development purposes.
#'
#' @param port An integer value passed to argument `port` of [shiny::runApp()].
#'
#' @param launch.browser An integer value passed to argument `launch.browser`
#'   of [shiny::runApp()].
#'
#' @param ... Further arguments passed to [shiny::runApp()].
#'
#' @returns The output of [shiny::runApp()].
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso
#' [shiny::runApp()]
#'
#' @examples
#' .run()
.run <- function(port = 3001L, launch.browser = FALSE, ...) {
    return(
        shiny::runApp(
            port           = 3001L,
            launch.browser = FALSE,
            ...
        )
    )
}
