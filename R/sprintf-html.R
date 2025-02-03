#' Format raw HTML strings
#'
#' [sprintf_html()] is a wrapper function that formats a string using
#' [sprintf()] and marks it as valid HTML (that should not be escaped
#' later by package [`shiny`][shiny::shiny]).
#'
#' @param fmt,... Passed as is to [sprintf()].
#'
#' @returns
#' A character vector of length 1 and of class `html`. It can be used as
#' if it were a regular HTML tag inside other Shiny tags, or components.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @examples
#' link <- shiny::tags$a(
#'   "personal website",
#'   href   = "https://ununoctium.dev",
#'   target = "_blank")
#'
#' sprintf_html("This is my %s.", as.character(link))
#'
#' @export
sprintf_html <- function(fmt, ...) {
    return(htmltools::HTML(sprintf(fmt, ...)))
}
