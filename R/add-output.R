#' Define Custom Shiny Outputs
#'
#' These functions wrap other `shiny::*Output()` functions and avoid code
#' repetition.
#'
#' @param A character string. The unique identifier for the output variable
#'   (to read the value from). It is passed as is to other `shiny::*Output()`
#'   functions.
#'
#' @param inline A logical value passed as is to other `shiny::*Output()`
#'   functions.
#'
#' @returns
#' [add_bold_text_output()] returns a single [`shiny.tag`][htmltools::tag]
#' object. It represents an HTML `<span>` element if `inline` is `TRUE` and
#' a `<div>` otherwise.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @examples
#' add_bold_text_output("my-plot")
#' add_bold_html_output("my-shiny-tag")
#'
#' @export
add_bold_text_output <- function(outputId = "", inline = TRUE) {
    return(
        htmltools::tagAppendAttributes(
            style = "font-weight: bold;",
            shiny::textOutput(outputId, inline = inline)))
}

#' @rdname add-output
#' @export
add_bold_html_output <- function(outputId = "", inline = TRUE) {
    return(
        htmltools::tagAppendAttributes(
            style = "font-weight: bold;",
            shiny::htmlOutput(outputId, inline = inline)))
}
