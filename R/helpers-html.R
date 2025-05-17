#' Create HTML Tags
#'
#' Alias to Shiny's usual list of HTML <tag> functions.
#'
#' @rdname helpers-html
#'
#' @export
tags <- htmltools::tags

#' Create HTML Content
#'
#' Embed HTML tags into source text.
#'
#' @param template A character string containing zero or more [sprintf()]
#'   placeholders. Use character placeholders (`%s`) for `shiny.tag` objects.
#'
#' @param ... Further arguments to be inserted into `template`.
#'
#' @param ignore A character string returned as is whenever `template` is
#'   identical to it.
#'
#' @returns A character string of class `html`.
#'
#' @examples
#' html("This is %s text.", shiny::span(style = "text-weight: bold;", "bold"))
#'
#' @seealso
#' [sprintf()],
#' [htmltools::HTML()]
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname helpers-html
#'
#' @export
html <- function(template = "", ..., ignore = default_missing_translation_msg) {
    text <- if (identical(template, ignore)) {
        ignore
    } else {
        do.call(sprintf, list(fmt = template, ...))
    }

    return(htmltools::HTML(text))
}
