#' Translate Source Text at Runtime
#'
#' [translate()] wraps [`transltr::Translator$translate()`][transltr::Translator].
#'
#' It avoids having to explicitly pass object `tr`, and value `input$lang`
#' whenever it is called (like [transltr::translate()] would require).
#'
#' @param ... Passed as is to method
#'   [`transltr::Translator$translate()`][transltr::Translator]. Any number of
#'   literal character vectors. The source text to  translate. Values can be
#'   empty and/or NA, but this may lead to unexpected results.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @examples
#' translate("
#'   Package transltr is quite flexible when it comes to how
#'   source text can be formatted. It natively handles multi-
#'   line litteral character strings for convenience. This
#'   makes it much easier to use compared to other alternatives.")
#'
#' @export
translate <- function(...) {
    # FIXME: (JMP) Temporary workaround for the bugs in transltr.
    trans <- tr$translate(..., lang = "en", concat = " ")
    return(if (is.null(trans)) transltr:::normalize(trans) else trans)
}
