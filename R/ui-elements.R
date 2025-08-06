#' Generate Static UI Elements
#'
#' These are helper functions used to standardize some aspects of the user
#' interface (and avoid code repetition).
#'
#' @param href A character string. A Uniform Resource Locator (URL).
#'
#' @param color A character string equal to `"primary"`, `"secondary"`,
#'   `"success"`, `"danger"`, `"warning"`, `"info"`, `"light"`, `"dark"`,
#'   or `"emphasis"`. The name of the Bootstrap color to use.
#'
#' @param emails A character vector.
#'
#' @param ... Further tag attributes (named arguments) and children (unnamed
#'   arguments) passed to various tag functions of package htmltools.
#'
#' @returns
#' [ui_link()],
#' [ui_link_mailto()], and
#' [ui_menu_icon()] return a `shiny.tag` object.
#'
#' [ui_bs_color()] returns a character string of class `BootstrapColorName`
#' which is equal to `color` if it is valid.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso
#' [Bootstrap 5 Colors](https://getbootstrap.com/docs/5.3/customize/color/)
#'
#' @rdname ui-helpers
#' @export
ui_link <- function(href = "", ..., color = "primary") {
    color <- ui_bs_color(color)
    return(
        htmltools::a(
            .noWS  = c("before", "after", "outside", "after-begin", "before-end"),
            class  = sprintf("link-%s link-offset-2 link-underline-opacity-25 link-underline-opacity-100-hover", color),
            href   = href,
            target = "_blank",
            ...
        )
    )
}

#' @rdname ui-helpers
#' @export
ui_link_mailto <- function(emails = character(), ..., color = "primary") {
    color <- ui_bs_color(color)
    return(
        htmltools::span(
            # Mailto Link (<a> tag).
            ui_link(
                color = color,
                sprintf("mailto:%s", paste0(emails, collapse = ",")),
                ...
            ),

            # Envelope icon (inserted after link in a <span> tag).
            htmltools::span(
                .noWS = c("before", "after", "outside", "after-begin", "before-end"),
                class = sprintf("text-%s ps-2", color),
                bsicons::bs_icon("envelope", a11y = "deco")
            )
        )
    )
}

#' @rdname ui-helpers
#' @export
ui_bs_color <- function(
    color = c(
        "primary",
        "secondary",
        "success",
        "danger",
        "warning",
        "info",
        "light",
        "dark",
        "emphasis"
    ))
{
    return(structure(match.arg(color), class = "BootstrapColorName"))
}

#' @rdname ui-helpers
#' @export
ui_menu_icon <- function() {
    return(
        tags$span(
            class = "pe-1",
            bsicons::bs_icon(name = "list", a11y = "deco")
        )
    )
}
