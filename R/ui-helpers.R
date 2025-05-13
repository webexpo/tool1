#' Generate Static UI Elements
#'
#' These are helper functions used to standardize some aspects of the user
#' interface and avoid code repetition.
#'
#' @param href A character string. A Uniform Resource Locator (URL).
#'
#' @param tags A list of tag functions used to construct the resulting
#'   UI element.
#'
#' @param emails A character vector.
#'
#' @param title A character string.
#'
#' @param icon_name A character string. It is passed to argument `name` of
#'   [bsicons::bs_icon()].
#'
#' @param icon_class A character string. It is passed to argument `class` of
#'   [bsicons::bs_icon()].
#'
#' @param ... Further tag attributes (named arguments) and children (unnamed
#'   arguments) passed to tag functions of package htmltools.
#'
#' @returns
#' A `shiny.tag` object.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-helpers
#' @export
ui_link <- function(href = "", ..., tags = htmltools::tags) {
    return(
        tags$a(
            class  = "icon-link icon-link-hover pe-1",
            href   = href,
            target = "_blank",
            ...,
            bsicons::bs_icon("arrow-right", a11y = "deco")
        )
    )
}

#' @rdname ui-helpers
#' @export
ui_link_mailto <- function(emails = character(), ..., tags = htmltools::tags) {
    return(
        tags$a(
            class  = "icon-link icon-link-hover pe-1",
            href   = sprintf("mailto:%s", paste0(emails, collapse = ",")),
            target = "_blank",
            ...,
            bsicons::bs_icon("envelope", a11y = "deco")
        )
    )
}

#' @rdname ui-helpers
#' @export
ui_panel_title_display <- function(
    title      = "",
    icon_name  = "",
    icon_class = NULL,
    tags       = htmltools::tags)
{
    return(
        tags$span(
            class = "text-primary fw-bold px-1",

            tags$span(
                class = "pe-1",
                bsicons::bs_icon(
                    name  = icon_name,
                    a11y  = "deco",
                    class = icon_class
                )
            ),

            title
        )
    )
}
