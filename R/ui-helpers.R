#' Generate Static UI Elements
#'
#' These are helper functions used to standardize some aspects of the user
#' interface and avoid code repetition.
#'
#' @param href A character string. A Uniform Resource Locator (URL).
#'
#' @param style A character string equal to `"primary"`, `"secondary"`,
#'   `"success"`, `"danger"`, `"warning"`, `"info"`, `"light"`, `"dark"`,
#'   or `"emphasis"`.
#'
#' @param emails A character vector.
#'
#' @param title A character string.
#'
#' @param icon_name A character string or `NULL`. It is passed to argument
#'   `name` of [bsicons::bs_icon()]. If `NULL` or an empty character string
#'   is passed to `icon_name`, no icon is shown and `icon_class` is ignored.
#'
#' @param icon_class A character string. It is passed to argument `class` of
#'   [bsicons::bs_icon()].
#'
#' @param ... Further tag attributes (named arguments) and children (unnamed
#'   arguments) passed to various tag functions of package htmltools.
#'
#' @returns
#' A `shiny.tag` object.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-helpers
#' @export
ui_link <- function(
    href = "",
    ...,
    style = c(
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
    style <- match.arg(style)
    return(
        htmltools::a(
            class  = sprintf("link-%s link-offset-2 link-underline-opacity-25 link-underline-opacity-100-hover", style),
            href   = href,
            target = "_blank",
            ...
        )
    )
}

#' @rdname ui-helpers
#' @export
ui_link_mailto <- function(
    emails = character(),
    ...,
    style = c(
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
    style <- match.arg(style)
    return(
        htmltools::span(
            ui_link(
                style = style,
                sprintf("mailto:%s", paste0(emails, collapse = ",")),
                ...
            ),

            htmltools::span(
                .noWS = c("before", "after", "outside", "after-begin", "before-end"),
                class = sprintf("text-%s ps-1", style),
                bsicons::bs_icon("envelope", a11y = "deco")
            )
        )
    )
}

#' @rdname ui-helpers
#' @export
ui_element <- function(
    title      = "",
    icon_name  = "",
    icon_class = NULL)
{
    return(
        htmltools::span(
            .noWS = c("before", "after", "outside", "after-begin", "before-end"),
            class = "text-primary fw-bold",

            if (!is.null(icon_name) && nzchar(icon_name)) {
                htmltools::span(
                    .noWS = c("before", "after", "outside", "after-begin", "before-end"),
                    class = "px-1",

                    bsicons::bs_icon(
                        name  = icon_name,
                        a11y  = "deco",
                        class = icon_class
                    )
                )
            },

            title
        )
    )
}
