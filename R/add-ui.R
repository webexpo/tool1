#' Static UI Elements
#'
#' Generate elements that does not require to be rendered at runtime.
#'
#' @details
#' The application uses Bootstrap version `>= 5.3` to create UI components.
#' Required CSS files are automatically included by package bslib.
#'
#' ## Alerts
#'
#' [add_bs_alert()] is a low-level constructor of Bootstrap 5 Alerts. These
#' are colored boxes providing contextual feedback messages. Other related
#' functions such as [add_bs_alert_info()] wrap [add_bs_alert()].
#'
#' @returns
#' [add_bs_alert()],
#' [add_bs_alert_info()] and
#' [add_bs_alert_warn()] returns a [`shiny.tag`][htmltools::tag] object.
#'
#' @seealso
#' <https://getbootstrap.com/docs/5.3/components/alerts/>
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @examples
#' add_bs_alert_info(shiny::p("This informs the user about something."))
#' add_bs_alert_warn(shiny::p("This warns the user about something."))
#'
#' @export
add_bs_alert <- function(
    which = c(
        "primary",
        "secondary",
        "success",
        "danger",
        "warning",
        "info",
        "light",
        "dark"),
    title = "",
    icon  = NULL,
    ...)
{
    ui <- shiny::tags$div(
        role  = "alert",
        class = sprintf("alert alert-%s", which),
        shiny::tags$h4(class = "alert-heading", icon, title),
        ...)

    # In Bootstrap v5.3.1, class mb-0
    # corresponds to margin-bottom: 0 !important.
    # It must be set on the last child element of
    # the alert container to fix spacing.
    if (nchilds <- ...length() + 1L) {
        ui$children[[nchilds]] <- htmltools::tagAppendAttributes(
            class = "mb-0",
            ui$children[[nchilds]])
    }

    return(ui)
}

#' @rdname add-ui
#' @external
add_bs_alert_info <- function(..., title = "Information") {
    return(
        add_bs_alert(
            ...,
            which = "info",
            title = title,
            icon  = shiny::icon(
                lib   = "glyphicon",
                name  = "info-sign",
                style = "padding-right: 10px;")))
}

#' @rdname add-ui
#' @external
add_bs_alert_warn <- function(..., title = "Warning") {
    return(
        add_bs_alert(
            ...,
            which = "warning",
            title = title,
            icon  = shiny::icon(
                lib   = "glyphicon",
                name  = "warning-sign",
                style = "padding-right: 10px;")))
}
