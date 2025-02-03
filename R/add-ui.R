#' Define Custom Shiny UI Static Elements
#'
#' Generate static UI elements. Functions below return miscellaneous
#' [`shiny.tag`][htmltools::tag] objects.
#'
#' @details
#' The application uses Bootstrap (version `>= 5.3`) to create UI components.
#' Since it relies on R package [bslib], it simply reuses Bootstrap (version
#' 5) external files exposed by this package.
#'
#' In practice, these means that functions below use Bootstrap-specific CSS
#' classes.
#'
#' ## Alerts
#'
#' [add_bs_alert()] is a low-level constructor of Bootstrap (version 5) Alerts.
#' Alerts provide contextual feedback messages for typical user actions. Other
#' related functions wrap [add_bs_alert()].
#'
#' @returns
#' [add_bs_alert()], [add_bs_alert_info()], and [add_bs_alert_warn()] returns
#' a [`shiny.tag`][htmltools::tag] object representing a Bootstrap 4 Alert
#' component.
#'
#' @seealso
#' [Bootstrap 5.3 documentation for alerts](https://getbootstrap.com/docs/5.3/components/alerts/)
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @examples
#' add_bs_alert_info("This blue box informs the user about something.")
#' add_bs_alert_warn("This orange-ish box warns the user about something.")
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
add_bs_alert_info <- function(...) {
    return(
        add_bs_alert(
            ...,
            which = "info",
            title = "Information",
            icon  = shiny::icon(
                lib   = "glyphicon",
                name  = "info-sign",
                style = "padding-right: 10px;")))
}

#' @rdname add-ui
#' @external
add_bs_alert_warn <- function(...) {
    return(
        add_bs_alert(
            ...,
            which = "warning",
            title = "Warning",
            icon  = shiny::icon(
                lib   = "glyphicon",
                name  = "warning-sign",
                style = "padding-right: 10px;")))
}
