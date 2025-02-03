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
