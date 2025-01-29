add_tooltip <- function(inputId = "", title = "") {
    return(
        shinyBS::bsTooltip(
            id        = inputId,
            title     = title,
            placement = "right",
            options   = list(container = "body")))
}
