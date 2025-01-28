add_tooltip <- function(param, txt = "[0 &lt; valid &le; 100]") {
    return(
        shinyBS::bsTooltip(
            id        = param,
            title     = txt,
            placement = "right",
            options   = list(container = "body")))
}
