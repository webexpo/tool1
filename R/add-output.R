add_bold_text_output <- function(outputId = "") {
    return(
        htmltools::tagAppendAttributes(
            style = "font-weight: bold;",
            shiny::textOutput(outputId, inline = TRUE)))
}
