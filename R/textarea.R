# TODO: (JMP) Implement proper input checks.
inputTextArea <- function(
    inputId = "",
    label   = "",
    value   = "",
    width   = "100%",
    nrows   = 10L)
{
    return(
        htmltools::tagList(
            # Shiny-like label for the <textarea> defined below.
            html$label(
                id    = sprintf("%s-label", inputId),
                class = "control-label",
                `for` = inputId,  # for is a reserved keyword in R.
                label),

            # Shiny-like custom <textarea> input box.
            # HTML attribute rows controls the default
            # vertical length of the <textarea> box.
            html$textarea(
                id    = inputId,
                rows  = nrows,
                class = "form-control",
                style = sprintf("width:%s!important", width),
                as.character(value))))
}
