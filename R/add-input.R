#' Define Custom Shiny Inputs
#'
#' [add_input()] is the low-level constructor. It automatically creates
#' an HTML `<label>` for `input`, and encapsulates these components into
#' a suitable `<div>` container. The output is identical to what [shiny]
#' would natively generate.
#'
#' [add_input_text_area()] creates a [shiny]-like `<textarea>` input.
#'
#' @param inputId A character string. A unique identifier that will be used
#'   to access the value of `input`.
#'
#' @param label A character string. A short tag describing the `input`.
#'
#' @param input A [`shiny.tag`][htmltools::tag] object typically created with
#'   [shiny::tags$input()][htmltools::tags].
#'
#' @param inputs A list of [`shiny.tag`][htmltools::tag] typically created
#'   with [shiny::tags$input()][htmltools::tags] for [add_input_field_set()].
#'   It can be empty.
#'
#' @param inline A logical value. Should the `input` container be inlined?
#'   More specifically, this controls which class should be passed to the
#'   underlying `<div>` container.
#'
#' @param containerId A character string, or `NULL`. An optional unique
#'   identifier that will be used to select the parent container.
#'
#' @param ... Further optional attributes passed to the parent container
#'   for [add_input()]. It cannot contain a value named `id`, or `class`.
#'
#' @param value An [atomic] vector to be coerced to a character string. The
#'   default value(s) of the `<textarea>` input.
#'
#' @param width A character string. A CSS property setting the width of the
#'   `<textarea>` input.
#'
#' @param nrows An integer greater than 0. The number of visible text lines.
#'
#' @returns
#' [add_input()] returns a single [`shiny.tag`][htmltools::tag] object
#' representing an HTML `<div>` element.
#'
#' [add_input_text_area()] returns the same output as [add_input()] where
#' the `input` is an HTML `<textaera>` element.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @examples
#' my_input <- shiny::tags$textarea()
#' add_input("my-input", "My text area input:", my_input)
#'
#' add_input_text_area(
#'   inputId = "data",
#'   label   = "Observations:",
#'   value   = c("28.9", "19.4", "<5.5", "149.9", "26.42", "56.1"))
#'
#' @export
add_input <- function(
    inputId     = "",
    label       = "",
    input       = shiny::tags$input(),
    inline      = FALSE,
    containerId = NULL,
    ...)
{
    if (!is.character(inputId) ||
        length(inputId) != 1L ||
        is.na(inputId) ||
        !nzchar(inputId)) {
        stop(
            "'inputId' must be a non-empty and non-NA character of length 1.",
            call. = FALSE)
    }
    if (!is.character(label) || length(label) != 1L || is.na(label)) {
        stop("'label' must be a non-NA character of length 1.", call. = FALSE)
    }
    if (!inherits(input, "shiny.tag")) {
        stop("'input' must be a 'shiny.tag' object.", call. = FALSE)
    }
    if (!is.logical(inline) || length(inline) != 1L || is.na(inline)) {
        stop("'inline' must be 'TRUE', or 'FALSE'.", call. = FALSE)
    }
    if (!is.null(containerId) && (
            !is.character(containerId) ||
            length(containerId) != 1L ||
            is.na(containerId) ||
            !nzchar(containerId))) {
        stop(
            "'containerId' must be a non-empty and non-NA character of length 1.",
            call. = FALSE)
    }

    # Shiny-like label for the <input>.
    label <- shiny::tags$label(
        class = "control-label",
        id    = sprintf("%s-label", inputId),
        `for` = inputId,  # for is a reserved keyword in R.
        label)

    containerClasses <- c(
        "form-group",
        if (inline) "shiny-input-container-inline" else "shiny-input-container")

    # Shiny encapsulates <label> and <input>
    # into a <div> having the following classes.
    return(
        html$div(
            id    = containerId,
            class = paste0(containerClasses, collapse = " "),
            ...,
            label,
            input))
}

#' @rdname add-input
#' @export
add_input_text_area <- function(
    inputId     = "",
    label       = "",
    containerId = NULL,
    value       = character(),
    width       = "100%",
    nrows       = 10L)
{
    if (!is.atomic(value)) {
        stop("'value' must be a vector of any atomic type.", call. = FALSE)
    }
    if (!is.character(width) ||
        length(width) != 1L ||
        is.na(width) ||
        !nzchar(width)) {
        stop(
            "'width' must be a non-empty and non-NA character of length 1.",
            call. = FALSE)
    }
    if (!is.integer(nrows) || length(nrows) != 1L || is.na(nrows)) {
        stop(
            "'nrows' must be a non-NA integer of length 1.",
            call. = FALSE)
    }

    # Shiny-like custom <textarea> input box.
    # HTML attribute rows controls the default
    # vertical length of the <textarea> box.
    input <- shiny::tags$textarea(
        id    = inputId,
        class = "form-control",
        style = sprintf("width: %s !important;", width),
        rows  = nrows,
        paste0(value, collapse = "\n"))

    return(
        add_input(
            inputId     = inputId,
            label       = label,
            input       = input,
            containerId = containerId))
}

#' @rdname add-input
#' @export
add_input_field_set <- function(
    inputId     = "",
    label       = "",
    containerId = NULL,
    inputs      = list(),
    ...)
{
    if (!is.list(inputs) ||
        !all(vapply(inputs, inherits, NA, what = "shiny.tag"))) {
        stop("'inputs' must be a list of 'shiny.tag' objects.", call. = FALSE)
    }

    input <- do.call(
        shiny::tags$fieldset,
        c(id = inputId, class = "app-flex-row", inputs))

    return(
        add_input(
            inputId     = inputId,
            label       = label,
            input       = input,
            inline      = TRUE,
            containerId = containerId,
            ...))
}
