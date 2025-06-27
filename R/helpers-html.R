#' Create HTML Tags
#'
#' Alias to Shiny's usual list of HTML <tag> functions.
#'
#' @rdname helpers-html
#' @export
tags <- htmltools::tags

#' Create HTML Content
#'
#' Embed HTML tags into source text.
#'
#' @param template A character string containing zero or more [sprintf()]
#'   placeholders. Use character placeholders (`%s`) for `shiny.tag` objects.
#'
#' @param ... Further arguments to be inserted into `template`.
#'
#' @param ignore A character string returned as is whenever `template` is
#'   identical to it.
#'
#' @returns A character string of class `html`.
#'
#' @examples
#' html("This is %s text.", shiny::span(style = "text-weight: bold;", "bold"))
#'
#' @seealso
#' [sprintf()],
#' [htmltools::HTML()]
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname helpers-html
#' @export
html <- function(template = "", ..., ignore = default_missing_translation_msg) {
    if (identical(template, ignore)) {
        return(htmltools::HTML(ignore))
    }

    str <- do.call(sprintf, c(fmt = template, lapply(list(...), as.character)))
    return(htmltools::HTML(str))
}

#' Create HTML Tables
#'
#' Convert R objects to a <table> `shiny.tag` object.
#'
#' @param x An R object.
#'
#' @param id A character string or `NULL`. An optional identifier for the
#'   table. Do not include the usual `#` prefix.
#'
#' @param colnames A character vector. The names to display for each column.
#'   These are used to create the <thead> tag.
#'
#' @param escape A logical value. Should HTML entities in the table be escaped?
#'
#' @param class_table A character string or `NULL`. Optional CSS classes to
#'   apply on the <table> tag (the whole table).
#'
#' @param class_thead A character string or `NULL`. Optional CSS classes to
#'   apply on the <thead> tag (the header's row).
#'
#' @param class_thead_tr A character string or `NULL`. Optional CSS classes
#'   to apply on the <tr> tag of <thead> (the header's row).
#'
#' @param class_tbody A character string or `NULL`. Optional CSS classes to
#'   apply on the <tbody> tag (the table's body or all rows except the first
#'   one).
#'
#' @param class_tbody_tr A character string or `NULL`. Optional CSS classes
#'   to apply on all <tr> tags of <tbody> (all rows except the first one).
#'
#' @param ... Further arguments passed to [paste0()]. This function is used
#'   to convert each value of `x` to a character string.
#'
#' @returns
#' A `shiny.tag` object.
#'
#' @examples
#' as_html_table(mtcars)
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname helpers-as-html-table
#' @export
as_html_table <- function(x, ...) {
    UseMethod("as_html_table")
}

#' @rdname helpers-as-html-table
#' @export
as_html_table.data.frame <- function(
    x,
    id             = NULL,
    colnames       = names(x),
    escape         = FALSE,
    class_table    = "table table-hover table-sm",
    class_thead    = NULL,
    class_thead_tr = NULL,
    class_tbody    = "table-group-divider",
    class_tbody_tr = "border-bottom",
    ...)
{
    stopifnot(exprs = {
        is.character(colnames)
        length(colnames) == ncol(x)
        is.null(escape) || is_lgl1(escape)
        is.null(id) || is_chr1(id)
        is.null(class_table) || is_chr1(class_table)
        is.null(class_thead) || is_chr1(class_thead)
        is.null(class_tbody) || is_chr1(class_tbody)
        is.null(class_thead_tr) || is_chr1(class_thead_tr)
        is.null(class_tbody_tr) || is_chr1(class_tbody_tr)
    })

    # If escape is TRUE, use function identity
    # to return each value as is. They will be
    # escaped by tag functions of htmltools.
    # Otherwise, mark each cell as being valid
    # HTML to prevent escaping.
    process <- if (escape) identity else htmltools::HTML

    # Create <td> cells from column names.
    row_header <- htmltools::tags$tr(
        class = class_thead_tr,
        lapply(colnames, htmltools::tags$th, scope = "col")
    )

    # Create <tr> rows from each row.
    rows_body <- apply(x, 1L, \(row) {
        # Convert cells to character strings. Mark
        # them as HTML contents if escape is FALSE.
        row <- lapply(vapply(row, paste0, NA_character_, ...), process)

        # Create <td> cells and encapsulate them in a <tr> row.
        return(
            htmltools::tags$tr(
                class = class_tbody_tr,
                lapply(row, htmltools::tags$td)
            )
        )
    })

    return(
        htmltools::tags$table(
            id    = sprintf("#%s", id),
            class = class_table,

            htmltools::tags$thead(
                class = class_thead,
                row_header
            ),

            htmltools::tags$tbody(
                class = class_tbody,
                rows_body
            )
        )
    )
}
