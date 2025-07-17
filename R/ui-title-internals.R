#' Managing User Interface Parameters
#'
#' Get and set customization parameters of the user interface.
#'
#' @details
#' Functions prefixed by a dot below are intended to be used only within
#' [server_title()].
#'
#' ## Paramaters
#'
#' The UI can be customized using query parameters and buttons. The former let
#' users bookmark their preferred settings.
#'
#' There are currently three parameters.
#'
#' | Parameter | Default value | Description                        |
#' | --------- | ------------- | ---------------------------------- |
#' | `lang`    | `"en"`        | The displayed language.            |
#' | `mode`    | `"express"`   | How much information is displayed. |
#' | `color`   | `"light"`     | The color theme.                   |
#'
#' The `express` mode corresponds to a version of Tool 1 that only shows a
#' curated subset of results in a single panel. This mode was called Tool 1
#' Express (Simplified) in earlier versions of Expostats. The `extended` mode
#' shows all panels.
#'
#' ## Mechanisms
#'
#' The objective is to establish a *walled garden*. Parameters are validated
#' once by parsing functions and treated as valid value afterwards. They can
#' be updated and fetched with dedicated setter and getter functions.
#'
#' Parsing functions enforce default values if the underlying query parameter
#' is missing or invalid.
#'
#' [new_query_string()] creates a query string from `lang`, `mode`, and `color`.
#'
#' [update_query_string()] builds on [new_query_string()] by further pushing
#' the resulting query string to the client using [shiny::updateQueryString()]
#' without triggering a refresh of the web page. This is useful to keep the
#' client's displayed URL synchronized with the UI's state.
#'
#' @param mode A character string equal to `"express"` (the default) or
#'   `"extended"`.
#'
#' @param color A character string equal to `"light" (the default)` or `"dark"`.
#'
#' @template param-lang-str
#'
#' @returns
#' [.set_lang()] returns `lang` invisibly. It further registers the former as
#' the new session's language.
#'
#' [.set_mode()] returns `mode` invisibly. It further registers the former as
#' the new session's mode.
#'
#' [.get_lang()] and [.get_mode()] returns a character string.
#'
#' [.set_color()] and [.get_color()] returns the output of
#' [set_color_icon_attr()]. The former does so invisibly.
#'
#' [.set_color_icon_attr()] returns `color` invisibly, and attaches one
#' attribute named `icon` to it. This is an `html` object representing an
#' `<svg>` HTML tag. It further registers this pair as the new session's color
#' mode.
#'
#' [.parse_lang()], [.parse_mode()], and [.parse_color()] return a character
#' string.
#'
#' [new_query_string()] returns a character string.
#'
#' [update_query_string()] returns the output of [new_query_string()],
#' invisibly.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso
#' [shiny::session],
#' [bslib::toggle_dark_mode()],
#' [shiny::updateActionButton()],
#' [shiny::updateQueryString()],
#' [Bootstrap 5 Color Modes](https://getbootstrap.com/docs/5.3/customize/color-modes/)
#'
#' @rdname ui-title-internals
#' @export
.set_lang <- function(lang = "") {
    session <- shiny::getDefaultReactiveDomain()
    assign(session$ns("app_state_lang"), lang, session$userData)
    return(invisible(lang))
}

#' @rdname ui-title-internals
#' @export
.get_lang <- function() {
    session <- shiny::getDefaultReactiveDomain()
    return(
        get0(
            session$ns("app_state_lang"),
            session$userData,
            mode       = "character",
            inherits   = FALSE,
            ifnotfound = "en"
        )
    )
}

#' @rdname ui-title-internals
#' @export
.set_mode <- function(mode = "") {
    session <- shiny::getDefaultReactiveDomain()
    assign(session$ns("app_state_mode"), mode, session$userData)
    return(invisible(mode))
}

#' @rdname ui-title-internals
#' @export
.get_mode <- function() {
    session <- shiny::getDefaultReactiveDomain()
    return(
        get0(
            session$ns("app_state_mode"),
            session$userData,
            mode       = "character",
            inherits   = FALSE,
            ifnotfound = "express"
        )
    )
}

#' @rdname ui-title-internals
#' @export
.set_color <- function(color = "") {
    if (is.null(color)) {
        # Toggle the current color mode if color is NULL.
        color <- switch(.get_color(), light = "dark", dark = "light")
    }

    color <- .set_color_icon_attr(color)
    session <- shiny::getDefaultReactiveDomain()
    assign(session$ns("app_state_color"), color, session$userData)
    return(invisible(color))
}

#' @rdname ui-title-internals
#' @export
.get_color <- function() {
    session <- shiny::getDefaultReactiveDomain()
    return(
        get0(
            session$ns("app_state_color"),
            session$userData,
            mode       = "character",
            inherits   = FALSE,
            ifnotfound = .set_color_icon_attr("light")
        )
    )
}

#' @rdname ui-title-internals
#' @export
.set_color_icon_attr <- function(color = "") {
    # Icon shown in light mode is the
    # one of dark mode and vice-versa.
    name <- switch(color, dark = "sun-fill", light = "moon-fill", NULL)
    return(structure(color, icon = bsicons::bs_icon(name, a11y = "sem")))
}

#' @rdname ui-title-internals
#' @export
.parse_lang <- function(lang = names(tr$native_languages)) {
    lang <- tolower(lang)
    return(match_arg(lang, "en"))
}

#' @rdname ui-title-internals
#' @export
.parse_mode <- function(mode = c("express", "extended")) {
    mode <- tolower(mode)
    return(match_arg(mode))
}

#' @rdname ui-title-internals
#' @export
.parse_color <- function(color = c("light", "dark")) {
    color <- tolower(color)
    return(match_arg(color))
}

#' @rdname ui-title-internals
#' @export
new_query_string <- function(
    lang  = .get_lang(),
    mode  = .get_mode(),
    color = .get_color())
{
    return(sprintf("?lang=%s&mode=%s&color=%s", lang, mode, color))
}

#' @rdname ui-title-internals
#' @export
update_query_string <- function(...) {
    session <- shiny::getDefaultReactiveDomain()
    string <- new_query_string(...)

    shiny::updateQueryString(string, "replace", session)
    return(invisible(string))
}
