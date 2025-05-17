#' Managing User Interface Parameters
#'
#' Get and set user interface's customization parameters.
#'
#' All functions below are meant to be used inside [server_title()].
#'
#' @details
#' The user interface can be customized using query parameters and buttons.
#' The former are only used once when the application is first loaded. The
#' latter can be used anytime after the initial load and whenever required.
#'
#' Query parameters let users bookmark their preferred settings.
#'
#' There are currently three parameters.
#'
#' ## Current Language (`lang`)
#'
#' `lang` controls the displayed language. Changing it currently requires a
#' full refresh of the application.
#'
#' ## Current Mode (`mode`)
#'
#' `mode` controls how much information is displayed to the user.
#'
#' The `default` mode corresponds to the default version of Tool 1. All panels
#' are available/shown.
#'
#' The `simplified` mode corresponds to a version of Tool 1 only showing a
#' curated subset of results in a single panel. This mode used to be called
#' Tool 1 Express (Simplified) in the past.
#'
#' The current `mode` can always be changed without triggering a full refresh
#' of the application.
#'
#' ## Current Color Mode (`color`)
#'
#' `color` controls the color theme: either `light` or `dark`. It can always be
#' changed without triggering a full refresh of the application.
#'
#' ## Handling Query Strings
#'
#' [parse_lang()], [parse_mode()], and [parse_color()] enforce default values
#' if the underlying query parameters are missing or invalid.
#'
#' [new_query_string()] creates a query string from `lang`, `mode`, and `color`.
#'
#' [update_query_string()] further pushes this update to the client using
#' [shiny::updateQueryString()]. This is useful to keep the client's URL
#' synchronized with the current state of the user interface. Doing so does
#' NOT trigger a refresh of the web page.
#'
#' @param lang A character string. It must be equal to one of the values
#'   defined in `default_lang_names`.
#'
#' @param mode A character string equal to `"default"` or `"simplified"`.
#'
#' @param color A character string equal to `"light"` or `"dark"`.
#'
#' @param what A character string equal to `"both"`, `"current"`, or `"label"`.
#'   What to extract: the current color theme, the name of the label to display
#'   on the button toggling it, or both.
#'
#' @param langs A character vector. Language codes of supported languages.
#'   It is equal to `names(tr$native_languages)` by default, where `tr` is
#'   a global constant.
#'
#' @returns
#' [set_lang()] returns `lang`, invisibly.
#'
#' [get_lang()] returns a character string, the currently registered `lang`
#' value.
#'
#' [set_mode()] returns `mode`, invisibly.
#'
#' [get_mode()] returns a character string, the currently registered `mode`
#' value.
#'
#' [set_color()] returns a named character vector of length 2 containing
#' elements `current` and `label`. See argument `what` for more information.
#'
#' [get_color()] returns a named character vector of length 2 containing
#' elements `current` and `label` if `what` is equal to `"both"`, and a
#' character string otherwise. See argument `what` for more information.
#'
#' [parse_lang()], [parse_mode()], and [parse_color()] return a character
#' string.
#'
#' [new_query_string()] returns a character string, a query string constructed
#' from `lang`, `mode`, and `color`.
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
#' @rdname helpers-ui-title
#' @export
set_lang <- function(lang = "") {
    session <- shiny::getDefaultReactiveDomain()
    assign(session$ns("lang"), lang, session$userData)
    return(invisible(lang))
}

#' @rdname helpers-ui-title
#' @export
get_lang <- function() {
    session <- shiny::getDefaultReactiveDomain()

    return(
        get0(
            session$ns("lang"),
            session$userData,
            mode       = "character",
            inherits   = FALSE,
            ifnotfound = default_lang
        )
    )
}

#' @rdname helpers-ui-title
#' @export
set_mode <- function(mode = "") {
    session <- shiny::getDefaultReactiveDomain()
    assign(session$ns("mode"), mode, session$userData)
    return(invisible(mode))
}

#' @rdname helpers-ui-title
#' @export
get_mode <- function() {
    session <- shiny::getDefaultReactiveDomain()

    return(
        get0(
            session$ns("mode"),
            session$userData,
            mode       = "character",
            inherits   = FALSE,
            ifnotfound = "default"
        )
    )
}

#' @rdname helpers-ui-title
#' @export
set_color <- function(color = "") {
    if (is.null(color)) {
        # Toggle the current color mode if color is NULL.
        color <- switch(
            get_color("current"),
            light = "dark",
            dark  = "light",
            NULL
        )
    }

    # Icon shown in light mode is the
    # one of dark mode and vice-versa.
    label <- bsicons::bs_icon(
        name = switch(color, dark = "sun-fill", light = "moon-fill"),
        a11y = "sem"
    )

    state <- c(current = color, label = label)
    session <- shiny::getDefaultReactiveDomain()
    assign(session$ns("color"), state, session$userData)
    return(invisible(state))
}

#' @rdname helpers-ui-title
#' @export
get_color <- function(what = c("both", "current", "label")) {
    session <- shiny::getDefaultReactiveDomain()
    state <- get0(
        session$ns("color"),
        session$userData,
        mode       = "character",
        inherits   = FALSE,
        ifnotfound = c(current = "light", label = "sun-fill")
    )

    return(switch(what, both = state, state[[what]]))
}

#' @rdname helpers-ui-title
#' @export
parse_lang <- function(lang = "", langs = names(default_lang_names)) {
    if (is.null(lang) || !match(lang <- tolower(lang), langs, 0L)) {
        return(default_lang)
    }

    return(lang)
}

#' @rdname helpers-ui-title
#' @export
parse_mode <- function(mode = "") {
    modes <- c("default", "simplified")

    if (is.null(mode) || !match(mode <- tolower(mode), modes, 0L)) {
        return(modes[[1L]])
    }

    return(mode)
}

#' @rdname helpers-ui-title
#' @export
parse_color <- function(color = "") {
    colors <- c("light", "dark")

    if (is.null(color) || !match(color <- tolower(color), colors, 0L)) {
        return(colors[[1L]])
    }

    return(color)
}

#' @rdname helpers-ui-title
#' @export
new_query_string <- function(
    lang  = get_lang(),
    mode  = get_mode(),
    color = get_color("current"))
{
    return(sprintf("?lang=%s&mode=%s&color=%s", lang, mode, color))
}

#' @rdname helpers-ui-title
#' @export
update_query_string <- function(...) {
    session <- shiny::getDefaultReactiveDomain()
    query_string <- new_query_string(...)

    shiny::updateQueryString(query_string, "replace", session)
    return(invisible(query_string))
}
