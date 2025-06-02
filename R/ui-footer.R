#' Footer Module
#'
#' @description
#' This module controls the Footer component which gives information on the
#' current version and copyright holder of Tool 1. It is currently nested
#' into the Sidebar and About Modal modules.
#'
#' @details
#' This module implicitly relies on values defined in `R/global.R` and
#' `R/helpers*.R` scripts. They are sourced by [shiny::runApp()].
#'
#' @template param-id
#'
#' @template param-lang
#'
#' @returns
#' [ui_footer()] returns a `shiny.tag` object.
#'
#' [server_footer()] returns `NULL`, invisibly.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-footer
#' @export
ui_footer <- function(id) {
    ns <- shiny::NS(id)

    return(
        tags$footer(
            class = "d-flex flex-column m-auto text-center",
            style = "gap: 4px; font-size: 0.75rem;",

            # Shiny uses display: contents when shiny::renderUI()
            # returns a list. Here, it breaks how contents should
            # flow, so display: block; is used to override this
            # default behavior.
            shiny::uiOutput(ns("version"),   class = "d-block"),
            shiny::uiOutput(ns("changelog"), class = "d-block"),
            shiny::uiOutput(ns("copyright"), class = "d-block"),
            shiny::uiOutput(ns("made_by"),   class = "d-block")
        )
    )
}

#' @rdname ui-footer
#' @export
server_footer <- function(id, lang) {
    stopifnot(shiny::is.reactive(lang))

    server <- function(input, output, session) {
        latest_tag_url <- sprintf(
            "%s/releases/tag/v%s",
            shared_urls$code,
            default_version[["number"]]
        )

        output$version <- shiny::renderUI({
            html(
                "Tool 1 %s %s (%s)",
                translate(lang = lang(), "version"),
                ui_link(latest_tag_url, default_version[["number"]]),
                default_version[["release_date"]]
            )
        }) |>
        # All values are constants.
        shiny::bindCache(lang())

        output$changelog <- shiny::renderUI({
            lang <- lang()
            list(
                ui_link(
                    "static/news.html",
                    translate(lang = lang, "Changelog")
                ),

                tags$span(
                    class = "ps-1",
                    translate(lang = lang, "(English only)")
                )
            )
        }) |>
        # All values are constants.
        shiny::bindCache(lang())

        output$copyright <- shiny::renderUI({
            html(
                "%s (%s). %s",

                ui_link(shared_urls$jerome_lavoue, "Jérôme Lavoué"),
                format(Sys.time(), tz = "EST", format = "%Y"),
                translate(lang = lang(), "All rights reserved.")
            )
        }) |>
        # We consider the year to be constant
        # while server_footer() is running.
        shiny::bindCache(lang())

        output$made_by <- shiny::renderUI({
            html(
                translate(lang = lang(), "Made with %s by %s."),

                tags$span(
                    class = "px-1",
                    style = "color: red;",
                    bsicons::bs_icon("heart-fill", ally = "sem")
                ),

                ui_link(shared_urls$ununoctium, "Ununoctium")
            )
        }) |>
        # All values are constants.
        shiny::bindCache(lang())

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}
