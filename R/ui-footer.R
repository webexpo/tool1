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
#'
#' @export
ui_footer <- function(id) {
    ns <- shiny::NS(id)

    return(
        tags$footer(
            class = "m-auto text-center",
            style = "font-size: 0.75rem;",

            shiny::uiOutput(ns("version")),
            shiny::uiOutput(ns("copyright")),
            shiny::uiOutput(ns("made_by"))
        )
    )
}

#' @rdname ui-footer
#' @export
server_footer <- function(id, lang) {
    stopifnot(shiny::is.reactive(lang))

    server <- function(input, output, session) {
        output$version <- shiny::renderUI({
            lang <- lang()

            tags$div(
                translate(lang = lang(), "Tool 1"),
                translate(lang = lang(), "version"),

                tags$a(
                    href   = default_urls$code,
                    target = "_blank",
                    default_version[["number"]]
                ),

                sprintf("(%s).", default_version[["release_date"]])
            )
        })

        output$copyright <- shiny::renderUI({
            tags$div(
                bsicons::bs_icon("c-circle-fill", ally = "sem"),

                tags$a(
                    href   = default_urls$jerome_lavoue,
                    target = "_blank",
                    "Jérôme Lavoué"
                ),

                format(Sys.time(), tz = "EST", format = "(%Y)."),
                translate(lang = lang(), "All rights reserved.")
            )
        })

        output$made_by <- shiny::renderUI({
            tags$div(
                html(
                    translate(lang = lang(), "Made with %s by %s."),

                    tags$span(
                        style = "color: red;",
                        bsicons::bs_icon("heart-fill", ally = "sem")
                    ),

                    tags$a(
                        href   = default_urls$ununoctium,
                        target = "_blank",
                        "Ununoctium"
                    )
                )
            )
        })

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}
