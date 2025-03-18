#' Footer Module
#'
#' @description
#' This module controls the Footer component which gives information on the
#' current version and copyright holder of Tool 1. It is currently nested
#' into the Sidebar and About Modal modules.
#'
#' @details
#' This module takes in parameters ony when necessary and relies on global
#' constants defined in `R/global.R` and `R/ui-theme.R` otherwise.
#'
#' @param id The module's unique identifier. It is passed to [shiny::NS()]
#'   to scope names of inputs and outputs. Shiny handles namespaces inside
#'   server functions.
#'
#' @param lang A [shiny::reactive()] object returning the current language.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @export
ui_footer <- function(id) {
    ns <- shiny::NS(id)
    return(
        tags$footer(
            class = "m-auto text-center",
            style = "font-size: 0.75rem;",

            shiny::uiOutput(ns("version")),
            shiny::uiOutput(ns("copyright"))
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

            return(
                tags$div(
                    intl(lang = lang(), "Tool 1"),
                    intl(lang = lang(), "version"),
                    tags$a(
                        version_number,
                        href   = urls$code,
                        target = "_blank"
                    ),
                    sprintf("(%s).", version_date)
                )
            )
        })

        output$copyright <- shiny::renderUI({
            return(
                tags$div(
                    ui_icons$copyright,
                    tags$a(
                        "Jérôme Lavoué",
                        href   = urls$jerome_lavoue,
                        target = "_blank"
                    ),
                    sprintf("(%s).", year),
                    intl(lang = lang(), "All rights reserved.")
                )
            )
        })
    }

    return(shiny::moduleServer(id, server))
}
