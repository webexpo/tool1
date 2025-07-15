#' Top Busy Banner Module
#'
#' @description
#' This module controls the Banner component conceptually illustrated below.
#'
#' ```
#' ------------------------------------------------------------
#' | Title                                                    |
#' |         ----------------------------------------         |
#' | ------- | Updating. Please Wait. (this module) | ------- |
#' | Sidebar ----------------------------------------         |
#' |         | Main                                           |
#' |         |  --------------------------------------------  |
#' |         |  | Panels Navigation                        |  |
#' |         |  --------------------------------------------  |
#' |         |  | Active Panel                             |  |
#' |         |  |                                          |  |
#' |         |  |     ---------------------------------    |  |
#' |         |  |     | Modal (this module)           |    |  |
#' |         |  |     | Shown when button is clicked  |    |  |
#' |         |  |     | (on top of other elements)    |    |  |
#' |         |  |     ---------------------------------    |  |
#' |         |  |                                          |  |
#' |         |  --------------------------------------------  |
#' ------------------------------------------------------------
#' ```
#'
#' The banner is shown whenever the Shiny engine is blocked.
#'
#' @template param-id
#'
#' @template param-lang
#'
#' @returns [ui_banner()] returns a `shiny.tag` object (an output of
#'   [shiny::conditionalPanel()]).
#'
#' [server_banner()] returns `NULL`, invisibly.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-banner
#' @export
ui_banner <- function(id) {
    ns <- shiny::NS(id)

    return(
        shiny::conditionalPanel(
            ns        = ns,
            condition = r"{$("html").hasClass("shiny-busy")}",

            tags$div(
                # This stacks the panel on top of all other elements
                # (z-2), and aligns its left edge with the page's top
                # center (position-fixed top-0 start-50). It is shifted
                # back on its x-axis so that the center of of the <div>
                # is centered not its edge (translate-middle-x). It is
                # shifted to the bottom slightly with a margin (mt-5).
                # The rest is self-explanatory.
                class = "position-fixed top-0 start-50 translate-middle-x z-2 mt-5 alert alert-warning",
                role  = "alert",

                tags$div(
                    class = "fs-4 text-center",

                    tags$span(
                        class = "pe-2",
                        bsicons::bs_icon("exclamation-triangle-fill", a11y = "deco")
                    ),

                    shiny::textOutput(ns("text"), tags$span)
                )
            )
        )
    )
}

#' @rdname ui-banner
#' @export
server_banner <- function(id, lang) {
    stopifnot(shiny::is.reactive(lang))

    server <- function(input, output, session) {
        output$text <- shiny::renderText({
            translate(lang = lang(), "Updating. Please wait.")
        }) |>
        shiny::bindCache(lang())

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}
