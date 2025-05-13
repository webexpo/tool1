ui_panel_simplified <- function(id) {
    ns <- shiny::NS(id)
    ui <- bslib::nav_panel(
        value = id,
        title = shiny::textOutput(ns("title"), tags$span),
        # Using shiny::uiOutput() is a quick and dirty way
        # of preventing the icon from being shown when the
        # app is first loaded in default mode. This ensures
        # it is not shown twice (even temporarily): one for
        # panel_simplified and one for panels_menu.
        icon = shiny::uiOutput(
            outputId  = ns("icon"),
            container = tags$span,
            class     = "pe-1"
        ),

        tags$p("Hello, world!")
    )

    return(ui)
}

server_panel_simplified <- function(id, lang) {
    stopifnot(shiny::is.reactive(lang))

    server <- function(input, output, session) {
        output$title <- shiny::renderText({
            translate(lang = lang(), "Inference")
        }) |>
        shiny::bindCache(lang())

        # See note in ui_panel_simplified().
        output$icon <- shiny::renderUI({
            bsicons::bs_icon(
                name  = "body-text",
                a11y  = "deco",
                class = "app-rotated-minus-90"
            )
        }) |>
        # Cache the output once and always use it afterwards.
        shiny::bindCache(NULL)

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}
