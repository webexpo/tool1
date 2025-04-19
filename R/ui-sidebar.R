#' Sidebar Module
#'
#' @description
#' This module controls the Sidebar component conceptually illustrated below.
#'
#' ```
#' ---------------------------------------------
#' | Title                                     |
#' ---------------------------------------------
#' | Sidebar       | Main                      |
#' | (this module) |  -----------------------  |
#' |               |  | Panels Navigation   |  |
#' |               |  -----------------------  |
#' |               |  | Active Panel        |  |
#' |               |  |                     |  |
#' |               |  |                     |  |
#' |               |  |                     |  |
#' |               |  -----------------------  |
#' ---------------------------------------------
#' ```
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
#' [ui_sidebar()] returns a `bslib_sidebar` object
#' (an output of [bslib::sidebar()]).
#'
#' [server_sidebar()] returns a [shiny::reactive()] object. For more
#' information, consult the source text passed to related tooltips in
#' [server_sidebar()] below.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-sidebar
#'
#' @export
ui_sidebar <- function(id) {
    ns <- shiny::NS(id)
    ui <- bslib::sidebar(
        width = "400px",
        open  = list(
            mobile  = "closed",
            desktop = "open"
        ),

        # Title ----------------------------------------------------------------

        # Setting class sidebar-title is recommended by
        # bslib. Otherwise, the title is ugly and must
        # be fixed with further classes.
        title = tags$h3(
            class = "sidebar-title text-center",

            tags$span(
                class = "pe-2",
                bsicons::bs_icon("calculator-fill", a11y = "deco")
            ),

            shiny::textOutput(ns("title"), tags$span)
        ),

        # Inputs ---------------------------------------------------------------

        shiny::numericInput(
            inputId = ns("oel"),
            label   = "",
            value   = 100
        ) |>
        bslib::tooltip(id = ns("oel_tooltip"), ""),

        shiny::numericInput(
            inputId = ns("conf"),
            label   = "",
            value   = 90,
            min     = 0,
            max     = 100
        ) |>
        bslib::tooltip(id = ns("conf_tooltip"), ""),

        shiny::numericInput(
            inputId = ns("psi"),
            label   = "",
            value   = 30,
            min     = 0,
            max     = 100
        ) |>
        bslib::tooltip(id = ns("psi_tooltip"), ""),

        # This output is only shown when the active panel is panel_fraction.
        shiny::numericInput(
            inputId = ns("frac_threshold"),
            label   = "",
            value   = 5,
            min     = 0,
            max     = 100
        ) |>
        shinyjs::hidden() |>
        bslib::tooltip(id = ns("frac_threshold_tooltip"), ""),

        # This output is only shown when the active panel is panel_percentiles.
        shiny::numericInput(
            inputId = ns("target_perc"),
            label   = "",
            value   = 95,
            min     = 0,
            max     = 100
        ) |>
        shinyjs::hidden() |>
        bslib::tooltip(id = ns("target_perc_tooltip"), ""),

        shiny::textAreaInput(
            inputId = ns("data"),
            label   = "",
            rows    = 10L,
            resize  = "vertical",
            value   = paste(
                "28.9", "19.4", "<5.5", "149.9", "26.42", "56.1",
                sep = "\n"
            )
        ) |>
        bslib::tooltip(id = ns("data_tooltip"), ""),

        # Buttons --------------------------------------------------------------

        tags$div(
            class = "d-flex justify-content-around",
            style = "gap: 1rem; margin-bottom: 1rem;",

            shiny::actionButton(
                inputId = ns("submit_btn"),
                class   = "btn btn-outline-secondary w-100 app-btn",
                label   = tags$span(
                    bsicons::bs_icon("check-circle-fill", a11y = "deco"),
                    htmltools::tagAppendAttributes(
                        style = "font-size: 1rem;",
                        shiny::textOutput(ns("submit_btn_label"))
                    )
                )
            ) |>
            bslib::tooltip(
                id        = ns("submit_btn_tooltip"),
                placement = "bottom",
                ""
            ),

            shiny::actionButton(
                inputId = ns("clear_btn"),
                class   = "btn btn-outline-secondary w-100 app-btn",
                label   = tags$span(
                    bsicons::bs_icon("x-circle-fill", a11y = "deco"),
                    htmltools::tagAppendAttributes(
                        style = "font-size: 1rem;",
                        shiny::textOutput(ns("clear_btn_label"))
                    )
                )
            ) |>
            bslib::tooltip(
                id        = ns("clear_btn_tooltip"),
                placement = "bottom",
                ""
            )
        ),

        # Warnings -------------------------------------------------------------

        # The <div> ensures that cards grow as expected
        # (without having to deal with fill and fillable
        # details) once they are updated by the server.
        tags$div(
            style = "margin-bottom: 1rem;",

            # Measurements' numbering format.
            bslib::card(
                id    = ns("data_format_card"),
                class = "border-warning bg-warning-subtle small",

                bslib::card_body(
                    shiny::textOutput(ns("data_format_card_text"), tags$p)
                )
            ),

            # Hidden inputs.
            bslib::card(
                id    = ns("hidden_inputs_card"),
                class = "border-info bg-info-subtle small mb-0",

                bslib::card_body(
                    shiny::textOutput(ns("hidden_inputs_card_text"), tags$p)
                )
            )
        ),

        # Footer ---------------------------------------------------------------

        tags$div(
            class = "border-top pt-3",
            ui_footer(ns("footer"))
        )
    )

    return(ui)
}

#' @rdname ui-sidebar
#' @export
server_sidebar <- function(id, lang, panel_active) {
    stopifnot(exprs = {
        shiny::is.reactive(lang)
        shiny::is.reactive(panel_active)
    })

    server <- \(input, output, session) {
        server_footer("footer", lang)

        output$title <- shiny::renderText({
            translate(lang = lang(), "Calculation Parameters")
        }) |>
        shiny::bindCache(lang())

        output$submit_btn_label <- shiny::renderText({
            translate(lang = lang(), "Submit")
        }) |>
        shiny::bindCache(lang())

        output$clear_btn_label <- shiny::renderText({
            translate(lang = lang(), "Clear")
        }) |>
        shiny::bindCache(lang())

        output$data_format_card_text <- shiny::renderText({
            translate(lang = lang(), "
                Always put a leading zero before decimals for numbers strictly
                smaller than one. Always use a dot for decimals. Do not use a
                separator for thousands.
            ")
        }) |>
        shiny::bindCache(lang())

        output$hidden_inputs_card_text <- shiny::renderText({
            translate(lang = lang(), "
                No results are shown in the right panels until inputs are
                submitted.
            ")
        }) |>
        shiny::bindCache(lang())

        # Clear the main <textarea> of input$data.
        shiny::observe({
            shiny::updateTextAreaInput(inputId = "data", value = "")
        }) |>
        shiny::bindEvent(input$clear_btn)

        # Hide warnings once inputs are submitted.
        shiny::observe({
            shinyjs::hide("data_format_card")
            shinyjs::hide("hidden_inputs_card")
        }) |>
        shiny::bindEvent(input$submit_btn)

        # Hide/show inputs that are specific to certain panels.
        # What panel_active returns depends on values passed to
        # arg id of ui_panel*() functions in app.R.
        shiny::observe({
            shinyjs::toggle("frac_threshold", condition = {
                panel_active() == "panel_fraction"
            })
            shinyjs::toggle("target_perc", condition = {
                panel_active() == "panel_percentiles"
            })
        }) |>
        shiny::bindEvent(panel_active())

        # Translate elements not rendered
        # with a shiny::render*() function.
        shiny::observe({
            lang <- lang()

            shiny::updateNumericInput(
                inputId = "oel",
                label   = translate(lang = lang, "Exposure Limit:")
            )

            shiny::updateNumericInput(
                inputId = "conf",
                label   = translate(lang = lang, "Credible Interval Probability:")
            )

            shiny::updateNumericInput(
                inputId = "psi",
                label   = translate(lang = lang, "Overexposure Risk Threshold:")
            )

            shiny::updateNumericInput(
                inputId = "frac_threshold",
                label   = translate(lang = lang, "Exceedance Fraction Threshold:")
            )

            shiny::updateNumericInput(
                inputId = "target_perc",
                label   = translate(lang = lang, "Critical Percentile:")
            )

            shiny::updateTextAreaInput(
                inputId = "data",
                label   = translate(lang = lang, "Measurements:")
            )

            bslib::update_tooltip("oel_tooltip", translate(lang = lang, "
                Use the exposure limit to assess overexposure. It must have the
                same unit as the measurement data.
            "))

            bslib::update_tooltip("conf_tooltip", translate(lang = lang, "
                Use this value as a probability for the credible intervals around
                parameter estimates. It must be between 0% and 100%. The default
                value is set equal to 90%. The credible interval is the Bayesian
                equivalent of the confidence interval.
            "))

            bslib::update_tooltip("psi_tooltip", translate(lang = lang, "
                Use this value as the maximal overexposure risk. It must be
                between 0% and 100%. It represents the maximal probability that
                the overexposure criterion is met. Above this value, the
                situation should trigger remedial action. While 5% is the
                traditional chosen value, recent guidelines suggest using 30%
                instead.
            "))

            bslib::update_tooltip("frac_threshold_tooltip", translate(lang = lang, "
                Use this value as an acceptable proportion of exposures above
                the exposure limit (OEL). It must be between 0% and 100%. The
                traditional default value is 5%.
            "))

            bslib::update_tooltip("target_perc_tooltip", translate(lang = lang, "
                Use this value to set the percentile of the exposure distribution
                that will be compared to the OEL. It must be between 0% and 100%.
                The traditional default value is 95%.
            "))

            bslib::update_tooltip("data_tooltip", translate(lang = lang, "
                The measurement dataset. There must be one value per line. Values
                can be censored to the left (<), to the right (>), or interval
                censored ([X-Y]). For more information, see the Calculation
                Parameters section in Frequently Asked Questions (FAQ) above.
            "))

            bslib::update_tooltip("submit_btn_tooltip", translate(lang = lang, "
                Submit all parameters and start Bayesian calculations.
            "))

            bslib::update_tooltip("clear_btn_tooltip", translate(lang = lang, "
                Clear the measurement dataset. Doing so does not automatically
                update the current results.
            "))
        }) |>
        shiny::bindEvent(lang())

        # Return all inputs except buttons.
        return(
            shiny::reactive({
                list(
                    oel            = input$oel,
                    conf           = input$conf,
                    psi            = input$psi,
                    data           = input$data,
                    frac_threshold = input$frac_threshold,
                    target_perc    = input$target_perc
                )
            }) |>
            shiny::bindEvent(input$submit_btn)
        )
    }

    return(shiny::moduleServer(id, server))
}
