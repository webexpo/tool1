#' Exceedance Plot Module
#'
#' This module controls the Exceedance Plot component. It is currently nested
#' into the Exceedance Fraction panel module. It itself relies on another
#' module, the Exceedance Plot Sidebar module which controls customization
#' paramaters. The latter is currently nested into the former.
#'
#' @template param-id
#'
#' @template param-lang
#'
#' @template param-inputs-calc
#'
#' @template param-results
#'
#' @returns
#' [ui_exceedance_plot()] returns a `bslib_fragment` object
#' (an output of [bslib::card()]).
#'
#' [ui_exceedance_plot_sidebar()] returns a `bslib_sidebar` object
#' (an output of [bslib::sidebar()]).
#'
#' [server_exceedance_plot()] returns `NULL`, invisibly.
#'
#' [server_exceedance_plot_sidebar()] returns a [shiny::reactive()] object.
#' For more information, consult the source text passed to related tooltips
#' in [server_exceedance_plot_sidebar()] below.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-exceedance-plot
#' @export
ui_exceedance_plot <- function(id) {
    ns <- shiny::NS(id)
    ui <- bslib::card(
        fill        = FALSE,
        full_screen = TRUE,
        # The card may have to grow vertically
        # beyond the usual threshold to avoid
        # vertical overflowing of the sidebar.
        min_height = getOption("app_card_height"),

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("title"), tags$span)
            )
        ),

        bslib::card_header(
            class = "fw-normal",
            shiny::textOutput(ns("description"), container = tags$p)
        ),

        # The body of the card is a two-columns
        # layout with a sidebar on the right for
        # customization.
        bslib::layout_sidebar(
            sidebar = ui_exceedance_plot_sidebar(ns("sidebar")),
            shiny::plotOutput(ns("plot"))
        ),

        bslib::card_footer(
            shiny::textOutput(ns("description_variant"), container = tags$p)
        )
    )

    return(ui)
}

#' @rdname ui-exceedance-plot
#' @export
server_exceedance_plot <- function(id, lang, inputs_calc, results) {
    stopifnot(exprs = {
        shiny::is.reactive(lang)
        shiny::is.reactive(inputs_calc)
        shiny::is.reactive(results)
    })

    server <- function(input, output, session) {
        inputs_exceedance_plot <- server_exceedance_plot_sidebar("sidebar", lang)

        output$title <- shiny::renderText({
            translate(lang = lang(), "Exceedance Plot")
        }) |>
        shiny::bindCache(lang())

        output$plot <- shiny::renderPlot({
            path_dir_images <- getOption("app_path_dir_images")

            lang <- lang()
            results <- results()
            frac_threshold <- inputs_calc()$frac_threshold
            inputs_exceedance_plot <- inputs_exceedance_plot()

            frac_estimate <- ceiling(results$frac$est)
            frac_ucl <- ceiling(results$frac$ucl)

            params_plots <- paramsVariantesFracDep(
                path_dir_images,
                file.path(path_dir_images, "flask.png"),
                file.path(path_dir_images, "flask-lines.png"),
                inputs_exceedance_plot$color_risk,
                inputs_exceedance_plot$color_no_risk,
                inputs_exceedance_plot$color_bg_threshold,
                inputs_exceedance_plot$color_bg
            )

            # gridExtra::grid.arrange() expects
            # a list even if it is of length 1.
            plots <- switch(inputs_exceedance_plot$variant,
                plot1 = list(
                    drawPlot(
                        params_plots,
                        fracDepasseEst = frac_threshold,
                        titre          = translate(lang = lang, "Acceptable Sample")
                    ),
                    drawPlot(
                        params_plots,
                        fracDepasseEst = frac_estimate,
                        titre          = translate(lang = lang, "Current Sample")
                    )
                ),
                plot2 = list(
                    drawPlot(
                        params_plots,
                        fracDepasseEst = frac_threshold,
                        titre          = translate(lang = lang, "Acceptable Sample")
                    ),
                    drawPlot(
                        params_plots,
                        fracDepasseEst = frac_estimate,
                        fracDepasseLim = frac_ucl,
                        titre          = translate(lang = lang, "Current Sample")
                    )
                ),
                plot3 = list(
                    drawPlot(
                        params_plots,
                        fracDepasseEst = frac_estimate,
                        seuil          = frac_threshold
                    )
                ),
                plot4 = list(
                    drawPlot(
                        params_plots,
                        fracDepasseEst = frac_estimate,
                        fracDepasseLim = frac_ucl,
                        seuil          = frac_threshold
                    )
                )
            )

            return(gridExtra::grid.arrange(grobs = plots, ncol = length(plots)))
        })

        output$description <- shiny::renderText({
            translate(lang = lang(), "
                This plot illustrates the proportion of exposures that
                would be above the OEL in a fictional sample of one hundred
                measurements. Each flask represents an exposure value. Those
                that are above the OEL are colored in red. This can be changed
                according to your needs using the sidebar on the right. Four
                variants of the Exceedance Plot are available.
            ")
        }) |>
        shiny::bindCache(lang())

        output$description_variant <- shiny::renderText({
            switch(inputs_exceedance_plot()$variant,
                plot1 = translate(lang = lang(), "
                    The plot on the left shows an acceptable situation for the
                    chosen exceedance threshold (traditionally 5% above the OEL).
                    The plot on the right shows the situation estimated by the
                    Bayesian model. It does not take into account estimation
                    uncertainty.
                "),
                plot2 = translate(lang = lang(), "
                    The plot on the left shows an acceptable situation for the
                    chosen exceedance threshold (traditionally 5% above the
                    OEL). The plot on the right shows the situation estimated
                    by the Bayesian model. It further illustrates the uncertainty
                    of the estimation with stripped flasks. The number of red
                    plain flasks represents the best estimate of the number of
                    measurements above the OEL. The total number of red flasks
                    (either plain or stripped) represents the maximum plausible
                    number of measurements above the OEL given the uncertainty
                    of the estimation process (using the upper limit of the
                    underlying credible interval).
                "),
                plot3 = translate(lang = lang(), "
                    This plot shows a shaded and darker region corresponding to
                    the maximal acceptable exceedance. Red flasks outside of it
                    are unacceptable exposures. It does not take into account
                    estimation uncertainty.
                "),
                plot4 = translate(lang = lang(), "
                    This plot shows a shaded and darker region corresponding to
                    the maximal acceptable exceedance. Red symbols outside of it
                    are unacceptable exposures. It further illustrates the
                    uncertainty of the estimation with stripped flasks. The
                    number of red plain flasks represents the best estimate of
                    the number of measurements above the OEL. The total number
                    of red flasks (either plain or stripped) represents the
                    maximum plausible number of measurements above the OEL given
                    the uncertainty of the estimation process (using the upper
                    limit of the underlying credible interval).
                ")
            )
        }) |>
        shiny::bindCache({ inputs_exceedance_plot()$variant }, lang())

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}

#' @rdname ui-exceedance-plot
#' @export
ui_exceedance_plot_sidebar <- function(id) {
    ns <- shiny::NS(id)
    ui <- bslib::sidebar(
        # Width is set to be 100px
        # less than the main sidebar.
        width    = "375px",
        position = "right",
        open     = list(
            mobile  = "closed",
            desktop = "open"
        ),

        # Title ----------------------------------------------------------------

        # Setting class sidebar-title is recommended by bslib.
        # bslib further automatically changes styling of title
        # when a bslib::accordion() is passed to bslib::sidebar()
        # (this is undocumented). Title is reset to its expected
        # state with classes border-bottom and pb-3.
        title = tags$h2(
            class = "sidebar-title text-center border-bottom pb-3",

            tags$span(
                class = "pe-2",
                bsicons::bs_icon("palette2", a11y = "deco")
            ),

            shiny::textOutput(ns("title"), tags$span)
        ),

        # Inputs ---------------------------------------------------------------

        # Semantic labels for choices are inserted by the
        # server function (based on the current language).
        shiny::selectInput(
            inputId  = ns("variant"),
            label    = "",
            selected = "plot1",
            choices  = c(
                "plot1",
                "plot2",
                "plot3",
                "plot4"
            )
        ) |>
        bslib::tooltip(id = ns("variant_tooltip"), ""),

        bslib::accordion(
            id       = ns("accordion"),
            open     = TRUE,
            multiple = TRUE,

            # Colors of exposures.
            bslib::accordion_panel(
                value = "exposure",
                title = "",
                icon  = bsicons::bs_icon("palette-fill", a11y = "deco"),

                colourpicker::colourInput(
                    inputId    = ns("color_no_risk"),
                    label      = "",
                    value      = "gray50",
                    returnName = TRUE,
                    palette    = "limited"
                ) |>
                bslib::tooltip(id = ns("color_no_risk_tooltip"), ""),

                colourpicker::colourInput(
                    inputId    = ns("color_risk"),
                    label      = "",
                    value      = "red",
                    returnName = TRUE,
                    palette    = "limited"
                ) |>
                bslib::tooltip(id = ns("color_risk_tooltip"), "")
            ),

            # Colors of backgrounds.
            bslib::accordion_panel(
                value = "background",
                title = "",
                icon  = bsicons::bs_icon("palette-fill", a11y = "deco"),

                colourpicker::colourInput(
                    inputId    = ns("color_bg"),
                    label      = "",
                    value      = "gray70",
                    returnName = TRUE,
                    palette    = "limited"
                ) |>
                bslib::tooltip(id = ns("color_bg_tooltip"), ""),

                colourpicker::colourInput(
                    inputId    = ns("color_bg_threshold"),
                    label      = "",
                    value      = "gray40",
                    returnName = TRUE,
                    palette    = "limited"
                ) |>
                shinyjs::hidden() |>
                bslib::tooltip(id = ns("color_bg_threshold_tooltip"), "")
            )
        )
    )

    return(ui)
}

#' @rdname ui-exceedance-plot
#' @export
server_exceedance_plot_sidebar <- function(id, lang) {
    stopifnot(shiny::is.reactive(lang))

    server <- function(input, output, session) {
        accordion_panel_exposure_title <- shiny::reactive({
            translate(lang = lang(), "Flasks (exposures)")
        }) |>
        shiny::bindCache(lang())

        accordion_panel_background_title <- shiny::reactive({
            translate(lang = lang(), "Backgrounds")
        }) |>
        shiny::bindCache(lang())

        variant_labels <- shiny::reactive({
            translate(lang = lang(), "Variant:")
        }) |>
        shiny::bindCache(lang())

        variant_choices <- shiny::reactive({
            lang <- lang()
            structure(
                # Names are the labels that users see.
                # Values are what input$variant returns.
                # They must not be changed.
                c(
                    "plot1",
                    "plot2",
                    "plot3",
                    "plot4"
                ),
                names = c(
                    translate(lang = lang, "Two plots (no uncertainty)"),
                    translate(lang = lang, "Two plots (with uncertainty)"),
                    translate(lang = lang, "Single plot (no uncertainty)"),
                    translate(lang = lang, "Single plot (with uncertainty)")
                )
            )
        }) |>
        shiny::bindCache(lang())

        color_no_risk_label <- shiny::reactive({
            translate(lang = lang(), "Below the OEL:")
        }) |>
        shiny::bindCache(lang())

        color_risk_label <- shiny::reactive({
            translate(lang = lang(), "Above the OEL:")
        }) |>
        shiny::bindCache(lang())

        color_bg_label <- shiny::reactive({
            translate(lang = lang(), "Default Background:")
        }) |>
        shiny::bindCache(lang())

        color_bg_threshold_label <- shiny::reactive({
            translate(lang = lang(), "Maximal Acceptable Exceedance:")
        }) |>
        shiny::bindCache(lang())

        variant_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Use this value to change the displayed plot. See the footer
                below for additional details on it.
            ")
        }) |>
        shiny::bindEvent(lang())

        color_no_risk_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Use this value to change the color of exposures that are
                below the OEL.
            ")
        }) |>
        shiny::bindEvent(lang())

        color_risk_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Use this value to change the color of exposures that are
                above the OEL.
            ")
        }) |>
        shiny::bindEvent(lang())

        color_bg_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Use this value to change the global background color.
            ")
        }) |>
        shiny::bindEvent(lang())

        color_bg_threshold_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Use this value to change the background color of the maximal
                acceptable exceedance region (maximal acceptable number of
                exposures that are above the OEL).
            ")
        }) |>
        shiny::bindEvent(lang())

        output$title <- shiny::renderText({
            translate(lang = lang(), "Customize")
        }) |>
        shiny::bindCache(lang())

        shiny::observe({
            shinyjs::toggle("color_bg_threshold", condition = {
                input$variant == "plot3" || input$variant == "plot4"
            })
        }) |>
        shiny::bindEvent(input$variant)

        # Translate elements not rendered
        # with a shiny::render*() function.
        shiny::observe({
            bslib::accordion_panel_update(
                id     = "accordion",
                target = "exposure",
                title  = accordion_panel_exposure_title()
            )

            bslib::accordion_panel_update(
                id     = "accordion",
                target = "background",
                title  = accordion_panel_background_title()
            )

            shiny::updateSelectInput(
                inputId  = "variant",
                label    = variant_labels(),
                choices  = variant_choices(),
                selected = input$variant
            )

            colourpicker::updateColourInput(
                session = session,
                inputId = "color_no_risk",
                label   = color_no_risk_label()
            )

            colourpicker::updateColourInput(
                session = session,
                inputId = "color_risk",
                label   = color_risk_label()
            )

            colourpicker::updateColourInput(
                session = session,
                inputId = "color_bg",
                label   = color_bg_label()
            )

            colourpicker::updateColourInput(
                session = session,
                inputId = "color_bg_threshold",
                label   = color_bg_threshold_label()
            )

            bslib::update_tooltip("variant_tooltip", variant_tooltip_text())
            bslib::update_tooltip("color_no_risk_tooltip", color_no_risk_tooltip_text())
            bslib::update_tooltip("color_risk_tooltip", color_risk_tooltip_text())
            bslib::update_tooltip("color_bg_tooltip", color_bg_tooltip_text())
            bslib::update_tooltip("color_bg_threshold_tooltip", color_bg_threshold_tooltip_text())
        }) |>
        shiny::bindEvent(lang())

        # Return all inputs.
        return(
            shiny::reactive({
                list(
                    variant            = input$variant,
                    color_no_risk      = input$color_no_risk,
                    color_risk         = input$color_risk,
                    color_bg           = input$color_bg,
                    color_bg_threshold = input$color_bg_threshold
                )
            })
        )
    }

    return(shiny::moduleServer(id, server))
}
