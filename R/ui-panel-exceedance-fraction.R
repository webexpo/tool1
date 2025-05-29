#' Exceedance Fraction Panel Module
#'
#' @description
#' This module controls the Exceedance Fraction panel component. It is currently
#' nested into the application's main [bslib::navset] conceptually illustrated
#' below.
#'
#' ```
#' -------------------------------------------------
#' | Title                                         |
#' -------------------------------------------------
#' | Sidebar | Main                                |
#' |         |  ---------------------------------  |
#' |         |  | Panels Navigation             |  |
#' |         |  ---------------------------------  |
#' |         |  | Active Panel                  |  |
#' |         |  |                               |  |
#' |         |  | Exceedance Fraction Panel     |  |
#' |         |  | (this module)                 |  |
#' |         |  | (shown when active)           |  |
#' |         |  |                               |  |
#' |         |  |                               |  |
#' |         |  |                               |  |
#' |         |  ---------------------------------  |
#' -------------------------------------------------
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
#' @template param-parameters
#'
#' @template param-bayesian-analysis
#'
#' @template param-num-results
#'
#' @template param-estimates-params
#'
#' @returns
#' [ui_panel_exceedance_fraction()] returns a `shiny.tag` object
#' (an output of [bslib::nav_panel()]).
#'
#' [server_panel_exceedance_fraction()] returns a [shiny::reactive()] object.
#' It can be called to get the panel's title.
#'
#' @note
#' This module is used as a general template that is copied and refactored
#' into the Percentiles and Arithmetic Mean panel modules. Changes in this
#' module should be carried into these other two modules.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-panel-exceedance-fraction
#' @export
ui_panel_exceedance_fraction <- function(id) {
    ns <- shiny::NS(id)

    # Risk Assessment ----------------------------------------------------------

    risk_assessment <- bslib::card(
        id     = ns("risk_assessment_card"),
        height = default_card_height_text_only,

        bslib::card_header(
            id = ns("risk_assessment_header"),

            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",

                shiny::uiOutput(
                    outputId  = ns("risk_assessment_icon"),
                    container = tags$span,
                    class     = "pe-2"
                ),

                shiny::textOutput(ns("risk_assessment_title"), tags$span)
            )
        ),

        bslib::card_body(
            shiny::uiOutput(ns("risk_assessment"))
        )
    )

    # Risk Meter ---------------------------------------------------------------

    risk_meter <- bslib::card(
        height      = default_card_height,
        full_screen = TRUE,

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("risk_meter_plot_title"), tags$span)
            )
        ),

        bslib::card_body(
            shiny::plotOutput(ns("risk_meter_plot"))
        ),

        bslib::card_footer(
            shiny::textOutput(ns("risk_meter_plot_desc"), tags$p)
        )
    )

    # Estimates ----------------------------------------------------------------

    estimates <- bslib::card(
        height = default_card_height_text_only,

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("estimates_title"), tags$span)
            )
        ),

        bslib::card_body(
            # Estimates of the underlying distribution parameters.
            tags$div(
                tags$h3(
                    class = "fs-5",
                    shiny::textOutput(ns("estimates_params_title"), tags$span)
                ),

                shiny::uiOutput(
                    outputId   = ns("estimates_params"),
                    container = tags$ul,
                    class     = "list-group list-group-flush"
                )
            ),

            # Estimate of the exceedance fraction.
            tags$div(
                tags$h3(
                    class = "fs-5",
                    shiny::textOutput(ns("estimates_fraction_title"), tags$span)
                ),

                shiny::uiOutput(
                    outputId   = ns("estimates_fraction"),
                    container = tags$ul,
                    class     = "list-group list-group-flush"
                )
            )
        ),

        bslib::card_footer(
            shiny::textOutput(ns("estimates_desc"), tags$p)
        )
    )

    # Sequential Plot ----------------------------------------------------------

    seq_plot <- bslib::card(
        height      = default_card_height,
        full_screen = TRUE,

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("seq_plot_title"), tags$span)
            )
        ),

        bslib::card_body(
            shiny::plotOutput(ns("seq_plot"))
        ),

        bslib::card_footer(
            shiny::textOutput(ns("seq_plot_desc"), tags$p)
        )
    )

    # Density Plot -------------------------------------------------------------

    density_plot <- bslib::card(
        height      = default_card_height,
        full_screen = TRUE,

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("density_plot_title"), tags$span)
            )
        ),

        bslib::card_body(
            shiny::plotOutput(ns("density_plot"))
        ),

        bslib::card_footer(
            shiny::textOutput(ns("density_plot_desc"), tags$p)
        )
    )

    # Risk Band Plot -----------------------------------------------------------

    risk_band_plot <- bslib::card(
        height      = default_card_height,
        full_screen = TRUE,

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("risk_band_plot_title"), tags$span)
            )
        ),

        bslib::card_body(
            shiny::plotOutput(ns("risk_band_plot"))
        ),

        bslib::card_footer(
            shiny::uiOutput(ns("risk_band_plot_desc"), container = tags$p)
        )
    )

    # Panel --------------------------------------------------------------------

    ui <- bslib::nav_panel(
        value = id,
        title = shiny::textOutput(ns("title"), tags$span),

        bslib::layout_column_wrap(
            width         = 1/2,
            fill          = FALSE,
            heights_equal = "row",

            risk_assessment,
            estimates,
            risk_meter,
            seq_plot,
            density_plot,
            risk_band_plot
        ),

        ui_exceedance_plot(ns("exceedance_plot"))
    )

    return(ui)
}

#' @rdname ui-panel-exceedance-fraction
#' @export
server_panel_exceedance_fraction <- function(
    id,
    lang,
    parameters,
    bayesian_analysis,
    num_results,
    estimates_params)
{
    stopifnot(exprs = {
        shiny::is.reactive(lang)
        shiny::is.reactive(parameters)
        shiny::is.reactive(bayesian_analysis)
        shiny::is.reactive(num_results)
        shiny::is.reactive(estimates_params)
    })

    server <- function(input, output, session) {
        server_exceedance_plot(
            id          = "exceedance_plot",
            lang        = lang,
            parameters  = parameters,
            num_results = num_results
        )

        title <- shiny::reactive({
            translate(lang = lang(), "Exceedance Fraction")
        }) |>
        shiny::bindCache(lang())

        risk_assessment <- shiny::reactive({
            risk_level <- if (num_results()$frac.risk >= parameters()$psi) {
                "problematic"
            } else {
                "acceptable"
            }

            aiha_risk_levels$metadata[[risk_level]]
        })

        output$title <- shiny::renderText({
            title()
        })

        output$risk_assessment_title <- shiny::renderText({
            translate(lang = lang(), "Risk Assessment")
        }) |>
        shiny::bindCache(lang())

        output$risk_meter_plot_title <- shiny::renderText({
            translate(lang = lang(), "Risk Meter")
        }) |>
        shiny::bindCache(lang())

        output$estimates_title <- shiny::renderText({
            translate(lang = lang(), "Estimates")
        }) |>
        shiny::bindCache(lang())

        output$estimates_params_title <- shiny::renderText({
            translate(lang = lang(), "Distribution Parameters")
        }) |>
        shiny::bindCache(lang())

        output$estimates_fraction_title <- shiny::renderText({
            translate(lang = lang(), "Exceedance Fraction")
        }) |>
        shiny::bindCache(lang())

        output$seq_plot_title <- shiny::renderText({
            translate(lang = lang(), "Sequential Plot")
        }) |>
        shiny::bindCache(lang())

        output$density_plot_title <- shiny::renderText({
            translate(lang = lang(), "Density Plot")
        }) |>
        shiny::bindCache(lang())

        output$risk_band_plot_title <- shiny::renderText({
            translate(lang = lang(), "Risk Band Plot")
        }) |>
        shiny::bindCache(lang())

        output$risk_assessment <- shiny::renderUI({
            lang <- lang()
            parameters <- parameters()
            num_results <- num_results()
            risk_assessment <- risk_assessment()

            li_classes <- sprintf(
                "list-group-item bg-%s-subtle border-%1$s",
                risk_assessment$color
            )

            tags$ul(
                class = sprintf(
                    "list-group list-group-flush bg-%s-subtle border-%1$s",
                    risk_assessment$color
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "
                            Overexposure is defined as the exceedance fraction
                            being greater than or equal to %s.
                        "),
                        tags$strong(as_percentage(parameters$frac_threshold))
                    )
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "
                            The probability that this criterion is met is equal
                            to %s.
                        "),
                        tags$strong(as_percentage(num_results$frac.risk))
                    )
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "
                            The probability that this criterion is met should
                            be lower than %s.
                        "),
                        tags$strong(as_percentage(parameters$psi))
                    )
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "The current situation is %s."),
                        tags$strong(risk_assessment$text(lang))
                    )
                )
            )
        })

        output$risk_assessment_icon <- shiny::renderUI({
            risk_assessment()$icon
        })

        output$risk_meter_plot <- shiny::renderPlot({
            dessinerRisqueMetre(
                actualProb          = num_results()$frac.risk,
                minProbUnacceptable = parameters()$psi
            )
        })

        output$risk_meter_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This risk meter shows the probability of the exposure being too
                high when compared to the OEL. The red zone indicates a
                problematic exposure.
            ")
        }) |>
        shiny::bindCache(lang())

        output$estimates_params <- shiny::renderUI({
            lang <- lang()
            estimates_params <- estimates_params()

            list(
                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of the geometric mean is equal
                            to %s.
                        "),
                        tags$strong(estimates_params$gm)
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of the geometric standard
                            deviation is equal to %s.
                        "),
                        tags$strong(estimates_params$gsd)
                    )
                )
            )
        })

        output$estimates_fraction <- shiny::renderUI({
            lang <- lang()
            frac <- lapply(num_results()$frac, as_percentage)

            list(
                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of the exceedance fraction is
                            equal to %s.
                        "),
                        tags$strong(
                            sprintf("%s [%s - %s]", frac$est, frac$lcl, frac$ucl)
                        )
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The 70%% upper confidence limit is equal to %s.
                        "),
                        tags$strong(
                            as_percentage(num_results()$frac.ucl70)
                        )
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The 95%% upper confidence limit is equal to %s.
                        "),
                        tags$strong(
                            as_percentage(num_results()$frac.ucl95)
                        )
                    )
                )
            )
        })

        output$estimates_desc <- shiny::renderText({
            translate(lang = lang(), "
                Credible intervals are shown in square brackets.
            ")
        }) |>
        shiny::bindCache(lang())

        output$seq_plot <- shiny::renderPlot({
            lang <- lang()
            results <- num_results()

            sequential.plot.frac(
                gm        = results$gm$est,
                gsd       = results$gsd$est,
                frac      = results$frac$est,
                c.oel     = parameters()$oel,
                seqplot.1 = translate(lang = lang, "Concentration"),
                seqplot.2 = translate(lang = lang, "Exceedance Fraction"),
                seqplot.6 = translate(lang = lang, "Measurement Index")
            )
        })

        output$seq_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This plot shows the estimated exposure distribution when assuming
                250 exposure measurements have been collected. If the measurements
                represent 8-hour TWA (Time-Weighted Average) values, this
                approximately represents a full year of exposure. The OEL is shown
                as a red line.
            ")
        }) |>
        shiny::bindCache(lang())

        output$density_plot <- shiny::renderPlot({
            lang <- lang()
            bayesian_analysis <- bayesian_analysis()

            distribution.plot.frac(
                gm         = exp(median(bayesian_analysis$mu.chain)),
                gsd        = exp(median(bayesian_analysis$sigma.chain)),
                frac       = num_results()$frac$est,
                c.oel      = parameters()$oel,
                distplot.1 = translate(lang = lang, "Concentration"),
                distplot.2 = translate(lang = lang, "Density"),
                distplot.3 = translate(lang = lang, "Exceedance Fraction"),
                distplot.4 = translate(lang = lang, "OEL outside of graphical limits."),
                distplot.5 = translate(lang = lang, "OEL")
            )
        })

        output$density_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This plot shows the probability density function of the
                estimated distribution of exposures. The OEL is shown as a
                red line. The exceedance fraction is the area under the curve
                beyond the OEL value.
            ")
        }) |>
        shiny::bindCache(lang())

        output$risk_band_plot <- shiny::renderPlot({
            lang <- lang()
            parameters <- parameters()
            bayesian_analysis <- bayesian_analysis()

            riskband.plot.frac(
                mu.chain       = bayesian_analysis$mu.chain,
                sigma.chain    = bayesian_analysis$sigma.chain,
                c.oel          = parameters$oel,
                frac_threshold = parameters$frac_threshold,
                psi            = parameters$psi,
                riskplot.1     = translate(lang = lang, "Exceedance Fraction Category"),
                riskplot.2     = translate(lang = lang, "Probability")
            )
        })

        output$risk_band_plot_desc <- shiny::renderUI({
            # These variables are used as semantic sugar
            # because it may not be ovious what the source
            # text refers to at first glance.
            frac_threshold <- parameters()$frac_threshold
            lower_limit <- as_percentage(frac_threshold / 10L)
            upper_limit <- as_percentage(frac_threshold)

            html(
                translate(lang = lang(), "
                    This plot shows the probability distribution of the
                    uncertainty around the exceedance fraction. It shows
                    the probability that its true value is
                    (1) below %s,
                    (2) between %1$s and %2$s, and
                    (3) greater than %s.
                    The red column represents the probability of an
                    overexposure. The latter should be lower than the
                    threshold (black dashed line).
                "),
                lower_limit,
                upper_limit
            )
        })

        # Update colors of borders and background
        # of the risk assessment card based on the
        # risk level.
        shiny::observe({
            risk_level <- risk_assessment()$level
            color_acceptable  <- aiha_risk_levels$metadata$acceptable$color
            color_problematic <- aiha_risk_levels$metadata$problematic$color

            # Use green colors if the risk is acceptable.
            shinyjs::toggleClass(
                id        = "risk_assessment_header",
                class     = sprintf("border-%s text-%1$s", color_acceptable),
                condition = { risk_level == "acceptable" }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_card",
                class     = sprintf("border-%s bg-%1$s-subtle", color_acceptable),
                condition = { risk_level == "acceptable" }
            )

            # Use red colors if the risk is problematic.
            shinyjs::toggleClass(
                id        = "risk_assessment_header",
                class     = sprintf("border-%s text-%1$s", color_problematic),
                condition = { risk_level == "problematic" }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_card",
                class     = sprintf("border-%s bg-%1$s-subtle", color_problematic),
                condition = { risk_level == "problematic" }
            )
        }) |>
        shiny::bindEvent(risk_assessment())

        return(title)
    }

    return(shiny::moduleServer(id, server))
}
