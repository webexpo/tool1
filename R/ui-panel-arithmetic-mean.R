#' Arithmetic Mean Panel Module
#'
#' @description
#' This module controls the Arithmetic Mean panel component. It is currently
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
#' |         |  | Arithmetic Mean Panel         |  |
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
#' [ui_panel_arithmetic_mean()] returns a `shiny.tag` object
#' (an output of [bslib::nav_panel()]).
#'
#' [server_panel_arithmetic_mean()] returns `NULL`, invisibly.
#'
#' @note
#' This module is almost identical to the Exceedance Fraction panel module
#' (the latter was copied and very lightly refactored).
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-panel-arithmetic-mean
#' @export
ui_panel_arithmetic_mean <- function(id) {
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
            # Position the warning at the bottom.
            # Since fillable is TRUE, this card's
            # body is already a flexbox container.
            class = "justify-content-between",

            shiny::uiOutput(ns("risk_assessment")),

            # Embedded card for warning. It is shown once
            # risk_assessment() is updated for the second time.
            bslib::card(
                id    = ns("risk_assessment_warning_card"),
                fill  = FALSE,
                class = "bg-warning-subtle border-warning small",

                bslib::card_body(
                    shiny::textOutput(ns("risk_assessment_warning"), tags$p)
                )
            ) |>
            shinyjs::hidden()
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

            # Estimate of the arithmetic mean.
            tags$div(
                tags$h3(
                    class = "fs-5",
                    shiny::textOutput(ns("estimates_mean_title"), tags$span)
                ),

                shiny::uiOutput(
                    outputId   = ns("estimates_mean"),
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
            shiny::textOutput(ns("risk_band_plot_desc"), container = tags$p)
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
        )
    )

    return(ui)
}

#' @rdname ui-panel-arithmetic-mean
#' @export
server_panel_arithmetic_mean <- function(
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
        risk_assessment <- shiny::reactive({
            risk_level <- if (num_results()$am.risk >= parameters()$psi) {
                "problematic"
            } else {
                "acceptable"
            }

            aiha_risk_levels$metadata[[risk_level]]
        })

        output$title <- shiny::renderText({
            translate(lang = lang(), "Arithmetic Mean")
        }) |>
        shiny::bindCache(lang())

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

        output$estimates_mean_title <- shiny::renderText({
            translate(lang = lang(), "Arithmetic Mean")
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
                    translate(lang = lang, "
                        Overexposure is defined as the arithmetic mean
                        being greater than or equal to the OEL.
                    ")
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "
                            The probability that this criterion is met is equal
                            to %s.
                        "),
                        tags$strong(as_percentage(num_results$am.risk))
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

        output$risk_assessment_warning <- shiny::renderText({
            translate(lang = lang(), "
                Risk assessment based on the arithmetic mean relies on the
                availability of a long-term averaged OEL (LTA-OEL in the
                AIHA terminology) representing a cumulative burden threshold.
                Most current OELs are not created as LTA-OEL.

                Despite an annoying lack of precise definition by most
                organizations, they should be most often viewed as thresholds
                to be exceeded as rarely as possible. Some authors have
                suggested using one-tenth of the OEL as a practical LTA-OEL
                when assessing risk using the arithmetic mean.
            ")
        }) |>
        shiny::bindCache(lang())

        output$risk_assessment_icon <- shiny::renderUI({
            risk_assessment()$icon
        })

        output$risk_meter_plot <- shiny::renderPlot({
            dessinerRisqueMetre(
                actualProb          = num_results()$am.risk,
                minProbUnacceptable = parameters()$psi
            )
        })

        output$risk_meter_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This risk meter shows the probability of the exposure being too
                high when compared to the occupational exposure limit. The red
                zone indicates a problematic exposure.
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

        output$estimates_mean <- shiny::renderUI({
            lang <- lang()
            am <- lapply(num_results()$am, signif, digits = default_n_digits)

            list(
                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of the arithmetic mean is
                            equal to %s.
                        "),
                        tags$strong(
                            sprintf("%s [%s - %s]", am$est, am$lcl, am$ucl)
                        )
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of its 70%% upper confidence
                            limit is equal to %s.
                        "),
                        tags$strong(
                            signif(num_results()$am.ucl70, default_n_digits)
                        )
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of its 95%% upper confidence
                            limit is equal to %s.
                        "),
                        tags$strong(
                            signif(num_results()$am.ucl95, default_n_digits)
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

            sequential.plot.am(
                gm        = results$gm$est,
                gsd       = results$gsd$est,
                am        = results$am$est,
                c.oel     = parameters()$oel,
                seqplot.1 = translate(lang = lang, "Concentration"),
                seqplot.3 = translate(lang = lang, "OEL"),
                seqplot.5 = translate(lang = lang, "Arithmetic Mean"),
                seqplot.6 = translate(lang = lang, "Measurement Index")
            )
        })

        output$seq_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This plot shows the estimated exposure distribution when
                assuming 250 exposure measurements have been collected. If
                the measurements represent 8-hour TWA (Time-Weighted Average)
                values, this approximately represents a full year of exposure.
                The OEL is shown as a red dotted line and the point estimate
                of the arithmetic mean as a continuous green line.
            ")
        }) |>
        shiny::bindCache(lang())

        output$density_plot <- shiny::renderPlot({
            lang <- lang()
            bayesian_analysis <- bayesian_analysis()

            distribution.plot.am(
                gm         = exp(median(bayesian_analysis$mu.chain)),
                gsd        = exp(median(bayesian_analysis$sigma.chain)),
                am         = num_results()$am$est,
                c.oel      = parameters()$oel,
                distplot.1 = translate(lang = lang, "Concentration"),
                distplot.2 = translate(lang = lang, "Density"),
                distplot.4 = translate(lang = lang, "OEL outside of graphical limits."),
                distplot.5 = translate(lang = lang, "OEL"),
                distplot.7 = translate(lang = lang, "Arithmetic Mean")
            )
        })

        output$density_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This plot shows the probability density function of the
                estimated distribution of exposures. The OEL is shown as a
                red dotted line and the point estimate of the arithmetic mean
                as a continuous green line.
            ")
        }) |>
        shiny::bindCache(lang())

        output$risk_band_plot <- shiny::renderPlot({
            lang <- lang()
            parameters <- parameters()
            bayesian_analysis <- bayesian_analysis()

            riskband.plot.am(
                mu.chain    = bayesian_analysis$mu.chain,
                sigma.chain = bayesian_analysis$sigma.chain,
                c.oel       = parameters$oel,
                psi         = parameters$psi,
                riskplot.2  = translate(lang = lang, "Probability"),
                riskplot.3  = translate(lang = lang, "≤ 1% OEL"),
                riskplot.4  = translate(lang = lang, "1% < OEL ≤ 10%"),
                riskplot.5  = translate(lang = lang, "10% < OEL ≤ 50%"),
                riskplot.6  = translate(lang = lang, "50% < OEL ≤ 100%"),
                riskplot.7  = translate(lang = lang, "> OEL"),
                riskplot.9  = translate(lang = lang, "Arithmetic Mean Category")
            )
        })

        output$risk_band_plot_desc <- shiny::renderUI({
            html(
                translate(lang = lang(), "
                    This plot shows the probability distribution of the
                    uncertainty around the arithmetic mean. It shows the
                    probability that its true value is
                    (1) below 1%% of the OEL,
                    (2) between 1%% and 10%% of the OEL,
                    (3) between 10%% and 50%% of the OEL,
                    (4) between 50%% and 100%% of the OEL, and
                    (5) greater than the OEL.
                    This is based on the classification adopted by %s. The red
                    column represents the probability of an overexposure. The
                    latter should be lower than the threshold (black dashed line).
                "),
                ui_link(default_urls$aiha, "AIHA")
            )
        }) |>
        shiny::bindCache(lang())

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

        # Show the warning sub-card in the main
        # risk_assessment card when it is rendered.
        shiny::observe({
            shinyjs::toggle("risk_assessment_warning_card")
        }) |>
        shiny::bindEvent(risk_assessment(), once = TRUE)

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}
