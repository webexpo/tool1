#' Percentiles Panel Module
#'
#' @description
#' This module controls the Percentiles panel component. It is currently nested
#' into the application's main [bslib::navset] conceptually illustrated below.
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
#' |         |  | Percentiles Panel             |  |
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
#' [ui_panel_percentiles()] returns a `shiny.tag` object
#' (an output of [bslib::nav_panel()]).
#'
#' [server_panel_percentiles()] returns `NULL`, invisibly.
#'
#' @note
#' This module is almost identical to the Exceedance Fraction panel module
#' (the latter was copied and very lightly refactored).
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-panel-percentiles
#' @export
ui_panel_percentiles <- function(id) {
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

            # Estimate of the percentile.
            tags$div(
                tags$h3(
                    class = "fs-5",
                    shiny::textOutput(ns("estimates_percentile_title"), tags$span)
                ),

                shiny::uiOutput(
                    outputId   = ns("estimates_percentile"),
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

#' @rdname ui-panel-percentiles
#' @export
server_panel_percentiles <- function(
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
        # Assess whether the situation is controlled or
        # not, and return miscellaneous values based on
        # the result. They are used to modify the UI
        # accordingly.
        risk_assessment <- shiny::reactive({
            if (num_results()$perc.risk >= parameters()$psi) {
                list(
                    is_controlled = FALSE,
                    text          = translate(lang = lang(), "poorly controlled"),
                    bs_base_color = "danger",
                    icon          = bsicons::bs_icon(
                        name = "exclamation-diamond",
                        a11y = "deco"
                    )
                )
            } else {
                list(
                    is_controlled = TRUE,
                    text          = translate(lang = lang(), "adequately controlled"),
                    bs_base_color = "success",
                    icon          = bsicons::bs_icon(
                        name = "check-circle",
                        a11y = "deco"
                    )
                )
            }
        })

        output$title <- shiny::renderText({
            translate(lang = lang(), "Percentiles")
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

        output$estimates_percentile_title <- shiny::renderText({
            translate(lang = lang(), "Percentile")
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
                risk_assessment$bs_base_color
            )

            tags$ul(
                class = sprintf(
                    "list-group list-group-flush bg-%s-subtle border-%1$s",
                    risk_assessment$bs_base_color
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "
                            Overexposure is defined as the %s percentile
                            being greater than or equal to the OEL.
                        "),

                        tags$span(
                            class = "fw-bold",
                            ordinal(parameters$target_perc, lang)
                        )
                    )
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "
                            The probability that this criterion is met is equal
                            to %s.
                        "),

                        tags$span(
                            class = "fw-bold",
                            as_percentage(num_results$perc.risk)
                        )
                    )
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "
                            The probability that this criterion is met should
                            be lower than %s.
                        "),

                        tags$span(
                            class = "fw-bold",
                            as_percentage(parameters$psi)
                        )
                    )
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "The current situation is %s."),

                        tags$span(
                            class = "fw-bold",
                            risk_assessment$text
                        )
                    )
                )
            )
        })

        output$risk_assessment_icon <- shiny::renderUI({
            risk_assessment()$icon
        })

        output$risk_meter_plot <- shiny::renderPlot({
            dessinerRisqueMetre(
                actualProb          = num_results()$perc.risk,
                minProbUnacceptable = parameters()$psi
            )
        })

        output$risk_meter_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This risk meter shows the probability of the exposure being too
                high when compared to the occupational exposure limit. The red
                zone indicates a poorly controlled exposure.
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

                        tags$span(
                            class = "fw-bold",
                            estimates_params$gm
                        )
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of the geometric standard
                            deviation is equal to %s.
                        "),

                        tags$span(
                            class = "fw-bold",
                            estimates_params$gsd
                        )
                    )
                )
            )
        })

        output$estimates_percentile <- shiny::renderUI({
            lang <- lang()
            perc <- lapply(num_results()$perc, signif, digits = default_n_digits)

            list(
                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of the %s percentile is
                            equal to %s.
                        "),

                        tags$span(
                            ordinal(parameters()$target_perc, lang)
                        ),

                        tags$span(
                            class = "fw-bold",
                            sprintf("%s%% [%s - %s]", perc$est, perc$lcl, perc$ucl)
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

                        tags$span(
                            class = "fw-bold",
                            signif(num_results()$perc.ucl70, default_n_digits)
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

                        tags$span(
                            class = "fw-bold",
                            signif(num_results()$perc.ucl95, default_n_digits)
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
            parameters <- parameters()
            results <- num_results()

            sequential.plot.perc(
                gm                 = results$gm$est,
                gsd                = results$gsd$est,
                perc               = results$perc$est,
                c.oel              = parameters$oel,
                target_perc        = parameters$target_perc,
                target_perc_suffix = ordinal_abbr(parameters$target_perc, lang),
                seqplot.1          = translate(lang = lang, "Concentration"),
                seqplot.3          = translate(lang = lang, "OEL"),
                seqplot.4          = translate(lang = lang, "Percentile"),
                seqplot.6          = translate(lang = lang, "Measurement Index")
            )
        })

        output$seq_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This plot shows the estimated exposure distribution when
                assuming 250 exposure measurements have been collected. If
                the measurements represent 8-hour TWA (Time-Weighted Average)
                values, this approximately represents a full year of exposure.
                The OEL is shown as a red line.
            ")
        }) |>
        shiny::bindCache(lang())

        output$density_plot <- shiny::renderPlot({
            lang <- lang()
            parameters <- parameters()
            bayesian_analysis <- bayesian_analysis()

            distribution.plot.perc(
                gm                 = exp(median(bayesian_analysis$mu.chain)),
                gsd                = exp(median(bayesian_analysis$sigma.chain)),
                perc               = num_results()$perc$est,
                target_perc        = parameters$target_perc,
                target_perc_suffix = ordinal_abbr(parameters$target_perc, lang),
                c.oel              = parameters$oel,
                distplot.1         = translate(lang = lang, "Concentration"),
                distplot.2         = translate(lang = lang, "Density"),
                distplot.4         = translate(lang = lang, "OEL outside of graphical limits."),
                distplot.5         = translate(lang = lang, "OEL"),
                distplot.6         = translate(lang = lang, "Percentile")
            )
        })

        output$density_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This plot shows the estimated exposure distribution when
                assuming 250 exposure measurements have been collected. If
                the measurements represent 8-hour TWA (Time-Weighted Average)
                values, this approximately represents a full year of exposure.
                The OEL is shown as a red dotted line and the point estimate
                of the selected percentile as a continuous blue line.
            ")
        }) |>
        shiny::bindCache(lang())

        output$risk_band_plot <- shiny::renderPlot({
            lang <- lang()
            parameters <- parameters()
            bayesian_analysis <- bayesian_analysis()

            riskband.plot.perc(
                mu.chain    = bayesian_analysis$mu.chain,
                sigma.chain = bayesian_analysis$sigma.chain,
                c.oel       = parameters$oel,
                target_perc = parameters$target_perc,
                psi         = parameters$psi,
                # ≤ may not render in all IDEs. This is Unicode
                # character U+2264 (&leq;) (Less-Than or Equal To).
                riskplot.2  = translate(lang = lang, "Probability"),
                riskplot.3  = translate(lang = lang, "≤ 1% OEL"),
                riskplot.4  = translate(lang = lang, "1% < OEL ≤ 10%"),
                riskplot.5  = translate(lang = lang, "10% < OEL ≤ 50%"),
                riskplot.6  = translate(lang = lang, "50% < OEL ≤ 100%"),
                riskplot.7  = translate(lang = lang, "> OEL"),
                riskplot.8  = translate(lang = lang, "Critical Percentile Category")
            )
        })

        output$risk_band_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This plot shows the probability distribution of the
                uncertainty around the selected percentile. It shows
                the probability that its true value is
                (1) below 1%% of the OEL,
                (2) between 1%% and 10%% of the OEL,
                (3) between 10%% and 50%% of the OEL,
                (4) between 50%% and 100%% of the OEL, and
                (5) greater than the OEL.
                This is based on the classification adopted by AIHA. The
                red column represents the probability of an overexposure.
                The latter should be lower than the threshold shown as a
                black dashed line.
            ")
        }) |>
        shiny::bindCache(lang())

        # Swap colors of borders and background of
        # the risk assessment card if data shows a
        # poorly controlled situation.
        shiny::observe({
            is_controlled <- risk_assessment()$is_controlled

            # Use green colors if the risk is controlled.
            shinyjs::toggleClass(
                id        = "risk_assessment_header",
                class     = "border-success text-success",
                condition = is_controlled
            )

            shinyjs::toggleClass(
                id        = "risk_assessment_card",
                class     = "bg-success-subtle border-success",
                condition = is_controlled
            )

            # Use red colors if the risk is not controlled.
            shinyjs::toggleClass(
                id        = "risk_assessment_header",
                class     = "border-danger text-danger",
                condition = !is_controlled
            )

            shinyjs::toggleClass(
                id        = "risk_assessment_card",
                class     = "bg-danger-subtle border-danger",
                condition = !is_controlled
            )
        }) |>
        shiny::bindEvent(risk_assessment())

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}
