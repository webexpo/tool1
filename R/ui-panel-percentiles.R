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
#' @template param-id
#'
#' @template param-lang
#'
#' @template param-inputs-calc
#'
#' @template param-simulations
#'
#' @template param-results
#'
#' @returns
#' [ui_panel_percentiles()] returns a `shiny.tag` object
#' (an output of [bslib::nav_panel()]).
#'
#' [server_panel_percentiles()] returns returns a [shiny::reactive()] object.
#' It can be called to get the panel's title.
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
    card_height      <- getOption("app_card_height_md")
    card_height_text <- getOption("app_card_height_sm")

    # Risk Assessment ----------------------------------------------------------

    risk_assessment <- bslib::card(
        id     = ns("risk_assessment_card"),
        height = card_height_text,

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
        height      = card_height,
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
        height = card_height_text,

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
        height      = card_height,
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
        height      = card_height,
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
        height      = card_height,
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
    inputs_calc,
    simulations,
    results)
{
    stopifnot(exprs = {
        shiny::is.reactive(lang)
        shiny::is.reactive(inputs_calc)
        shiny::is.reactive(simulations)
        shiny::is.reactive(results)
    })

    server <- function(input, output, session) {
        title <- shiny::reactive({
            translate(lang = lang(), "Percentiles")
        }) |>
        shiny::bindCache(lang())

        risk_level <- shiny::reactive({
            get_risk_level_info(results()$.risk_levels$perc, lang())
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

        output$estimates_percentile_title <- shiny::renderText({
            translate(lang = lang(), "Critical Percentile")
        })

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
            results <- results()
            risk_level <- risk_level()
            inputs_calc <- inputs_calc()

            li_classes <- sprintf(
                "list-group-item bg-%s-subtle border-%1$s",
                risk_level$color
            )

            tags$ul(
                class = sprintf(
                    "list-group list-group-flush bg-%s-subtle border-%1$s",
                    risk_level$color
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "
                            Overexposure is defined as the %s percentile
                            being greater than or equal to the OEL.
                        "),
                        tags$strong(ordinal(inputs_calc$target_perc, lang))
                    )
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "
                            The probability that this criterion is met is equal
                            to %s.
                        "),
                        tags$strong(as_percentage(results$perc.risk))
                    )
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "
                            The probability that this criterion is met should
                            be lower than %s.
                        "),
                        tags$strong(as_percentage(inputs_calc$psi))
                    )
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "The current situation is %s."),
                        tags$strong(risk_level$name)
                    )
                )
            )
        })

        output$risk_assessment_icon <- shiny::renderUI({
            risk_level()$icon
        })

        output$risk_meter_plot <- shiny::renderPlot({
            dessinerRisqueMetre(
                actualProb          = results()$perc.risk,
                minProbUnacceptable = inputs_calc()$psi
            )
        })

        output$risk_meter_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This risk meter shows the probability of the exposure being
                too high when compared to the OEL. The red zone indicates a
                problematic exposure.
            ")
        }) |>
        shiny::bindCache(lang())

        output$estimates_params <- shiny::renderUI({
            lang <- lang()
            results <- results()

            list(
                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of the geometric mean is equal
                            to %s.
                        "),
                        tags$strong(results$.intervals$gm)
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of the geometric standard
                            deviation is equal to %s.
                        "),
                        tags$strong(results$.intervals$gsd)
                    )
                )
            )
        })

        output$estimates_percentile <- shiny::renderUI({
            lang <- lang()
            results <- results()

            list(
                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of the %s percentile is
                            equal to %s.
                        "),
                        tags$span(ordinal(inputs_calc()$target_perc, lang)),
                        tags$strong(results$.intervals$perc)
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The 70%% upper confidence limit is equal to %s.
                        "),
                        tags$strong(results$.rounded$perc.ucl70)
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The 95%% upper confidence limit is equal to %s.
                        "),
                        tags$strong(results$.rounded$perc.ucl95)
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
            results <- results()
            inputs_calc <- inputs_calc()

            sequential.plot.perc(
                gm                 = results$gm$est,
                gsd                = results$gsd$est,
                perc               = results$perc$est,
                c.oel              = results$c.oel,
                target_perc        = inputs_calc$target_perc,
                target_perc_suffix = ordinal_abbr(inputs_calc$target_perc, lang),
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
                The OEL is shown as a dotted red line and the point estimate
                of the selected percentile as a continuous blue line.
            ")
        }) |>
        shiny::bindCache(lang())

        output$density_plot <- shiny::renderPlot({
            lang <- lang()
            results <- results()
            inputs_calc <- inputs_calc()
            simulations <- simulations()

            distribution.plot.perc(
                gm                 = exp(median(simulations$mu.chain)),
                gsd                = exp(median(simulations$sigma.chain)),
                perc               = results$perc$est,
                c.oel              = results$c.oel,
                target_perc        = inputs_calc$target_perc,
                target_perc_suffix = ordinal_abbr(inputs_calc$target_perc, lang),
                distplot.1         = translate(lang = lang, "Concentration"),
                distplot.2         = translate(lang = lang, "Density"),
                distplot.4         = translate(lang = lang, "OEL outside of graphical limits."),
                distplot.5         = translate(lang = lang, "OEL"),
                distplot.6         = translate(lang = lang, "Percentile")
            )
        })

        output$density_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This plot shows the probability density function of the
                estimated distribution of exposures. The OEL is shown as a
                dotted red line and the point estimate of the selected
                percentile as a continuous blue line.
            ")
        }) |>
        shiny::bindCache(lang())

        output$risk_band_plot <- shiny::renderPlot({
            lang <- lang()
            inputs_calc <- inputs_calc()
            simulations <- simulations()

            riskband.plot.perc(
                mu.chain    = simulations$mu.chain,
                sigma.chain = simulations$sigma.chain,
                c.oel       = results()$c.oel,
                target_perc = inputs_calc$target_perc,
                psi         = inputs_calc$psi,
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
                (1) below 1% of the OEL,
                (2) between 1% and 10% of the OEL,
                (3) between 10% and 50% of the OEL,
                (4) between 50% and 100% of the OEL, and
                (5) greater than the OEL.
                This is based on the classification adopted by AIHA. The red
                column represents the probability of an overexposure. The
                latter should be lower than the threshold (black dashed line).
            ")
        }) |>
        shiny::bindCache(lang())

        # Update colors of borders and background
        # of the risk assessment card based on the
        # risk level.
        shiny::observe({
            level <- risk_level()$level
            colors <- get_risk_level_colors()

            shinyjs::toggleClass(
                id        = "risk_assessment_header",
                class     = sprintf("border-%s text-%1$s", colors[[1L]]),
                condition = { level == 1L }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_card",
                class     = sprintf("border-%s bg-%1$s-subtle", colors[[1L]]),
                condition = { level == 1L }
            )

            shinyjs::toggleClass(
                id        = "risk_assessment_header",
                class     = sprintf("border-%s text-%1$s", colors[[3L]]),
                condition = { level == 3L }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_card",
                class     = sprintf("border-%s bg-%1$s-subtle", colors[[3L]]),
                condition = { level == 3L }
            )
        }) |>
        shiny::bindEvent(risk_level())

        return(title)
    }

    return(shiny::moduleServer(id, server))
}
