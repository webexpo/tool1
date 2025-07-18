#' Express Mode Inference Panel Module
#'
#' @description
#' This module controls the Express Mode Inference panel component. It is
#' currently nested into the application's main [bslib::navset] conceptually
#' illustrated below.
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
#' |         |  | Express Mode Panel            |  |
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
#' [ui_panel_express()] returns a `shiny.tag` object
#' (an output of [bslib::nav_panel()]).
#'
#' [server_panel_express()] returns returns a [shiny::reactive()] object.
#' It can be called to get the panel's title.
#'
#' @note
#' This module is similar to the Exceedance Fraction, Percentiles, and
#' Arithmetic Mean panels. Most of its contents was copied over from
#' these modules and lightly refactored.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-panel-percentiles
#' @export
ui_panel_express <- function(id) {
    ns <- shiny::NS(id)
    card_height      <- getOption("app_card_height_md")
    card_height_text <- getOption("app_card_height_xs")

    # Risk Assessment ----------------------------------------------------------

    risk_assessment_help <- bslib::popover(
        title = shiny::textOutput(
            outputId  = ns("risk_assessment_help_title"),
            container = tags$span
        ),
        # Increase maximum width of parent container.
        # This can only be done with a custom class.
        options = list(customClass = "app-popover"),
        # Clickable element that toggles the popover.
        trigger = tags$div(
            id    = ns("risk_assessment_help_btn"),
            class = "btn btn-outline-secondary app-btn",

            bsicons::bs_icon(name = "info-circle-fill", a11y = "sem")
        ),

        shiny::uiOutput(ns("risk_assessment_help"))
    )

    risk_assessment <- bslib::card(
        id     = ns("risk_assessment_card"),
        height = card_height_text,

        bslib::card_header(
            id    = ns("risk_assessment_header"),
            class = "d-flex justify-content-between",

            bslib::card_title(
                container = tags$h2,
                class     = "d-flex justify-content-between my-2 fs-5",

                shiny::uiOutput(
                    outputId  = ns("risk_assessment_icon"),
                    container = tags$span,
                    class     = "pe-2"
                ),

                shiny::textOutput(ns("risk_assessment_title"), tags$span)
            ),

            risk_assessment_help
        ),

        bslib::card_body(
            shiny::uiOutput(ns("risk_assessment"))
        )
    )

    # Estimates ----------------------------------------------------------------

    estimates <- bslib::navset_card_underline(
        height   = card_height_text,
        id       = ns("estimates_panel_active"),
        selected = ns("estimates_panel_params"),

        title = bslib::card_title(
            container = tags$h2,
            class     = "my-2 fs-5",
            shiny::textOutput(ns("estimates_title"), tags$span)
        ),

        footer = bslib::card_footer(
            shiny::textOutput(ns("estimates_desc"), tags$p)
        ),

        # nav_menu() must be used (until further notice) to ensure a
        # consistent responsive design. <nav> bars of cards created
        # with navset_card_underline() do not collapse into a mobile
        # menu. This is very problematic on smaller screens because
        # it breaks the layout.
        bslib::nav_menu(
            title = shiny::textOutput(ns("estimates_menu_title"), tags$span),
            icon  = tags$span(
                class = "pe-1",
                bsicons::bs_icon(name = "list", a11y = "deco")
            ),

            # Estimates of the underlying distribution parameters.
            bslib::nav_panel(
                value = ns("estimates_panel_params"),
                title = shiny::textOutput(
                    outputId  = ns("estimates_panel_params_title"),
                    container = tags$span
                ),

                shiny::uiOutput(
                    outputId  = ns("estimates_params"),
                    container = tags$ul,
                    class     = "list-group list-group-flush"
                )
            ),

            # Estimate of the exceedance fraction.
            bslib::nav_panel(
                value = ns("estimates_panel_fraction"),
                title = shiny::textOutput(
                    outputId  = ns("estimates_panel_fraction_title"),
                    container = tags$span
                ),

                shiny::uiOutput(
                    outputId  = ns("estimates_fraction"),
                    container = tags$ul,
                    class     = "list-group list-group-flush"
                )
            ),

            # Estimate of the 95th percentile.
            bslib::nav_panel(
                value = ns("estimates_panel_percentile"),
                title = shiny::uiOutput(
                    outputId  = ns("estimates_panel_percentile_title"),
                    container = tags$span
                ),

                shiny::uiOutput(
                    outputId  = ns("estimates_percentile"),
                    container = tags$ul,
                    class     = "list-group list-group-flush"
                )
            ),

            # Estimate of the arithmetic mean.
            bslib::nav_panel(
                value = ns("estimates_panel_mean"),
                title = shiny::textOutput(
                    outputId  = ns("estimates_panel_mean_title"),
                    container = tags$span
                ),

                shiny::uiOutput(
                    outputId  = ns("estimates_mean"),
                    container = tags$ul,
                    class     = "list-group list-group-flush"
                )
            )
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
            risk_band_plot
        )
    )

    return(ui)
}

#' @rdname ui-panel-percentiles
#' @export
server_panel_express <- function(id, lang, inputs_calc, simulations, results) {
    stopifnot(exprs = {
        shiny::is.reactive(lang)
        shiny::is.reactive(inputs_calc)
        shiny::is.reactive(simulations)
        shiny::is.reactive(results)
    })

    server <- function(input, output, session) {
        title <- shiny::reactive({
            translate(lang = lang(), "Statistical Inference")
        }) |>
        shiny::bindCache(lang())

        risk_level <- shiny::reactive({
            get_risk_level_info(results()$.risk_levels$express, lang())
        })

        output$title <- shiny::renderText({
            title()
        })

        output$risk_assessment_help_title <- shiny::renderText({
            translate(lang = lang(), "How should I interpret these results?")
        }) |>
        shiny::bindCache(lang())

        output$risk_assessment_title <- shiny::renderText({
            translate(lang = lang(), "Risk Assessment")
        }) |>
        shiny::bindCache(lang())

        output$estimates_title <- shiny::renderText({
            translate(lang = lang(), "Estimates")
        }) |>
        shiny::bindCache(lang())

        output$estimates_menu_title <- shiny::renderText({
            translate(lang = lang(), "Parameters")
        }) |>
        shiny::bindCache(lang())

        output$estimates_panel_params_title <- shiny::renderText({
            translate(lang = lang(), "Distribution Parameters")
        }) |>
        shiny::bindCache(lang())

        output$estimates_panel_fraction_title <- shiny::renderText({
            translate(lang = lang(), "Exceedance Fraction")
        }) |>
        shiny::bindCache(lang())

        output$estimates_panel_percentile_title <- shiny::renderUI({
            translate(lang = lang(), "Critical Percentile")
        }) |>
        shiny::bindCache(lang())

        output$estimates_panel_mean_title <- shiny::renderText({
            translate(lang = lang(), "Arithmetic Mean")
        }) |>
        shiny::bindCache(lang())

        output$risk_meter_plot_title <- shiny::renderText({
            translate(lang = lang(), "Risk Meter")
        }) |>
        shiny::bindCache(lang())

        output$seq_plot_title <- shiny::renderText({
            translate(lang = lang(), "Sequential Plot")
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
                        translate(lang = lang, "The current situation is %s."),
                        tags$strong(risk_level$name)
                    )
                )
            )
        })

        output$risk_assessment_icon <- shiny::renderUI({
            risk_level()$icon
        })

        output$risk_assessment_help <- shiny::renderUI({
            lang <- lang()
            risk_level_1 <- get_risk_level_info(1L, lang)
            risk_level_2 <- get_risk_level_info(2L, lang)
            risk_level_3 <- get_risk_level_info(3L, lang)

            list(
                tags$p(
                    html(
                        translate(lang = lang, "
                            The decision scheme to interpret the probability of
                            overexposure (the overexposure risk, which is the
                            probability that the overexposure criterion is met)
                            follows the recommendation of the AIHA video series
                            %s (English only).
                        "),
                        ui_link(
                            urls$aiha_videos,
                            "Making Accurate Exposure Risk Decisions"
                        )
                    )
                ),

                tags$ul(
                    class = "list-group list-group-flush px-3",

                    tags$li(
                        class = sprintf(
                            "list-group-item text-%s pb-3",
                            risk_level_1$color
                        ),

                        tags$h3(
                            class = "fs-6 fw-bold",
                            risk_level_1$icon,
                            risk_level_1$name_caps
                        ),

                        html(
                            translate(lang = lang, "
                                If the overexposure risk is lower than %s, it
                                is very low. The situation is well controlled.
                            "),
                            as_percentage(risk_level_1$threshold)
                        )
                    ),

                    tags$li(
                        class = sprintf(
                            "list-group-item text-%s py-3",
                            risk_level_2$color
                        ),

                        tags$h3(
                            class = "fs-6 fw-bold",
                            risk_level_2$icon,
                            risk_level_2$name_caps
                        ),

                        html(
                            translate(lang = lang, "
                                If the overexposure risk is betwen %s and %s,
                                it is moderate. The situation is controlled,
                                but with a limited safety margin.
                            "),
                            as_percentage(risk_level_2$threshold),
                            as_percentage(risk_level_3$threshold)
                        )
                    ),

                    tags$li(
                        class = sprintf(
                            "list-group-item text-%s pt-3",
                            risk_level_3$color
                        ),

                        tags$h3(
                            class = "fs-6 fw-bold",
                            risk_level_3$icon,
                            risk_level_3$name_caps
                        ),

                        html(
                            translate(lang = lang, "
                                If the overexposure risk is higher than %s, it
                                is high. The situation requires remedial action.
                            "),
                            as_percentage(risk_level_3$threshold)
                        )
                    )
                )
            )
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

        output$estimates_fraction <- shiny::renderUI({
            lang <- lang()
            results <- results()

            list(
                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of the exceedance fraction is
                            equal to %s.
                        "),
                        tags$strong(results$.intervals$frac)
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The 70%% upper confidence limit is equal to %s.
                        "),
                        tags$strong(as_percentage(results$frac.ucl70))
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The 95%% upper confidence limit is equal to %s.
                        "),
                        tags$strong(as_percentage(results$frac.ucl95))
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

        output$estimates_mean <- shiny::renderUI({
            lang <- lang()
            results <- results()

            list(
                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The point estimate of the arithmetic mean is
                            equal to %s.
                        "),
                        tags$strong(results$.intervals$am)
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The 70%% upper confidence limit is equal to %s.
                        "),
                        tags$strong(results$.rounded$am.ucl70)
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            The 95%% upper confidence limit is equal to %s.
                        "),
                        tags$strong(results$.rounded$am.ucl95)
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

        # FIXME: dessinerRisqueMetre.G() is an artifact stemming
        # from Tool 1 Express version 3 imported as is into Tool
        # 1 version 5. It is similar to dessinerRisqueMetre() but
        # is a bit more cryptic. Both functions should be merged
        # into a single unified function (and rewritten).
        output$risk_meter_plot <- shiny::renderPlot({
            inputs_calc <- inputs_calc()
            simulations <- simulations()

            frac <- 100 * (
                1 - pnorm(
                    (log(inputs_calc$oel) - simulations$mu) / simulations$sigma
                )
            )

            risk <- length(frac[frac >= inputs_calc$frac_threshold]) / length(frac)

            dessinerRisqueMetre.G(
                actualProb          = risk,
                minProbUnacceptable = inputs_calc$psi / 100L
            )
        })

        output$risk_meter_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                This risk meter shows the probability of the exposure being too
                high when compared to the OEL. The green region indicates an
                acceptable exposure, the yellow region a tolerable exposure,
                and the red region a problematic exposure.
            ")
        }) |>
        shiny::bindCache(lang())

        output$seq_plot <- shiny::renderPlot({
            lang <- lang()
            results <- results()

            sequential.plot.frac(
                gm        = results$gm$est,
                gsd       = results$gsd$est,
                frac      = results$frac$est,
                c.oel     = results$c.oel,
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
                riskplot.8  = translate(lang = lang, "Percentile Category")
            )
        })

        output$risk_band_plot_desc <- shiny::renderUI({
            lang <- lang()
            html(
                translate(lang = lang, "
                    This plot shows the probability distribution of the
                    uncertainty around the %s percentile. It shows the
                    probability that its true value is
                    (1) below 1%% of the OEL,
                    (2) between 1%% and 10%% of the OEL,
                    (3) between 10%% and 50%% of the OEL,
                    (4) between 50%% and 100%% of the OEL, and
                    (5) greater than the OEL.
                    This is based on the classification adopted by AIHA. The
                    red column represents the probability of an overexposure.
                    The latter should be lower than the threshold (black dashed
                    line).
                "),
                ordinal(inputs_calc()$target_perc, lang)
            )
        }) |>
        # Since target_perc is an internal constant,
        # do not take a dependency on inputs_calc().
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
                id        = "risk_assessment_help_btn",
                class     = sprintf("btn-outline-%s", colors[[1L]]),
                condition = { level == 1L }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_card",
                class     = sprintf("border-%s bg-%1$s-subtle", colors[[1L]]),
                condition = { level == 1L }
            )

            shinyjs::toggleClass(
                id        = "risk_assessment_header",
                class     = sprintf("border-%s text-%1$s", colors[[2L]]),
                condition = { level == 2L }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_help_btn",
                class     = sprintf("btn-outline-%s", colors[[2L]]),
                condition = { level == 2L }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_card",
                class     = sprintf("border-%s bg-%1$s-subtle", colors[[2L]]),
                condition = { level == 2L }
            )

            shinyjs::toggleClass(
                id        = "risk_assessment_header",
                class     = sprintf("border-%s text-%1$s", colors[[3L]]),
                condition = { level == 3L }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_help_btn",
                class     = sprintf("btn-outline-%s", colors[[3L]]),
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
