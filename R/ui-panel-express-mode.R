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

    # Override default height of cards only containing text.
    # They require less (vertical) space in express mode.
    default_card_height_text_only <- "300px"

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
        height = default_card_height_text_only,

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
        height   = default_card_height_text_only,
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
            risk_band_plot
        )
    )

    return(ui)
}

#' @rdname ui-panel-percentiles
#' @export
server_panel_express <- function(
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
        title <- shiny::reactive({
            translate(lang = lang(), "Statistical Inference")
        }) |>
        shiny::bindCache(lang())

        risk_assessment <- shiny::reactive({
            # findInterval() creates a fourth interval from
            # the standard thresholds: (-∞, 0L). It maps it
            # to 0. It is not used.
            risk_level <- switch(
                findInterval(num_results()$perc.risk, aiha_risk_levels$thresholds),
                "acceptable",  # Bin 1.
                "tolerable",   # Bin 2.
                "problematic", # Bin 3.
                NULL           # Bin 0.
            )

            aiha_risk_levels$metadata[[risk_level]]
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
                            Overexposure is defined as the %s percentile
                            being greater than or equal to the OEL.
                        "),
                        tags$strong(ordinal(parameters$target_perc, lang))
                    )
                ),

                tags$li(
                    class = li_classes,
                    html(
                        translate(lang = lang, "
                            The probability that this criterion is met is equal
                            to %s.
                        "),
                        tags$strong(as_percentage(num_results$perc.risk))
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

        output$risk_assessment_help <- shiny::renderUI({
            lang <- lang()

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
                            shared_urls$aiha_videos,
                            "Making Accurate Exposure Risk Decisions"
                        )
                    )
                ),

                tags$ul(
                    class = "list-group list-group-flush",

                    tags$li(
                        class = "list-group-item text-success pb-3",

                        tags$h3(
                            class = "fs-6 fw-bold",
                            translate(lang = lang, "Acceptable")
                        ),

                        translate(lang = lang, "
                            If the overexposure risk is lower than 5%, it is
                            very low. The situation is well controlled.
                        ")
                    ),

                    tags$li(
                        class = "list-group-item text-warning py-3",

                        tags$h3(
                            class = "fs-6 fw-bold",
                            translate(lang = lang, "Tolerable")
                        ),

                        translate(lang = lang, "
                            If the overexposure risk is betwen 5% and 30%, it
                            is moderate. The situation is controlled, but with
                            a limited safety margin.
                        ")
                    ),

                    tags$li(
                        class = "list-group-item text-danger pt-3",

                        tags$h3(
                            class = "fs-6 fw-bold",
                            translate(lang = lang, "Problematic")
                        ),

                        translate(lang = lang, "
                            If the overexposure risk is higher than 30%, it
                            is high. The situation requires remedial action.
                        ")
                    )
                )
            )
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
                        as.character(
                            ordinal(default_express_inputs$target_perc, lang)
                        ),
                        tags$strong(
                            sprintf("%s [%s - %s]", perc$est, perc$lcl, perc$ucl)
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
                            signif(num_results()$perc.ucl70, default_n_digits)
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
                            signif(num_results()$perc.ucl95, default_n_digits)
                        )
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
                            The 70%% upper confidence limit is equal to %s.
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
                            The 95%% upper confidence limit is equal to %s.
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

        # FIXME: dessinerRisqueMetre.G() is an artifact stemming
        # from Tool 1 Express version 3 imported as is into Tool
        # 1 version 5. It is similar to dessinerRisqueMetre() but
        # is a bit more cryptic. Both functions should be merged
        # into a single unified function (and rewritten).
        output$risk_meter_plot <- shiny::renderPlot({
            parameters <- parameters()
            bayesian_analysis <- bayesian_analysis()

            frac <- 100 * (
                1 - pnorm(
                    (log(parameters$oel) - bayesian_analysis$mu) / bayesian_analysis$sigma
                )
            )

            risk <- length(frac[frac >= parameters$frac_threshold]) / length(frac)

            dessinerRisqueMetre.G(
                actualProb          = risk,
                minProbUnacceptable = parameters$psi / 100L
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
            results <- num_results()

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
            parameters <- parameters()
            bayesian_analysis <- bayesian_analysis()

            riskband.plot.perc(
                mu.chain    = bayesian_analysis$mu.chain,
                sigma.chain = bayesian_analysis$sigma.chain,
                c.oel       = num_results()$c.oel,
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
                ordinal(default_express_inputs$target_perc, lang)
            )
        }) |>
        # Since target_perc is an internal constant,
        # do not take a dependency on parameters().
        shiny::bindCache(lang())

        # Update colors of borders and background
        # of the risk assessment card based on the
        # risk level.
        shiny::observe({
            risk_level <- risk_assessment()$level
            color_acceptable  <- aiha_risk_levels$metadata$acceptable$color
            color_tolerable   <- aiha_risk_levels$metadata$tolerable$color
            color_problematic <- aiha_risk_levels$metadata$problematic$color

            # Use green colors if the risk is acceptable.
            shinyjs::toggleClass(
                id        = "risk_assessment_header",
                class     = sprintf("border-%s text-%1$s", color_acceptable),
                condition = { risk_level == "acceptable" }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_help_btn",
                class     = sprintf("btn-outline-%s", color_acceptable),
                condition = { risk_level == "acceptable" }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_card",
                class     = sprintf("border-%s bg-%1$s-subtle", color_acceptable),
                condition = { risk_level == "acceptable" }
            )

            # Use yellow colors if the risk is tolerable.
            shinyjs::toggleClass(
                id        = "risk_assessment_header",
                class     = sprintf("border-%s text-%1$s", color_tolerable),
                condition = { risk_level == "tolerable" }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_help_btn",
                class     = sprintf("btn-outline-%s", color_tolerable),
                condition = { risk_level == "tolerable" }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_card",
                class     = sprintf("border-%s bg-%1$s-subtle", color_tolerable),
                condition = { risk_level == "tolerable" }
            )

            # Use red colors if the risk is problematic.
            shinyjs::toggleClass(
                id        = "risk_assessment_header",
                class     = sprintf("border-%s text-%1$s", color_problematic),
                condition = { risk_level == "problematic" }
            )
            shinyjs::toggleClass(
                id        = "risk_assessment_help_btn",
                class     = sprintf("btn-outline-%s", color_problematic),
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
