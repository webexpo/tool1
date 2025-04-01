#' Statistics Panel Module
#'
#' @description
#' This module cotrols the Statistics panel component. It is currently nested
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
#' |         |  | Statistics Panel              |  |
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
#' @template param-data-sample
#'
#' @returns
#' [ui_panel_statistics()] returns a `shiny.tag` object
#' (an output of [bslib::nav_panel()]).
#'
#' [server_panel_statistics()] returns `NULL`, invisibly.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-panel-statistics
#'
#' @export
ui_panel_statistics <- function(id) {
    ns <- shiny::NS(id)

    # Descriptive Statistics ---------------------------------------------------

    stats <- bslib::card(
        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("stats_title"), tags$span)
            )
        ),

        bslib::card_body(
            DT::DTOutput(ns("stats"))
        )
    )

    # Information --------------------------------------------------------------

    info <- bslib::card(
        class = "bg-info-subtle border-info",

        bslib::card_header(
            class = "border-info",

            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",

                tags$span(
                    class = "pe-2",
                    bsicons::bs_icon("info-circle-fill", a11y = "deco")
                ),

                shiny::textOutput(ns("info_title"), tags$span)
            )
        ),

        bslib::card_body(
            shiny::textOutput(ns("info"), tags$p),

            tags$div(
                htmltools::tagAppendAttributes(
                    class = "fs-5",
                    shiny::textOutput(ns("info_censoring_title"), tags$h3),
                ),

                shiny::uiOutput(ns("info_censoring"))
            )
        )
    )

    # QQ Plot -------------------------------------------------------------------

    qq_plot <- bslib::card(
        full_screen = TRUE,

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("qq_plot_title"), tags$span)
            )
        ),

        bslib::card_body(
            shiny::plotOutput(ns("qq_plot"))
        ),

        bslib::card_footer(
            shiny::textOutput(ns("qq_plot_desc"), tags$p)
        )
    )

    # Box Plot -----------------------------------------------------------------

    box_plot <- bslib::card(
        full_screen = TRUE,

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("box_plot_title"), tags$span)
            )
        ),

        bslib::card_body(
            shiny::plotOutput(ns("box_plot"))
        ),

        bslib::card_footer(
            shiny::uiOutput(ns("box_plot_desc"), container = tags$p)
        )
    )

    # Panel --------------------------------------------------------------------

    ui <- bslib::nav_panel(
        value = id,
        title = shiny::textOutput(ns("title"), tags$span),

        bslib::layout_column_wrap(
            width  = 1/2,
            height = default_text_card_height,
            fill   = FALSE,
            stats,
            info
        ),

        bslib::layout_column_wrap(
            width  = 1/2,
            height = default_plot_card_height,
            fill   = FALSE,
            qq_plot,
            box_plot
        )
    )

    return(ui)
}

#' @rdname ui-panel-statistics
#' @export
server_panel_statistics <- function(id, lang, parameters, data_sample) {
    stopifnot(exprs = {
        shiny::is.reactive(lang)
        shiny::is.reactive(parameters)
        shiny::is.reactive(data_sample)
    })

    server <- function(input, output, session) {
        data_sample_imputed <- reactive({
            data_sample <- data_sample()

            simple.censored.treatment(
                observations.formatted = data_sample$data,
                notcensored            = data_sample$notcensored,
                leftcensored           = data_sample$leftcensored,
                rightcensored          = data_sample$rightcensored,
                intcensored            = data_sample$intcensored)
        })

        output$title <- shiny::renderText({
            translate(lang = lang(), "Statistics")
        }) |>
        shiny::bindCache(lang())

        output$stats_title <- shiny::renderText({
            translate(lang = lang(), "Descriptive Statistics")
        }) |>
        shiny::bindCache(lang())

        output$info_title <- shiny::renderText({
            translate(lang = lang(), "Information")
        }) |>
        shiny::bindCache(lang())

        output$info_censoring_title <- shiny::renderText({
            translate(lang = lang(), "Censored Measurements")
        }) |>
        shiny::bindCache(lang())

        output$qq_plot_title <- shiny::renderText({
            translate(lang = lang(), "Quantile-Quantile Plot")
        }) |>
        shiny::bindCache(lang())

        output$box_plot_title <- shiny::renderText({
            translate(lang = lang(), "Box and Whiskers Plot")
        }) |>
        shiny::bindCache(lang())

        output$stats <- DT::renderDT(server = FALSE, {
            lang <- lang()
            stats <- as.matrix(
                fun.desc.stat(
                    data.simply.imputed = data_sample_imputed(),
                    c.oel               = parameters()$oel
                )
            )

            # Values (statistics) are in the second column.
            # The first column contains internal row names
            # that are replaced below.
            DT::datatable(
                data     = stats[, "value", drop = FALSE],
                class    = "stripe hover compact",
                options  = list(
                    pageLength  = nrow(stats),
                    ordering    = FALSE
                ),
                rownames = c(
                    translate(lang = lang, "Number of Obversations"),
                    translate(lang = lang, "Proportion Censored"),
                    translate(lang = lang, "Minimum"),
                    as.character(
                        tags$span(
                            ordinal(25L, lang),
                            translate(lang = lang, "Percentile")
                        )
                    ),
                    translate(lang = lang, "Median"),
                    as.character(
                        tags$span(
                            ordinal(75L, lang),
                            translate(lang = lang, "Percentile")
                        )
                    ),
                    translate(lang = lang, "Maximum"),
                    translate(lang = lang, "Proportion Greater than OEL"),
                    translate(lang = lang, "Arithmetic Mean"),
                    translate(lang = lang, "Arithmetic Standard Deviation"),
                    translate(lang = lang, "Coefficient of Variation"),
                    translate(lang = lang, "Geometric Mean"),
                    translate(lang = lang, "Geometric Standard Deviation")
                ),
                colnames = c(
                    translate(lang = lang, "Sample Statistic"),
                    translate(lang = lang, "Value")
                ),
                # Escape first HTML column (the one containing
                # row names above) to allow usage of <sup> tags.
                escape             = -1L,
                filter             = "none",
                style              = "bootstrap",
                editable           = FALSE,
                autoHideNavigation = TRUE
            )
        })

        output$info <- shiny::renderText({
            translate(lang = lang(), "
                The parameters shown here are a simple description of the
                measurements. They ensure that the data input and import
                process went well and that there are no obvious outliers.
                They should not be viewed as useful estimates of the
                underlying exposure distribution. For that purpose, use
                the other panels (Exceedance Fraction, Percentiles, or
                Arithmetic Mean above) where inference is based on
                Bayesian models.
            ")
        }) |>
        shiny::bindCache(lang())

        output$info_censoring <- shiny::renderUI({
            lang <- lang()
            list(
                tags$p(translate(lang = lang, "
                    Censored measurements are subject to one of the
                    following procedure.
                ")),

                tags$ul(
                    tags$li(translate(lang = lang, "
                        Interval censored measurements are imputed as the
                        mid-range.
                    ")),

                    tags$li(translate(lang = lang, "
                        Measurements censored to the right are imputed as 9/4
                        of the censoring point.
                    ")),

                    tags$li(
                        html(
                            translate(lang = lang, "
                                Measurements censored to the left are treated
                                using robust Log-probit regression on order
                                statistics. The algorithm used is derived from
                                %s (itself derived from previous work of %s).
                            "),

                            tags$a(
                                href   = default_urls$ndexpo,
                                target = "_blank",
                                "NDExpo"
                            ),

                            tags$a(
                                href   = default_urls$dennis_helsel,
                                target = "_blank",
                                "Dennis Helsel"
                            )
                        )
                    )
                )
            )
        }) |>
        shiny::bindCache(lang())

        output$qq_plot <- shiny::renderPlot({
            lang <- lang()
            fun.qqplot(
                data.simply.imputed = data_sample_imputed(),
                notcensored         = data_sample()$notcensored,
                qqplot.1            = translate(lang = lang, "Quantile-Quantile Plot"),
                qqplot.2            = translate(lang = lang, "Quantiles (Lognormal Distribution)"),
                qqplot.3            = translate(lang = lang, "Quantiles (Standardized Measurements)"),
                qqplot.4            = translate(lang = lang, "Measurement Type"),
                qqplot.5            = translate(lang = lang, "Censored"),
                qqplot.6            = translate(lang = lang, "Detected"))
        })

        output$qq_plot_desc <- shiny::renderText({
            translate(lang = lang(), "
                The points above should follow a straight line. Random deviations
                from it are expected. However, significant deviations suggest that
                the data may have to be split into distinct subsets, or that some
                outliers must be investigated.
            ")
        }) |>
        shiny::bindCache(lang())

        output$box_plot <- shiny::renderPlot({
            lang <- lang()
            fun.boxplot(
                data.simply.imputed = data_sample_imputed(),
                notcensored         = data_sample()$notcensored,
                c.oel               = parameters()$oel,
                boxplot.1           = translate(lang = lang, "Measurement Type"),
                boxplot.2           = translate(lang = lang, "Concentration"),
                boxplot.3           = translate(lang = lang, "Exposure Limit"),
                boxplot.4           = translate(lang = lang, "Censored"),
                boxplot.5           = translate(lang = lang, "Not Censored"),
                boxplot.6           = translate(lang = lang, "Measurements"))
        })

        output$box_plot_desc <- shiny::renderUI({
            lang <- lang()
            html(
                translate(lang = lang, "
                    The measurements are scattered around the x-axis middle
                    point. The box (outer horizontal lines) represents the
                    distance between the 25%s and 75%s percentiles. The whiskers
                    (vertical lines) represent the distance between the 10%s and
                    90%s percentiles. The inner black horizontal line is the
                    median.
                "),
                tags$sup(ordinal_abbr(25L, lang)),
                tags$sup(ordinal_abbr(75L, lang)),
                tags$sup(ordinal_abbr(10L, lang)),
                tags$sup(ordinal_abbr(90L, lang))
            )
        }) |>
        shiny::bindCache(lang())

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}
