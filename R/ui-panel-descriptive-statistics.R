#' Descriptive Statistics Panel Module
#'
#' @description
#' This module controls the Descriptive Statistics panel component. It is
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
#' |         |  | Descriptive Statistics Panel  |  |
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
#' @template param-data-sample
#'
#' @returns
#' [ui_panel_descriptive_statistics()] returns a `shiny.tag` object
#' (an output of [bslib::nav_panel()]).
#'
#' [server_panel_descriptive_statistics()] returns a [shiny::reactive()]
#' object. It can be called to get the panel's title.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-panel-descriptive-statistics
#' @export
ui_panel_descriptive_statistics <- function(id) {
    ns <- shiny::NS(id)
    card_height <- getOption("app_card_height_md")

    # Descriptive Statistics ---------------------------------------------------

    stats <- bslib::card(
        height      = card_height,
        full_screen = TRUE,

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("stats_title"), tags$span)
            )
        ),

        bslib::card_body(
            class = "px-5",
            shiny::uiOutput(ns("stats"))
        )
    )

    # QQ Plot -------------------------------------------------------------------

    qq_plot <- bslib::card(
        height      = card_height,
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
        height      = card_height,
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
            width = 1/2,
            fill  = FALSE,
            stats,
            qq_plot,
            box_plot
        )
    )

    return(ui)
}

#' @rdname ui-panel-descriptive-statistics
#' @export
server_panel_descriptive_statistics <- function(
    id,
    lang,
    inputs_calc,
    data_sample)
{
    stopifnot(exprs = {
        shiny::is.reactive(lang)
        shiny::is.reactive(inputs_calc)
        shiny::is.reactive(data_sample)
    })

    server <- function(input, output, session) {
        title <- shiny::reactive({
            translate(lang = lang(), "About My Measurements")
        }) |>
        shiny::bindCache(lang())

        data_sample_imputed <- shiny::reactive({
            data_sample <- data_sample()

            simple.censored.treatment(
                observations.formatted = data_sample$data,
                notcensored            = data_sample$notcensored,
                leftcensored           = data_sample$leftcensored,
                rightcensored          = data_sample$rightcensored,
                intcensored            = data_sample$intcensored)
        })

        # The order of row and col names must
        # match what fun.desc.stat(() returns.
        stats_dim_names <- shiny::reactive({
            lang <- lang()
            list(
                rows = c(
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
                cols = c(
                    translate(lang = lang, "Sample Statistic"),
                    translate(lang = lang, "Value")
                )
            )
        }) |>
        shiny::bindCache(lang())

        output$title <- shiny::renderText({
            title()
        })

        output$stats_title <- shiny::renderText({
            translate(lang = lang(), "Descriptive Statistics")
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

        output$stats <- shiny::renderUI({
            lang <- lang()
            dim_names <- stats_dim_names()
            stats <- fun.desc.stat(data_sample_imputed(), data_sample()$c.oel)

            # Overwrite internal row names. They are stored in
            # the first column of whatfun.desc.stat() returns.
            stats$parameter <- dim_names$rows

            as_html_table(stats, colnames = dim_names$cols)
        })

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
                qqplot.6            = translate(lang = lang, "Not Censored"))
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
            data_sample <- data_sample()

            fun.boxplot(
                data.simply.imputed = data_sample_imputed(),
                notcensored         = data_sample$notcensored,
                c.oel               = data_sample$c.oel,
                boxplot.1           = translate(lang = lang, "Measurement Type"),
                boxplot.2           = translate(lang = lang, "Concentration"),
                boxplot.3           = translate(lang = lang, "OEL"),
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
                    distance between the %s and %s percentiles. The whiskers
                    (vertical lines) represent the distance between the %s
                    and %s percentiles. The inner black horizontal line is
                    the median.
                "),
                ordinal(25L, lang),
                ordinal(75L, lang),
                ordinal(10L, lang),
                ordinal(90L, lang)
            )
        }) |>
        shiny::bindCache(lang())

        return(title)
    }

    return(shiny::moduleServer(id, server))
}
