#' Tool1: Data Interpretation for One Similarly Exposed Group
#'
#' Define the user interface (`ui`) and [server()] logic.
#'
#' @usage
#' # Further arguments may be required.
#' shiny::runApp()
#'
#' @details
#' Static assets are stored in `www/` and are served under `/` at runtime by
#' the application. For example, file `www/css/main.css` has the following
#' URL: `http<s>://<domain>.<tld>/css/main.css`.
#'
#' Required objects and functions are stored in `R/` and loaded automatically
#' when the application is launch with [shiny::runApp()]. All scripts stored
#' in this directory are sourced.
#'
#' ## `scripts/`
#'
#' For historical reasons, the application further depends on a set of scripts
#' stored in `scripts/`. These must be explicitly loaded at runtime. See script
#' `R/global.R`. It is worthwhile noting they were **not** refactored.
#'
#' @section Naming Conventions:
#  All R custom functions, [shiny] reactive values, and identifiers (`inputId`
#' and `outputId`) use `snake_case`.
#'
#'   * Custom functions are easily identifiable as such.
#'   * Reactive values are functions and are treated like other custom
#'     functions.
#'   * Identifiers written in `snake_case` yield syntactic names.
#'
#' See file IO.md for more information.
#'
#' CSS Classes uses `dash-case`. Each class must use the prefix `app-` to
#' avoid collisions.
#'
#' @section Google Analytics:
#' Google Analytics has been deactivated until further notice. We are required
#' by law (Quebec's Loi 25, effective since September 2023) to inform users we
#' collect usage data from them. We further require a formal Terms of Service,
#' and a Privacy Policy.
#'
#' Here how it used to be in `html$head()` below.
#'
#' ```r
#' html$script(src = file.path("www", "js", "ga-id.js"))
#' html$script(src = file.path("www", "js", "ga.js"))
#' html$noscript(src = file.path("www", "js", "ga-tm.js"))
#' ```
#'
#' Google Analytics changed its API recently. This may require further tweaks.
#'
#' @author Jérôme Lavoué (<jerome.lavoue@@umontreal.ca>)
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)


# NOTE: (JMP) I would strongly consider breaking the app into Shiny modules
# or functions that returns each tabPanel().

# FIXME: (JMP) Many numeric values are passed to signif() which yields a
# variable number of (significant) digits. Is this truly needed? For UX
# purposes, I suggest always showing 1 or 2 digits?

# FIXME: (JMP) The app heavily relies on functions defined in scripts. These
# must be revamped (those containing plot functions being the top priority).
# Theis contents should be split into a proper set of functions stored in
# multiple R files in R/. Each function should be documented and manually
# tested (at least).

# FIXME: (JMP) Plots are not generated with the same margins. They must be
# standardized. For some plots, this is a bigger problem and I left a TODO.

# FIXME: (JMP) OEL (Occupational Exposure Limit) and EL (Exposure Limit)
# are both used interchangeably. Choose one and stick to it. Below, I
# chose the latter until further notice. All other inputs have slightly
# different names (sometimes) in the app. More consistency is required.


# User Interface ---------------------------------------------------------------


ui <- shiny::fluidPage(
    # TODO: (JMP) Integrate this value with transltr in a near future.
    lang  = "en",
    theme = bslib::bs_theme(version = 5L, preset = "flatly"),

    ## Head --------------------------------------------------------------------

    html$head(
        html$title(translate("Expostats | Tool 1")),
        html$link(rel = "stylesheet", media = "all", href = "css/main.css"),
        shinyjs::useShinyjs()
    ),

    ## Body --------------------------------------------------------------------

    html$h1(
        class = "app-title",
        translate("Tool 1: Data Interpretation for One Similarly Exposed Group")),

    shiny::sidebarLayout(

        ### Sidebar ------------------------------------------------------------

        shiny::sidebarPanel(
            width = 3L,

            #### Inputs --------------------------------------------------------

            shiny::numericInput(
                inputId = "sb_oel",
                label   = translate("Exposure Limit:"),
                value   = 100) |>
                htmltools::tagAppendAttributes(class = "app-input") |>
                bslib::tooltip(html$p(
                    class = "app-input-tooltip",
                    translate("
                        Use the exposure limit to assess overexposure. It
                        must have the same unit as the measurement data."))),

            shiny::numericInput(
                inputId = "sb_al",
                label   = translate("Exposure Limit Multiplier:"),
                value   = 1) |>
                htmltools::tagAppendAttributes(class = "app-input") |>
                bslib::tooltip(html$p(
                    class = "app-input-tooltip",
                    translate("
                        Use this multiplier to modify the exposure limit. The
                        product of the former and the latter is the actual
                        exposure limit value for calculation purposes."))),

            shiny::numericInput(
                inputId = "sb_conf",
                label   = translate("Credible Interval Probability:"),
                value   = 90) |>
                htmltools::tagAppendAttributes(class = "app-input") |>
                bslib::tooltip(html$p(
                    class = "app-input-tooltip",
                    translate("
                        Use this value as a probability for the credible
                        intervals around parameter estimates. It must be
                        between 0% and 100%. The default value is set equal
                        to 90%. The credible interval is the Bayesian
                        equivalent of the confidence interval."))),

            shiny::numericInput(
                inputId = "sb_psi",
                label   = translate("Overexposure Risk Threshold:"),
                value   = 30) |>
                htmltools::tagAppendAttributes(class = "app-input") |>
                bslib::tooltip(html$p(
                    class = "app-input-tooltip",
                    translate("
                        Use this value as the maximal overexposure risk. It
                        must be between 0% and 100%. It represents the maximal
                        probability that the overexposure limit is met. Above
                        this value, the situation should trigger remedial
                        action. INRS and BOHS suggest using 5% and 30%,
                        respectively."))),

            add_input_text_area(
                inputId = "sb_data",
                label   = translate("Measurements:"),
                value   = c(
                    "28.9",
                    "19.4",
                    "<5.5",
                    "149.9",
                    "26.42",
                    "56.1")) |>
                htmltools::tagAppendAttributes(class = "app-input") |>
                bslib::tooltip(html$p(
                    class = "app-input-tooltip",
                    translate("
                        The measurement dataset. There must be one value per
                        line. Values can be censored to the left (<), to the
                        right (>), or interval censored ([X-Y])."))),

            # This output is only shown when the active panel is
            # exceedance. See observer in section Sidebar of server().
            shiny::numericInput(
                inputId = "sb_frac_threshold",
                label   = translate("Exceedance Fraction Threshold:"),
                value   = 5) |>
                htmltools::tagAppendAttributes(
                    class = "app-input",
                    style = "display: none;") |>
                bslib::tooltip(html$p(
                    class = "app-input-tooltip",
                    translate("
                        Use this value as an acceptable proportion of exposures
                        above the exposure limit (OEL). It must be between 0%
                        and 100%. The traditional default value is 5%."))),

            # This output is only shown when the active panel is
            # percentiles. See observer in section Sidebar of server().
            shiny::numericInput(
                inputId = "sb_target_perc",
                label   = translate("Critical Percentile:"),
                value   = 95) |>
                htmltools::tagAppendAttributes(
                    class = "app-input",
                    style = "display: none;") |>
                bslib::tooltip(html$p(
                    class = "app-input-tooltip",
                    translate("
                        Use this value to set the percentile of the exposure
                        distribution that will be compared to the OEL. It must
                        be between 0% and 100%. The traditional default value
                        is 95%."))),

            #### Footer --------------------------------------------------------

            # A footer can be passed to shiny::tabsetPanel(), but it
            # is placed here to maximize visibility on all screens.
            html$footer(
                class = "app-footer",
                html$p(translate("Tool 1"), "version", current_version),

                html$p(sprintf_html(translate(
                    "Source code available on %s."),
                    a_strs[["source"]])),

                # This produces a <p> containing a single line of text:
                # <a>Jérôme Lavoué</a> (YYYY). All rights reserved.
                html$p(
                    static$a$jerome_lavoue,
                    sprintf("(%s)", current_year),
                    translate("All rights reserved"))
            )
        ),

        ### Main ---------------------------------------------------------------

        shiny::mainPanel(width = 8L,

            #### Top Banner for Calculations -----------------------------------

            # It is shown whenever the Shiny engine is blocked.
            shiny::conditionalPanel(
                html$p(
                    class = "app-banner-wait",
                    translate("Calculating... Please wait.")),
                condition = r"{$("html").hasClass("shiny-busy")}"
            ),

            #### Panels --------------------------------------------------------

            shiny::tabsetPanel(id = "active_panel",

                ##### Panel: Statistics ----------------------------------------

                shiny::tabPanel(
                    value = "statistics",
                    title = translate("Statistics"),

                    ##### Descriptive Statistics -------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Descriptive Statistics")),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            html$h3(
                                class = "app-panel-subtitle",
                                translate("Summary")),

                            shiny::tableOutput("st_stats_tbl"),
                        ),
                        shiny::column(width = 6L,
                            add_bs_alert_info(
                                html$p(translate("
                                    Censored measurements are subject to one
                                    of the following procedure.")),

                                html$ul(
                                    html$li(translate("
                                        Interval censored measurements are
                                        imputed as the mid-range.")),

                                    html$li(translate("
                                        Measurements censored to the right are
                                        imputed as 9/4 of the censoring point.")),

                                    html$li(sprintf_html(translate("
                                        Measurements censored to the left are
                                        treated using robust regression on order
                                        statistics, or a Log-probit regression.
                                        The algorithm used is derived from %s
                                        (itself derived from previous work of
                                        %s)."),
                                        a_strs[["expostats_ndexpo"]],
                                        a_strs[["dennis_helsel"]]))
                                )
                            )
                        )
                    ),

                    ##### QQ Plot ----------------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Quantile-Quantile Plot")),

                    # This plot is ugly if it is rendered using the
                    # full available width of its parent container.
                    # Width is restricted to 50%, and margin are set
                    # by the browser to center the underlying image.
                    # plotOutput() rerturns a <div> tag by default.
                    shiny::plotOutput("st_qq_plot",
                        width  = "50%",
                        height = plot_height) |>
                        htmltools::tagAppendAttributes(style = "margin: auto;"),

                    # TODO: (JMP) Standardize margins and remove style.
                    html$p(style = "margin: 10.5px 0 0 0;", translate("
                        The points above should follow a straight line. Random
                        deviations from it are expected. However, significant
                        deviations suggest that the data may have to be split
                        into distinct subsets, or that some outliers must be
                        investigated.")),

                    ##### Box and Whiskers Plot --------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Box and Whiskers Plot")),

                    shiny::plotOutput("st_box_plot", height = plot_height),

                    html$p(sprintf_html(translate("
                        The measurements are scattered around the x-axis middle
                        point. The box (outer horizontal lines) represents the
                        spread between the 25<sup>th</sup> and 75<sup>th</sup>
                        percentiles. The whiskers (vertical lines) represent the
                        spread between the 10<sup>th</sup> and 90<sup>th</sup>
                        percentiles. The inner horizontal line is the median."))),
                ),

                ##### Panel: Exceedance Fraction -------------------------------

                shiny::tabPanel(
                    value = "exceedance",
                    title = translate("Exceedance Fraction"),

                    ###### Risk Decision ---------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Risk Analysis Based on the Exceedance Fraction")),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            html$h3(
                                class = "app-panel-subtitle",
                                translate("Risk Decision")),

                            html$ul(
                                class = "app-ul",

                                html$li(sprintf_html(translate("
                                    Overexposure is defined as the exceedance
                                    fraction being greater than or equal to %s."),
                                    add_bold_text_output("ef_sb_frac_threshold_percent_1"))),

                                html$li(sprintf_html(translate("
                                    The probability that this criterion is met
                                    is equal to %s."),
                                    add_bold_text_output("ef_risk_prob_criterion"))),

                                html$li(sprintf_html(translate("
                                    The probability that this criterion is met
                                    should be lower than %s."),
                                    add_bold_text_output("ef_risk_prob_limit_1"))),

                                html$li(sprintf_html(translate("
                                    Consequently, the current situation is
                                    declared to be %s."),
                                    add_bold_text_output("ef_risk_decision")))
                            ),

                            html$p(translate("
                                This risk meter shows the probability of the
                                exposure being too high when compared to the
                                occupational exposure limit. The red zone
                                indicates a poorly controlled exposure."))
                        ),

                        shiny::column(width = 6L,
                            # Add a top margin to the plot <img> to match the
                            # margin of top <h3> above (see other column. This
                            # aligns both elements horizontally.
                            htmltools::tagAppendAttributes(
                                style = "margin-top: 21px",
                                shiny::plotOutput(
                                    outputId = "ef_risk_meter_plot",
                                    height   = plot_risk_meter_height))
                        )
                    ),

                    ###### Parameter Estimates ---------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Parameters Estimates")),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            html$h3(
                                class = "app-panel-subtitle",
                                translate("Distribution Parameters")),

                            html$ul(
                                class = "app-ul",

                                html$li(sprintf_html(translate("
                                    The geometric mean point estimate is equal
                                    to %s."),
                                    add_bold_text_output("ef_estimate_geo_mean"))),

                                html$li(sprintf_html(translate("
                                    The geometric standard deviation point
                                    estimate is equal to %s."),
                                    add_bold_text_output("ef_estimate_geo_sd")))
                            )
                        ),
                        shiny::column(width = 6L,
                            html$h3(
                                class = "app-panel-subtitle",
                                translate("Exceedance Fraction")),

                            html$ul(
                                class = "app-ul",
                                html$li(sprintf_html(translate("
                                    The point estimate is equal to %s."),
                                    add_bold_text_output("ef_estimate")))
                            )
                        )
                    ),

                    html$p(translate("
                        Square brackets indicate the underlying
                        credible intervals.")),

                    ###### Exceedance Plot -------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Exceedance Plot")),

                    html$p(translate("
                        The following plot illustrates the proportion of exposures
                        that would be above the OEL in a fictional sample of one
                        hundred measurements. Each flask represents an exposure.
                        Red flasks correspond to exposures that are above the
                        exposure limit. The plot can be shown in one of four
                        variations. You may choose any variant (an alternative
                        way of displaying the same information) below and,
                        optionally, customize colors.")),

                    shiny::radioButtons(
                        inputId  = "ef_exceed_plot_btn_variant",
                        label    = translate("Variants:"),
                        inline   = TRUE,
                        choices  = c(
                            # What users see = Internal input value.
                            `1` = "figure1",
                            `2` = "figure2",
                            `3` = "figure3",
                            `4` = "figure4")),

                    shiny::actionButton(
                        style   = "margin-bottom: 15px;",
                        inputId = "ef_exceed_plot_btn_custom",
                        label   = translate("Customize Colors"),
                        icon    = static$icons$bottom),

                    # The <fieldset> is initially hidden, and is
                    # either shown, or hidden whenever the user
                    # clicks on the action button above.
                    add_input_field_set(
                        inputId      = "ef_exceed_plot_cols",
                        container_id = "ef_exceed_plot_cols_container",
                        label        = translate("Colors:"),
                        style        = "display: none;",
                        inputs       = list(
                            colourpicker::colourInput(
                                inputId    = "ef_exceed_plot_col_risk",
                                label      = translate("Flask Color (Exceedance):"),
                                value      = "red",
                                returnName = TRUE,
                                palette    = "limited"),
                            colourpicker::colourInput(
                                inputId    = "ef_exceed_plot_col_no_risk",
                                label      = translate("Flask Color (No Exceedance):"),
                                value      = "gray50",
                                returnName = TRUE,
                                palette    = "limited"),
                            colourpicker::colourInput(
                                inputId    = "ef_exceed_plot_col_bg",
                                label      = translate("Background Color (Default):"),
                                value      = "gray70",
                                returnName = TRUE,
                                palette    = "limited"),
                            colourpicker::colourInput(
                                inputId    = "ef_exceed_plot_col_bg_threshold",
                                label      = translate("Background Color (Threshold):"),
                                value      = "gray40",
                                returnName = TRUE,
                                palette    = "limited"))),

                    # Width is set by a function passed to renderPlot().
                    shiny::plotOutput(
                        outputId = "ef_exceed_plot",
                        width    = "auto",
                        height   = plot_height) |>
                        # This class is used to center
                        # variants that only shows 1 plot.
                        htmltools::tagAppendAttributes(class = "app-center-plot") |>
                        # This color is extracted from
                        # the chosen shiny theme (flatly).
                        shinycssloaders::withSpinner(
                            type  = 8L,
                            color = "#212529"),

                    shiny::textOutput(
                        outputId  = "ef_exceed_plot_description",
                        container = html$p),

                    ###### Sequential Plot -------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Sequential Plot")),

                    shiny::plotOutput("ef_seq_plot", height = plot_height),

                    # TODO: (JMP) Standardize margins and remove style.
                    html$p(style = "margin: 10.5px 0 0 0;", translate("
                        This plot shows the estimated exposure distribution when
                        assuming 250 exposure measurements have been collected.
                        If the measurements represent 8-hour TWA (Time-Weighted
                        Average) values, this approximately represents a full
                        year of exposure. The OEL is shown as a red line.")),

                    ###### Density Plot ----------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Density Plot")),

                    shiny::plotOutput("ef_dist_plot", height = plot_height),

                    html$p(translate("
                        This plot shows the probability density function of the
                        estimated distribution of exposures. The OEL is shown as
                        a red line. The exceedance fraction is the area under
                        the curve beyond the OEL value.")),

                    ###### Risk Band Plot --------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Risk Band Plot")),

                    shiny::plotOutput("ef_risk_band_plot", height = plot_height),

                    html$p(sprintf_html(translate("
                        This plot shows the probability distribution of the
                        uncertainty around the exceedance fraction. It shows
                        the probability that the true exceedance fraction is
                        below %s, between %s and %s, and greater than %s. The
                        red column represents the probability of an
                        overexposure. The latter should be lower than the %s
                        threshold shown by the black dashed line."),
                        shiny::textOutput("ef_good_exposure_percent_1", inline = TRUE),
                        shiny::textOutput("ef_good_exposure_percent_2", inline = TRUE),
                        shiny::textOutput("ef_sb_frac_threshold_percent_2", inline = TRUE),
                        shiny::textOutput("ef_sb_frac_threshold_percent_3", inline = TRUE),
                        shiny::textOutput("ef_risk_prob_limit_2", inline = TRUE)))
                ),

                ##### Panel: Percentiles ---------------------------------------

                shiny::tabPanel(
                    value = "percentiles",
                    title = translate("Percentiles"),

                    ###### Risk Decision ---------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Risk Analysis Based on Percentiles")),

                    shiny::fluidRow(
                        shiny::column(width = 6L,

                            html$h3(
                                class = "app-panel-subtitle",
                                translate("Risk Decision")),

                            html$ul(
                                class = "app-ul",

                                html$li(sprintf_html(translate("
                                    Overexposure is defined as the %s percentile
                                    being greater than or equal to the OEL."),
                                    add_bold_html_output("pe_sb_target_perc_ordinal_1"))),

                                html$li(sprintf_html(translate("
                                    The probability that this criterion is met
                                    is equal to %s."),
                                    add_bold_text_output("pe_risk_prob_criterion"))),

                                html$li(sprintf_html(translate("
                                    The probability that this criterion is met
                                    should be lower than %s."),
                                    add_bold_text_output("pe_risk_prob_limit_1"))),

                                html$li(sprintf_html(translate("
                                    Consequently, the current situation is
                                    declared to be %s."),
                                    add_bold_text_output("pe_risk_decision")))
                            ),

                            html$p(translate("
                                This risk meter shows the probability of the
                                exposure being too high when compared to the
                                occupational exposure limit. The red zone
                                indicates a poorly controlled exposure."))
                        ),

                        shiny::column(width = 6L,
                            # Add a top margin to the plot <img> to match the
                            # margin of top <h3> above (see other column. This
                            # aligns both elements horizontally.
                            htmltools::tagAppendAttributes(
                                style = "margin-top: 21px",
                                shiny::plotOutput(
                                    outputId = "pe_risk_meter_plot",
                                    height   = plot_risk_meter_height))
                        )
                    ),

                    ###### Parameter Estimates ---------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Parameters Estimates")),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            html$h3(
                                class = "app-panel-subtitle",
                                translate("Distribution Parameters")),

                            html$ul(
                                class = "app-ul",

                                html$li(sprintf_html(translate("
                                    The geometric mean point estimate is equal
                                    to %s."),
                                    add_bold_text_output("pe_estimate_geo_mean"))),

                                html$li(sprintf_html(translate("
                                    The geometric standard deviation point
                                    estimate is equal to %s."),
                                    add_bold_text_output("pe_estimate_geo_sd")))
                            )
                        ),
                        shiny::column(width = 6L,
                            html$h3(
                                class = "app-panel-subtitle",
                                shiny::htmlOutput(
                                    outputId = "pe_sb_target_perc_ordinal_2",
                                    inline   = TRUE),
                                    translate("Percentile Estimate")),

                            html$ul(
                                class = "app-ul",
                                html$li(sprintf_html(translate("
                                    The point estimate is equal to %s."),
                                    add_bold_text_output("pe_estimate")))
                            )
                        )
                    ),

                    html$p(translate("
                        Square brackets indicate the underlying
                        credible intervals.")),

                    ###### Sequential Plot -------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Sequential Plot")),

                    shiny::plotOutput("pe_seq_plot", height = plot_height),

                    html$p(translate("
                        This plot shows the estimated exposure distribution when
                        assuming 250 exposure measurements have been collected.
                        If the measurements represent 8-hour TWA (Time-Weighted
                        Average) values, this approximately represents a full
                        year of exposure. The OEL is shown as a red dotted
                        line and the point estimate of the selected percentile
                        as a continuous blue line.")),

                    ###### Density Plot ----------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Density Plot")),

                    shiny::plotOutput("pe_dist_plot", height = plot_height),

                    html$p(translate("
                        This plot shows the probability density function of the
                        estimated distribution of exposures. The OEL is shown as
                        a red dotted line and the point estimate of the selected
                        percentile as a continuous blue line.")),

                    ###### Risk Band Plot --------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Risk Band Plot")),

                    shiny::plotOutput("pe_risk_band_plot", height = plot_height),

                    # TODO: (JMP) Standardize margins and remove style.
                    html$p(
                        style = "margin: 10.5px 0 0 0;",
                        sprintf_html(translate("
                            This plot shows the probability distribution of the
                            uncertainty around the selected percentile. It shows
                            the probability that the true %s percentile is (1)
                            below 1%% of the OEL, (2) between 1%% and 10%% of
                            the OEL, (3) between 10%% and 50%% of the OEL, (4)
                            between 50%% and 100%% of the OEL, and (5) greater
                            than the OEL. This is based on the classification
                            adopted by %s. The red column represents the
                            probability of an overexposure. The latter should
                            be lower than the %s threshold shown by the black
                            dashed line."),
                        shiny::htmlOutput("pe_sb_target_perc_ordinal_3", inline = TRUE),
                        a_strs[["aiha"]],
                        shiny::textOutput("pe_risk_prob_limit_2", inline = TRUE)))
                ),

                ##### Panel: Arithmetic Mean -----------------------------------

                shiny::tabPanel(
                    value = "arithmetic_mean",
                    title = translate("Arithmetic Mean"),

                    ###### Risk Decision ---------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Risk Analysis Based on the Arithmetic Mean")),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            html$h3(
                                class = "app-panel-subtitle",
                                translate("Risk Decision")),

                            html$ul(
                                class = "app-ul",

                                html$li(translate("
                                    Overexposure is defined as the arithmetic
                                    mean being greater than or equal to the OEL.")),

                                html$li(sprintf_html(translate("
                                    The probability that this criterion is met
                                    is equal to %s."),
                                    add_bold_text_output("am_risk_prob_criterion"))),

                                html$li(sprintf_html(translate("
                                    The probability that this criterion is met
                                    should be lower than %s."),
                                    add_bold_text_output("am_risk_prob_limit_1"))),

                                html$li(sprintf_html(translate("
                                    Consequently, the current situation is
                                    declared to be %s."),
                                    add_bold_text_output("am_risk_decision")))
                            ),

                            html$p(translate("
                                This risk meter shows the probability of the
                                exposure being too high when compared to the
                                occupational exposure limit. The red zone
                                indicates a poorly controlled exposure.")),

                            add_bs_alert_warn(html$p(translate("
                                The risk assessment based on AM relies on the
                                availability of a long-term averaged OEL
                                (LTA-OEL in the AIHA terminology), representing
                                a cumulative burden threshold. Most current OELs
                                are not created as LTA-OEL. Despite an annoying
                                lack of precise definition by most organizations,
                                they should be most often viewed as thresholds
                                to be exceeded as few times as possible. Some
                                authors have suggested using one-tenth of the
                                OEL as a practical LTA-OEL when assessing risk
                                using the arithmetic mean.")))
                        ),

                        shiny::column(width = 6L,
                            # Add a top margin to the plot <img> to match the
                            # margin of top <h3> above (see other column. This
                            # aligns both elements horizontally.
                            htmltools::tagAppendAttributes(
                                style = "margin-top: 21px",
                                shiny::plotOutput(
                                    outputId = "am_risk_meter_plot",
                                    height   = plot_risk_meter_height))
                        )
                    ),

                    ###### Parameter Estimates ---------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Parameters Estimates")),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            html$h3(
                                class = "app-panel-subtitle",
                                translate("Distribution Parameters")),

                            html$ul(
                                class = "app-ul",

                                html$li(sprintf_html(translate("
                                    The geometric mean point estimate is equal
                                    to %s."),
                                    add_bold_text_output("am_estimate_geo_mean"))),

                                html$li(sprintf_html(translate("
                                    The geometric standard deviation point
                                    estimate is equal to %s."),
                                    add_bold_text_output("am_estimate_geo_sd")))
                            )
                        ),
                        shiny::column(width = 6L,
                            html$h3(
                                class = "app-panel-subtitle",
                                translate("Arithmetic Mean")),

                            html$ul(
                                class = "app-ul",
                                html$li(sprintf_html(translate("
                                    The point estimate is equal to %s."),
                                    add_bold_text_output("am_estimate")))
                            )
                        )
                    ),

                    html$p(translate("
                        Square brackets indicate the underlying
                        credible intervals.")),

                    ###### Sequential Plot -------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Sequential Plot")),

                    shiny::plotOutput("am_seq_plot", height = plot_height),

                    html$p(translate("
                        This plot shows the estimated exposure distribution when
                        assuming 250 exposure measurements have been collected.
                        If the measurements represent 8-hour TWA (Time-Weighted
                        Average) values, this approximately represents a full
                        year of exposure. The OEL is shown as a red dotted
                        line and the point estimate of the arithmetic mean
                        as a continuous green line.")),

                    ###### Density Plot ----------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Density Plot")),

                    shiny::plotOutput("am_dist_plot", height = plot_height),

                    html$p(translate("
                        This plot shows the probability density function of the
                        estimated distribution of exposures. The OEL is shown as
                        a red dotted line and the point estimate of the
                        arithmetic mean as a continuous green line.")),

                    ###### Risk Band Plot --------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Risk Band Plot")),

                    shiny::plotOutput("am_risk_band_plot", height = plot_height),

                    # TODO: (JMP) Standardize margins and remove style.
                    html$p(
                        style = "margin: 10.5px 0 0 0;",
                        sprintf_html(translate("
                            This plot shows the probability distribution of the
                            uncertainty around the arithmetic mean. It shows
                            the probability that its true value is (1) below
                            1%% of the OEL, (2) between 1%% and 10%% of
                            the OEL, (3) between 10%% and 50%% of the OEL, (4)
                            between 50%% and 100%% of the OEL, and (5) greater
                            than the OEL. This is based on the classification
                            adopted by %s. The red column represents the
                            probability of an overexposure. The latter should
                            be lower than the %s threshold shown by the black
                            dashed line."),
                        a_strs[["aiha"]],
                        shiny::textOutput("am_risk_prob_limit_2", inline = TRUE)))
                ),

                ##### Panel: About ---------------------------------------------

                shiny::tabPanel(
                    value = "about",
                    title = translate("About"),

                    ###### About -----------------------------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("About")),

                    html$p(sprintf_html(translate("
                        This application (and related tools) are developped by
                        the Industrial Hygiene team of the Department of
                        Environmental and Occupational Health at the %s of the
                        %s. The source code is available on %s."),

                        # TODO: (JMP) This must depend on future input$lang.
                        # It will likely move to server() because of that,
                        # like all calls to translate().
                        a_strs[[if (TRUE) "epsum_en" else "epsum_fr"]],
                        a_strs[[if (TRUE) "udm_en"   else "udm_fr"]],
                        a_strs[["source"]])),

                    html$p(sprintf_html(translate("
                        %s (Jean-Mathieu Potvin) collaborated on the development
                        efforts (partial refactoring of the source code) that
                        led to version 4.0.0."),
                        a_strs[["ununoctium"]])),

                    ###### How To Use This Application -------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("How to Use This Application")),

                    html$p(translate("
                        This application eases the interpretation of industrial
                        hygiene measurements. Notably, it helps with checking
                        compliance with respect to an occupational exposure
                        limit (OEL). It is based on a risk assessment framework
                        recognized by prominent institutions such as the American
                        Industrial Hygiene Association, the British and Dutch
                        Society for Occupational Health and Safety (BOHS/NVVA),
                        the French Institut national de recherche et de sécurité
                        (INRS), and the European Standards Organization.")),

                    html$p(translate("
                        It assumes that input measurements (measurements)
                        represent a random sample stemming from the distribution
                        of exposures that underlie the sampled context. In other
                        words, the data is representative of the specific
                        exposure regimen one wishes to assess.")),

                    html$p(translate("
                        The application is straightforward to use. Follow
                        these three steps.")),

                    html$ul(
                        html$li(translate("
                            Enter your measurements under Measurements in
                            the left panel. There must be one value per line.
                            Write them as you would in your favourite text
                            editor. You may also copy and paste values stored
                            in a spreadsheet's column. The initial dataset
                            provided for illustration purposes can be deleted
                            as you usually would in any text editor.")),

                        html$li(translate("
                            Enter other parameters (in the same panel above
                            Measurements).")),

                        html$li(translate("
                            Wait for the calculations to be performed."))
                    ),

                    html$p(translate(
                        "Results are updated whenever an input changes.")),

                    html$p(translate("
                        Censored values are written as <X (left censored),
                        >X (right censored), or [X1-X2] (interval censored),
                        where X is the censored value. It must have the same
                        unit as other non-censored measurements.")),

                    ###### Methodological Background ---------------------------

                    html$h2(
                        class = "app-panel-title",
                        translate("Methodological Background")),

                    html$p(translate("
                        This application uses a Bayesian approach to estimate
                        the parameters of the log-normal distribution.")),

                    html$ul(
                        html$li(translate("
                            It yields a more intuitive rationale compared
                            to traditional (frequentist) methods.")),

                        html$li(translate("
                            It naturally integrates the treatment of
                            non-detects.")),

                        html$li(translate("
                            It allows the inclusion of external information
                            in the measurements (not yet leveraged by the
                            application)."))
                    ),

                    html$p(sprintf_html(translate("
                        The Bayesian models and data interpretation procedures
                        used by this application are derived from current best
                        practices in industrial hygiene, which are described in
                        the following scientific paper: Jérôme Lavoué, Lawrence
                        Joseph, Peter Knott, Hugh Davies, France Labrèche,
                        Frédéric Clerc, Gautier Mater, Tracy Kirkham,
                        Expostats: A Bayesian Toolkit to Aid the Interpretation
                        of Occupational Exposure Measurements, Annals of Work
                        Exposures and Health, Volume 63, Issue 3, April 2019,
                        Pages 267-279, %s."),
                        a_strs[["expostats_paper"]])),

                    html$p(sprintf_html(translate("
                        Additional details and references are available on %s."),

                        # TODO: (JMP) This must depend on future input$lang.
                        # It will likely move to server() because of that,
                        # like all calls to translate().
                        a_strs[[if (TRUE) "expostats_info_en" else "expostats_info_fr"]]))
                )
            )
        )
    )
)


# Server logic -----------------------------------------------------------------


server <- function(input, output, session) {

    ## Sidebar -----------------------------------------------------------------

    # This observer hides inputs sb_frac_threshold and sb_target_perc
    # by default, and respectively shows either of them only when
    # a specific panel is opened.
    shiny::observeEvent(input$active_panel, {
        shinyjs::hide("sb_frac_threshold")
        shinyjs::hide("sb_target_perc")
        switch(input$active_panel,
            exceedance  = shinyjs::show("sb_frac_threshold"),
            percentiles = shinyjs::show("sb_target_perc"))
    })

    # Values -------------------------------------------------------------------

    # TODO: (JMP) Ask JL what to do with this. This is a weird case.
    # This reactive value is called by bayesian_analysis below. It has
    # no other reference in the code. This could later be refactored by
    # removing it, and passing 1L to fun.bayes.jags().
    uninformed_prior <- shiny::reactive({
        # FIXME: (JMP) Comments refer to input prior.sigma, which does
        # not exist. It could be an earlier UI artifact that was left
        # over as a comment and later removed by JMP. Commenting code
        # is a bad idea. Use Git instead.
        prior <- "1" # input$prior.sigma

        # FIXME: (JMP) !strtoi("1") yields FALSE, which is most likely
        # implicitly coerced to 0L below. I don't know what this does,
        # and as.integer() should always be used outside of if().
        return(!strtoi(prior))
    })

    user_inputs <- shiny::reactive({
        return(
            list(
                conf           = input$sb_conf,
                psi            = input$sb_psi,
                frac_threshold = input$sb_frac_threshold,
                target_perc    = input$sb_target_perc))
    })

    user_formatted_sample <- shiny::reactive({
        return(
            data.formatting.SEG(
                data.in  = input$sb_data,
                oel      = input$sb_oel,
                oel.mult = input$sb_al))
    })

    bayesian_analysis <- shiny::reactive({
        on.exit(progress$close())
        progress <- shiny::Progress$new()
        progress$set(
            value   = 0L,
            message = translate("Bayesian iterations:"))

        # FIXME: (JMP) This is really weird. All Bayesian functions accept a
        # function that updates a Progress object implicitly. This is error-
        # prone and requires a fix. Binding 'progress' may not exist, or could
        # be out of scope in some cases.
        updateProgress <- function(detail = NULL) {
            progress$inc(amount = 1/50, detail = detail)
        }

        user_sample <- user_formatted_sample()
        return(
            fun.bayes.jags(
                observations     = user_sample$data,
                notcensored      = user_sample$notcensored,
                leftcensored     = user_sample$leftcensored,
                rightcensored    = user_sample$rightcensored,
                intcensored      = user_sample$intcensored,
                seed             = user_sample$seed,
                c.oel            = user_sample$c.oel,
                n.iter           = 25000L,
                uninformed.prior = uninformed_prior(),
                updateProgress   = updateProgress))
    })

    num_results <- shiny::reactive({
        bayesian_outputs <- bayesian_analysis()
        inputs           <- user_inputs()
        return(
            all.numeric(
                mu.chain       = bayesian_outputs$mu.chain,
                sigma.chain    = bayesian_outputs$sigma.chain,
                c.oel          = user_formatted_sample()$c.oel,
                conf           = inputs$conf,
                frac_threshold = inputs$frac_threshold,
                target_perc    = inputs$target_perc))
    })

    ## Shared Outputs ----------------------------------------------------------

    output$ef_risk_prob_limit_1 <-
    output$ef_risk_prob_limit_2 <-
    output$pe_risk_prob_limit_1 <-
    output$pe_risk_prob_limit_2 <-
    output$am_risk_prob_limit_1 <-
    output$am_risk_prob_limit_2 <- shiny::renderText({
        return(paste0(input$sb_psi, "%"))
    })

    output$ef_estimate_geo_mean <-
    output$pe_estimate_geo_mean <-
    output$am_estimate_geo_mean <- shiny::renderText({
        gm <- lapply(num_results()$gm, \(x) as.character(signif(x, 2L)))
        return(sprintf("%s [%s - %s]", gm$est, gm$lcl, gm$ucl))
    })

    output$ef_estimate_geo_sd <-
    output$pe_estimate_geo_sd <-
    output$am_estimate_geo_sd <- shiny::renderText({
        gsd <- lapply(num_results()$gsd, \(x) as.character(signif(x, 2L)))
        return(sprintf("%s [%s - %s]", gsd$est, gsd$lcl, gsd$ucl))
    })

    ## Panel: Statistics -------------------------------------------------------

    user_formatted_sample_imputed <- reactive({
        user_sample <- user_formatted_sample()
        return(
            simple.censored.treatment(
                observations.formatted = user_sample$data,
                notcensored            = user_sample$notcensored,
                leftcensored           = user_sample$leftcensored,
                rightcensored          = user_sample$rightcensored,
                intcensored            = user_sample$intcensored))
    })

    ### Descriptive Statistics -------------------------------------------------

    output$st_stats_tbl <- shiny::renderTable(
        rownames = FALSE,
        spacing  = "m",
        hover    = TRUE,
        expr     = {
            stats_df <- fun.desc.stat(
                data.simply.imputed = user_formatted_sample_imputed(),
                c.oel = user_formatted_sample()$c.oel)

            # <sup> tag cannot be used in a renderTable()
            # call. Instead, we use superscripts ᵗʰ. These
            # are valid UTF characters that can be rendered
            # by browsers. This is not optimal, but better
            # than nothing.
            stats_df$parameter <- c(
                translate("Number of Obversations"),
                translate("Proportion Censored"),
                translate("Minimum"),
                sprintf("25ᵗʰ %s", translate("Percentile")),
                translate("Median"),
                sprintf("75ᵗʰ %s", translate("Percentile")),
                translate("Maximum"),
                translate("Proportion > OEL"),
                translate("Arithmetic Mean"),
                translate("Arithmetic Standard Deviation"),
                translate("Coefficient of Variation"),
                translate("Geometric Mean"),
                translate("Geometric Standard Deviation"))

            colnames(stats_df) <- c(
                translate("Parameter"),
                translate("Value"))

            return(stats_df)
    })

    ### QQ Plot ----------------------------------------------------------------

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter and semantic names.
    output$st_qq_plot <- shiny::renderPlot({
        return(
            fun.qqplot(
                data.simply.imputed = user_formatted_sample_imputed(),
                notcensored         = user_formatted_sample()$notcensored,
                qqplot.1            = translate("Quantile-Quantile Plot"),
                qqplot.2            = translate("Quantiles (Lognormal Distribution)"),
                qqplot.3            = translate("Quantiles (Standardized Measurements)"),
                qqplot.4            = translate("Measurement Type"),
                qqplot.5            = translate("Censored"),
                qqplot.6            = translate("Detected")))
    })

    ### Box and Whiskers Plot --------------------------------------------------

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter and semantic names.
    output$st_box_plot <- shiny::renderPlot({
        user_sample <- user_formatted_sample()
        return(
            fun.boxplot(
                data.simply.imputed = user_formatted_sample_imputed(),
                notcensored         = user_sample$notcensored,
                c.oel               = user_sample$c.oel,
                boxplot.1           = translate("Measurement Type"),
                boxplot.2           = translate("Concentration"),
                boxplot.3           = translate("Exposure Limit"),
                boxplot.4           = translate("Censored"),
                boxplot.5           = translate("Not Censored"),
                boxplot.6           = translate("Measurements")))
    })

    ## Panel: Exceedance Fraction ----------------------------------------------

    ### UI ---------------------------------------------------------------------

    # Each click on ef_exceed_plot_btn_custom triggers two actions:
    #   1. the icon of the button is updated, and
    #   2. ef_exceed_plot_cols_container is either shown
    #      or hidden (based on the button's state).
    # The button state's starts at 0 (hidden). Odd numbers
    # correspond to a displayed container, and even numbers
    # to a hidden container.
    shiny::observeEvent(input$ef_exceed_plot_btn_custom, {
        new_icon <- if (input$ef_exceed_plot_btn_custom %% 2L == 0L) {
            static$icons$bottom
        } else {
            static$icons$top
        }

        shiny::updateActionButton(
            inputId = "ef_exceed_plot_btn_custom",
            icon    = new_icon)
        shinyjs::toggle("ef_exceed_plot_cols_container")
    })

    ### Values -----------------------------------------------------------------

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter English names.
    ef_exceed_plots <- shiny::reactive({
        seuil    <- input$sb_frac_threshold
        results  <- num_results()
        frac_est <- ceiling(results$frac$est)
        frac_ucl <- ceiling(results$frac$ucl)

        params_plots <- paramsVariantesFracDep(
            images_dir,
            file.path(images_dir, "flask.png"),
            file.path(images_dir, "flask-lines.png"),
            input$ef_exceed_plot_col_risk,
            input$ef_exceed_plot_col_no_risk,
            input$ef_exceed_plot_col_bg_threshold,
            input$ef_exceed_plot_col_bg)

        return(
            list(
                figure1 = list(
                    drawPlot(
                        params_plots,
                        fracDepasseEst = seuil,
                        titre          = translate("Acceptable Sample")),
                    drawPlot(
                        params_plots,
                        fracDepasseEst = frac_est,
                        titre          = translate("Current Sample"))),
                figure2 = list(
                    drawPlot(
                        params_plots,
                        fracDepasseEst = seuil,
                        titre          = translate("Acceptable Sample")),
                    drawPlot(
                        params_plots,
                        fracDepasseEst = frac_est,
                        fracDepasseLim = frac_ucl,
                        titre          = translate("Current Sample"))),
                figure3 = list(
                    drawPlot(
                        params_plots,
                        fracDepasseEst = frac_est,
                        seuil          = seuil)),
                figure4 = list(
                    drawPlot(
                        params_plots,
                        fracDepasseEst = frac_est,
                        fracDepasseLim = frac_ucl,
                        seuil          = seuil))))
    })

    ### Shared Outputs ---------------------------------------------------------

    output$ef_sb_frac_threshold_percent_1 <-
    output$ef_sb_frac_threshold_percent_2 <-
    output$ef_sb_frac_threshold_percent_3 <- shiny::renderText({
        return(sprintf("%.1f%%", input$sb_frac_threshold))
    })

    ### Risk Decision ----------------------------------------------------------

    # See sections Shared Outputs above for
    # output$ef_sb_frac_threshold_percent_1 and output$ef_risk_prob_limit_1.

    output$ef_risk_prob_criterion <- shiny::renderText({
        return(paste0(signif(num_results()$frac.risk, 3L), "%"))
    })

    output$ef_risk_decision <- shiny::renderText({
        if (num_results()$frac.risk >= user_inputs()$psi) {
            return(translate("poorly controlled"))
        }

        return(translate("adequately controlled"))
    })

    output$ef_risk_meter_plot <- shiny::renderPlot({
        return(
            dessinerRisqueMetre(
                actualProb          = num_results()$frac.risk,
                minProbUnacceptable = user_inputs()$psi))
    })

    ### Parameter Estimates ----------------------------------------------------

    # See section Shared Outputs above for output$ef_estimate_geo_mean,
    # and output$ef_estimate_geo_sd.

    output$ef_estimate <- shiny::renderText({
        frac <- lapply(num_results()$frac, \(x) as.character(signif(x, 3L)))
        return(sprintf("%s%% [%s - %s]", frac$est, frac$lcl, frac$ucl))
    })

    ### Exceedance Plot --------------------------------------------------------

    output$ef_exceed_plot <- shiny::renderPlot({
        ptlist <- ef_exceed_plots()[[input$ef_exceed_plot_btn_variant]]
        return(gridExtra::grid.arrange(grobs = ptlist, ncol = length(ptlist)))
    },
    # Variant 3 and 4 are just one plot. They are centered
    # by limiting their width (and by setting apppropriate
    # CSS classes, see plotOutput() above).
    width = \() {
        return(
            switch(input$ef_exceed_plot_btn_variant,
                figure1 = "auto",
                figure2 = "auto",
                figure3 = 700L,
                figure4 = 700L))
    })

    output$ef_exceed_plot_description <- shiny::renderText({
        return(
            switch(input$ef_exceed_plot_btn_variant,
                figure1 = translate("
                    The plot on the left shows an acceptable situation for the
                    chosen exceedance threshold (traditionally 5% above the OEL).
                    The plot on the right shows the situation estimated by the
                    Bayesian model. It does not take into account estimation
                    uncertainty."),
                figure2 = translate("
                    The plot on the left shows an acceptable situation for the
                    chosen exceedance threshold (traditionally 5% above the
                    OEL). The plot on the right shows the situation estimated
                    by the Bayesian model. It takes into account estimation
                    uncertainty with stripped symbols. The number of plain
                    symbols represents the best estimate of the number of
                    measurements above the OEL. The total number of symbols
                    (either plain or stripped) represents the maximum plausible
                    number of measurements above the OEL given estimation
                    uncertainty (using the upper limit of the underlying
                    credible interval)."),
                figure3 = translate("
                    This plot shows a shaded and darker region corresponding to
                    the maximal acceptable exceedance. Red symbols outside of it
                    are unacceptable exposures. It does not take into account
                    estimation uncertainty."),
                figure4 = translate("
                    This plot shows a shaded and darker region corresponding to
                    the maximal acceptable exceedance. Red symbols outside of
                    it are unacceptable exposures. It takes into account
                    estimation uncertainty with stripped symbols. The number of
                    plain symbols represents the best estimate of the number of
                    measurements above the OEL. The total number of symbols
                    (either plain or stripped) represents the maximum plausible
                    number of measurements above the OEL given estimation
                    uncertainty (using the upper limit of the underlying
                    credible interval).")
        ))
    })

    ### Sequential Plot --------------------------------------------------------

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter and semantic names.
    output$ef_seq_plot <- shiny::renderPlot({
        results <- num_results()
        return(
            sequential.plot.frac(
                gm        = results$gm$est,
                gsd       = results$gsd$est,
                frac      = results$frac$est,
                c.oel     = user_formatted_sample()$c.oel,
                seqplot.1 = translate("Concentration"),
                seqplot.2 = translate("Exceedance Fraction"),
                seqplot.6 = translate("Measurement Index")))
    })

    ### Density Plot -----------------------------------------------------------

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter and semantic names.
    output$ef_dist_plot <- shiny::renderPlot({
        bayesian_outputs <- bayesian_analysis()
        return(
            distribution.plot.frac(
                gm         = exp(median(bayesian_outputs$mu.chain)),
                gsd        = exp(median(bayesian_outputs$sigma.chain)),
                frac       = num_results()$frac$est ,
                c.oel      = user_formatted_sample()$c.oel,
                distplot.1 = translate("Concentration"),
                distplot.2 = translate("Density"),
                distplot.3 = translate("Exceedance Fraction"),
                distplot.4 = translate("OEL outside of graphical limits."),
                distplot.5 = translate("OEL")))
    })

    ### Risk Band Plot ---------------------------------------------------------

    output$ef_good_exposure_percent_1 <-
    output$ef_good_exposure_percent_2 <- shiny::renderText({
        return(paste0(input$sb_frac_threshold / 10, "%"))
    })

    # See subsection Shared Outputs above for
    # output$ef_sb_frac_threshold_percent_2 and output$ef_sb_frac_threshold_percent_3.

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter and semantic names.
    output$ef_risk_band_plot <- shiny::renderPlot({
        bayesian_outputs <- bayesian_analysis()
        inputs           <- user_inputs()
        return(
            riskband.plot.frac(
                mu.chain       = bayesian_outputs$mu.chain,
                sigma.chain    = bayesian_outputs$sigma.chain,
                c.oel          = user_formatted_sample()$c.oel,
                frac_threshold = inputs$frac_threshold,
                psi            = inputs$psi,
                riskplot.1     = translate("Exceedance Fraction Category"),
                riskplot.2     = translate("Probability")))
    })

    ## Panel: Percentiles ------------------------------------------------------

    ### Shared Outputs ---------------------------------------------------------

    output$pe_sb_target_perc_ordinal_1 <-
    output$pe_sb_target_perc_ordinal_2 <-
    output$pe_sb_target_perc_ordinal_3 <- shiny::renderUI({
        value <- input$sb_target_perc
        return(
            sprintf_html(
                "%.0f<sup>%s</sup>",
                value,
                ordinal_number_suffix(value)))
    })

    ### Risk Decision ----------------------------------------------------------

    # See section Shared Outputs above for output$pe_sb_target_perc_ordinal_1.

    output$pe_risk_prob_criterion <- shiny::renderText({
        return(paste0(signif(num_results()$perc.risk, 3L), "%"))
    })

    # See section Shared Outputs above for output$pe_risk_prob_limit_1.

    output$pe_risk_decision <-shiny::renderText({
        if (num_results()$perc.risk >= user_inputs()$psi) {
            return(translate("poorly controlled"))
        }

        return(translate("adequately controlled"))
    })

    output$pe_risk_meter_plot <- shiny::renderPlot({
        return(
            dessinerRisqueMetre(
                actualProb          = num_results()$perc.risk,
                minProbUnacceptable = user_inputs()$psi))
    })

    ### Parameter Estimates ----------------------------------------------------

    # See section Shared Outputs above for output$pe_estimate_geo_mean,
    # and output$pe_estimate_geo_sd.

    # See subsection Panel: Percentiles - Shared Outputs
    # above for output$pe_sb_target_perc_ordinal_2.

    output$pe_estimate <- shiny::renderText({
        perc <- lapply(num_results()$perc, \(x) as.character(signif(x, 3L)))
        return(sprintf("%s [%s - %s]", perc$est, perc$lcl, perc$ucl))
    })

    ### Sequential Plot --------------------------------------------------------

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter and semantic names.
    output$pe_seq_plot <- shiny::renderPlot({
        results     <- num_results()
        target_perc <- user_inputs()$target_perc
        return(
            sequential.plot.perc(
                gm                 = results$gm$est,
                gsd                = results$gsd$est,
                perc               = results$perc$est,
                c.oel              = user_formatted_sample()$c.oel,
                target_perc        = target_perc,
                target_perc_suffix = ordinal_number_suffix(target_perc),
                seqplot.1          = translate("Concentration"),
                seqplot.3          = translate("OEL"),
                seqplot.4          = translate("Percentile"),
                seqplot.6          = translate("Measurement Index")))
    })

    ### Density Plot -----------------------------------------------------------

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter and semantic names.
    output$pe_dist_plot <- shiny::renderPlot({
        bayesian_outputs <- bayesian_analysis()
        target_perc      <- user_inputs()$target_perc
        return(
            distribution.plot.perc(
                gm                 = exp(median(bayesian_outputs$mu.chain)),
                gsd                = exp(median(bayesian_outputs$sigma.chain)),
                perc               = num_results()$perc$est,
                target_perc        = target_perc,
                target_perc_suffix = ordinal_number_suffix(target_perc),
                c.oel              = user_formatted_sample()$c.oel,
                distplot.1         = translate("Concentration"),
                distplot.2         = translate("Density"),
                distplot.4         = translate("OEL outside of graphical limits."),
                distplot.5         = translate("OEL"),
                distplot.6         = translate("Percentile")))
    })

    ### Risk Band Plot ---------------------------------------------------------

    # See subsection Panel: Percentiles - Shared Outputs
    # above for output$pe_sb_target_perc_ordinal_3.

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter and semantic names.
    output$pe_risk_band_plot <- shiny::renderPlot({
        bayesian_outputs <- bayesian_analysis()
        inputs           <- user_inputs()
        return(
            riskband.plot.perc(
                mu.chain    = bayesian_outputs$mu.chain,
                sigma.chain = bayesian_outputs$sigma.chain,
                c.oel       = user_formatted_sample()$c.oel,
                target_perc = inputs$target_perc,
                psi         = inputs$psi,
                # ≤ may not render in all IDEs. This is Unicode
                # character U+2264 (&leq;) (Less-Than or Equal To).
                riskplot.2  = translate("Probability"),
                riskplot.3  = translate("≤ 1% OEL"),
                riskplot.4  = translate("1% < OEL ≤ 10%"),
                riskplot.5  = translate("10% < OEL ≤ 50%"),
                riskplot.6  = translate("50% < OEL ≤ 100%"),
                riskplot.7  = translate("> OEL"),
                riskplot.8  = translate("Critical Percentile Category")))
    })

    ## Panel: Arithmetic Mean --------------------------------------------------

    ### Risk Decision ----------------------------------------------------------

    # See section Shared Outputs above for output$am_risk_prob_limit_1.

    output$am_risk_prob_criterion <- shiny::renderText({
        return(paste0(signif(num_results()$am.risk, 3L), "%"))
    })

    output$am_risk_decision <-shiny::renderText({
        msgid <- if (num_results()$am.risk >= user_inputs()$psi) {
            return(translate("poorly controlled"))
        }

        return(translate("adequately controlled"))
    })

    output$am_risk_meter_plot <- renderPlot({
        return(
            dessinerRisqueMetre(
                actualProb          = num_results()$am.risk,
                minProbUnacceptable = user_inputs()$psi))
    })

    ### Parameter Estimates ----------------------------------------------------

    # See section Shared Outputs above for
    # output$am_estimate_geo_mean, and output$am_estimate_geo_sd.

    output$am_estimate <- shiny::renderText({
        am <- lapply(num_results()$am, \(x) as.character(signif(x, 3L)))
        return(sprintf("%s [%s - %s]", am$est, am$lcl, am$ucl))
    })

    ### Sequential Plot --------------------------------------------------------

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter and semantic names.
    output$am_seq_plot <- shiny::renderPlot({
        results <- num_results()
        return(
            sequential.plot.am(
                gm        = results$gm$est,
                gsd       = results$gsd$est,
                am        = results$am$est,
                c.oel     = user_formatted_sample()$c.oel,
                seqplot.1 = translate("Concentration"),
                seqplot.3 = translate("Exceedance Fraction"),
                seqplot.5 = translate("Arithmetic Mean"),
                seqplot.6 = translate("Measurement Index")))
    })

    ### Density Plot -----------------------------------------------------------

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter and semantic names.
    output$am_dist_plot <- shiny::renderPlot({
        bayesian_outputs <- bayesian_analysis()
        return(
            distribution.plot.am(
                gm         = exp(median(bayesian_outputs$mu.chain)),
                gsd        = exp(median(bayesian_outputs$sigma.chain)),
                am         = num_results()$am$est,
                c.oel      = user_formatted_sample()$c.oel,
                distplot.1 = translate("Concentration"),
                distplot.2 = translate("Density"),
                distplot.4 = translate("OEL outside of graphical limits."),
                distplot.5 = translate("OEL"),
                distplot.7 = translate("Arithmetic Mean")))
    })

    ### Risk Band Plot ---------------------------------------------------------

    # FIXME: (JMP) Rename arguments of this
    # function. Use shorter and semantic names.
    output$am_risk_band_plot <- shiny::renderPlot({
        bayesian_outputs <- bayesian_analysis()
        return(
            riskband.plot.am(
                mu.chain    = bayesian_outputs$mu.chain,
                sigma.chain = bayesian_outputs$sigma.chain,
                c.oel       = user_formatted_sample()$c.oel,
                psi         = user_inputs()$psi,
                riskplot.2  = translate("Probability"),
                riskplot.3  = translate("≤ 1% OEL"),
                riskplot.4  = translate("1% < OEL ≤ 10%"),
                riskplot.5  = translate("10% < OEL ≤ 50%"),
                riskplot.6  = translate("50% < OEL ≤ 100%"),
                riskplot.7  = translate("> OEL"),
                riskplot.9  = translate("Arithmetic Mean Category")))
    })
}


# Instantiation ----------------------------------------------------------------


app <- shiny::shinyApp(ui, server)
