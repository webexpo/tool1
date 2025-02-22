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
#' Global constants defined in `R/global.R` are capitalized.
#'
#' @section Google Analytics:
#' Google Analytics has been deactivated until further notice. We are required
#' by law (Quebec's Loi 25, effective since September 2023) to inform users we
#' collect usage data from them. We further require a formal Terms of Service,
#' and a Privacy Policy.
#'
#' Here how it used to be in `TAGS$head()` below.
#'
#' ```r
#' TAGS$script(src = file.path("www", "js", "ga-id.js"))
#' TAGS$script(src = file.path("www", "js", "ga.js"))
#' TAGS$noscript(src = file.path("www", "js", "ga-tm.js"))
#' ```
#'
#' Google Analytics changed its API recently. This may require further tweaks.
#'
#' @author Jérôme Lavoué (<jerome.lavoue@@umontreal.ca>)
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

# FIXME: (JMP) Many numeric values are passed to signif() which yields a
# variable number of (significant) digits. Is this truly needed? For UX
# purposes, I suggest always showing 1 to 3 digits? Create new global option
# signif_digits and set it equal to 3.

# FIXME: (JMP) The app heavily relies on functions defined in scripts. These
# must be revamped (those containing plot functions being the top priority).
# Theis contents should be split into a proper set of functions stored in
# multiple R files in R/. Each function should be documented and manually
# tested (at least).

# FIXME: (JMP) Plots are not generated with the same margins. They must be
# standardized. For some plots, this is a bigger problem and I left a TODO.

# FIXME: (JMP) All functions that generate plots should use semantic argument
# names. Using numbered arguments such as riskplot.1, riskplot.2 is a disaster
# for readability and long-term maintenance.

# FIXME: (JMP) OEL (Occupational Exposure Limit) and EL (Exposure Limit)
# are both used interchangeably. Choose one and stick to it. Below, I
# chose the latter until further notice. All other inputs have slightly
# different names (sometimes) in the app. More consistency is required.

# FIXME: (JMP) Use |> whenever appropriate below to reduce nesting.

# FIXME: (JMP) Check usage of add_bold_*_output() functions. They should be
# wrapped by as.character() in most cases to avoid undefined behavior.

# FIXME: (JMP) Convert uiOutput() / renderUI() pairs into
# textOutput() / renderText() whenever appropriate. The latter is slightly
# faster and reduce code.

ui <- shiny::fluidPage(
    # lang and title must be updated using custom Shiny messages
    # sent with session$sendCustomMessage(). See www/main.js for
    # more information.
    lang  = NULL,
    title = NULL,
    theme = bslib::bs_theme(version = 5L, preset = "flatly"),

    # Head ---------------------------------------------------------------------

    TAGS$head(
        TAGS$link(rel = "stylesheet", media = "all", href = "main.css"),
        TAGS$script(src = "main.js"),
        shinyjs::useShinyjs()
    ),

    # Body ---------------------------------------------------------------------

    shiny::uiOutput("top_title", container = TAGS$h1, class = "app-title"),

    # It is shown whenever the Shiny engine is blocked.
    shiny::conditionalPanel(
        condition = r"{$("html").hasClass("shiny-busy")}",
        shiny::uiOutput(
            outputId  = "top_banner",
            container = TAGS$p,
            class     = "app-banner-wait")
    ),

    shiny::sidebarLayout(

        ## Sidebar -------------------------------------------------------------

        shiny::sidebarPanel(width = 3L,

            ### Inputs ---------------------------------------------------------

            shiny::selectInput(
                inputId   = "lang",
                label     = "",
                selected  = DEFAULT_LANG,
                choices   = SUPPORTED_LANGS,
                selectize = FALSE,
                multiple  = FALSE) |>
                htmltools::tagAppendAttributes(class = "app-input") |>
                bslib::tooltip(id = "lang_tooltip", ""),

            TAGS$hr(class = "app-sidebar-hr"),

            shiny::numericInput(
                inputId = "oel",
                label   = "",
                value   = 100) |>
                htmltools::tagAppendAttributes(class = "app-input") |>
                bslib::tooltip(id = "oel_tooltip", ""),

            shiny::numericInput(
                inputId = "al",
                label   = "",
                value   = 1) |>
                htmltools::tagAppendAttributes(class = "app-input") |>
                bslib::tooltip(id = "al_tooltip", ""),

            shiny::numericInput(
                inputId = "conf",
                label   = "",
                value   = 90) |>
                htmltools::tagAppendAttributes(class = "app-input") |>
                bslib::tooltip(id = "conf_tooltip", ""),

            shiny::numericInput(
                inputId = "psi",
                label   = "",
                value   = 30) |>
                htmltools::tagAppendAttributes(class = "app-input") |>
                bslib::tooltip(id = "psi_tooltip", ""),

            shiny::textAreaInput(
                inputId = "data",
                label   = "",
                rows    = 10L,
                resize  = "vertical",
                value   = paste0(collapse = "\n", c(
                    "28.9",
                    "19.4",
                    "<5.5",
                    "149.9",
                    "26.42",
                    "56.1"))) |>
                htmltools::tagAppendAttributes(class = "app-input") |>
                bslib::tooltip(id = "data_tooltip", ""),

            # This output is only shown when the active panel is
            # exceedance. See observer in section Sidebar of server().
            shiny::numericInput(
                inputId = "frac_threshold",
                label   = "",
                value   = 5) |>
                htmltools::tagAppendAttributes(
                    class = "app-input",
                    style = "display: none;") |>
                bslib::tooltip(id = "frac_threshold_tooltip", ""),

            # This output is only shown when the active panel is
            # percentiles. See observer in section Sidebar of server().
            shiny::numericInput(
                inputId = "target_perc",
                label   = "",
                value   = 95) |>
                htmltools::tagAppendAttributes(
                    class = "app-input",
                    style = "display: none;") |>
                bslib::tooltip(id = "target_perc_tooltip", ""),

            TAGS$hr(class = "app-sidebar-hr"),

            ### Footer ---------------------------------------------------------

            # It is placed here to maximize visibility.
            TAGS$footer(
                class = "app-sidebar-footer",
                shiny::uiOutput("sb_footer_app_version", container = TAGS$p),

                shiny::uiOutput("sb_footer_copyright", container = TAGS$p)
            )
        ),

        ## Main ----------------------------------------------------------------

        shiny::mainPanel(width = 8L,

            ### Panels ---------------------------------------------------------

            shiny::tabsetPanel(
                id   = "active_panel",
                type = "pills",

                #### Panel: Statistics -----------------------------------------

                shiny::tabPanel(
                    value = "st",
                    title = shiny::uiOutput("st_tab_name", TRUE),

                    ##### Descriptive Statistics -------------------------------

                    shiny::uiOutput("st_desc_stats_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            shiny::uiOutput("st_desc_stats_subtitle",
                                container = TAGS$h3,
                                class     = "app-panel-subtitle"),

                            shiny::tableOutput("st_desc_stats_tbl"),
                        ),

                        shiny::column(width = 6L,
                            shiny::uiOutput("st_desc_stats_alert_info")
                        )
                    ),

                    ##### QQ Plot ----------------------------------------------

                    shiny::uiOutput("st_qq_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    # This plot is ugly if it is rendered using the
                    # full available width of its parent container.
                    # Width is restricted to 50%, and margin are set
                    # by the browser to center the underlying image.
                    # plotOutput() rerturns a <div> tag by default.
                    # FIXME: (JMP) Standardize bottom margin and remove styling.
                    shiny::plotOutput("st_qq_plot",
                        width  = "50%",
                        height = PLOT_DEFAULT_HEIGHT) |>
                        htmltools::tagAppendAttributes(
                            style = "margin: auto; margin-bottom: 10.5px;"),

                    shiny::uiOutput("st_qq_desc", container = TAGS$p),

                    ##### Box and Whiskers Plot --------------------------------

                    shiny::uiOutput("st_box_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::plotOutput("st_box_plot", height = PLOT_DEFAULT_HEIGHT),

                    shiny::uiOutput("st_box_desc", container = TAGS$p)
                ),

                #### Panel: Exceedance Fraction --------------------------------

                shiny::tabPanel(
                    value = "ef",
                    title = shiny::uiOutput("ef_tab_name", TRUE),

                    ##### Risk Decision ----------------------------------------

                    shiny::uiOutput("ef_risk_decision_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            shiny::uiOutput("ef_risk_decision_subtitle",
                                container = TAGS$h3,
                                class     = "app-panel-subtitle"),

                            shiny::uiOutput("ef_risk_decision",
                                container = TAGS$ul,
                                class     = "app-ul"),

                            shiny::uiOutput("ef_risk_meter_desc",
                                container = TAGS$p)
                        ),

                        shiny::column(width = 6L,
                            # Add a top margin to the plot <img> to match the
                            # margin of top <h3> above (see other column. This
                            # aligns both elements horizontally.
                            htmltools::tagAppendAttributes(
                                style = "margin-top: 21px",
                                shiny::plotOutput(
                                    outputId = "ef_risk_meter_plot",
                                    height   = PLOT_RISK_METER_DEFAULT_HEIGHT))
                        )
                    ),

                    ##### Parameter Estimates ----------------------------------

                    shiny::uiOutput("ef_estim_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::uiOutput("ef_estim", container = TAGS$p),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            shiny::uiOutput("ef_estim_dist_title",
                                container = TAGS$h3,
                                class     = "app-panel-subtitle"),

                            shiny::uiOutput("ef_estim_dist",
                                container = TAGS$ul,
                                class     = "app-ul")
                        ),

                        shiny::column(width = 6L,
                            shiny::uiOutput("ef_estim_ef_title",
                                    container = TAGS$h3,
                                    class     = "app-panel-subtitle"),

                            shiny::uiOutput("ef_estim_ef",
                                container = TAGS$ul,
                                class     = "app-ul")
                        )
                    ),

                    ##### Exceedance Plot --------------------------------------

                    shiny::uiOutput("ef_exceed_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::uiOutput("ef_exceed", container = TAGS$p),

                    shiny::radioButtons(
                        inputId  = "ef_exceed_btn_choose",
                        label    = "",
                        inline   = TRUE,
                        choices  = c(
                            # The format is
                            # What users see = Internal value of the input.
                            `1` = "plot1",
                            `2` = "plot2",
                            `3` = "plot3",
                            `4` = "plot4")) |>
                        htmltools::tagAppendAttributes(class = "app-input"),

                    shiny::actionButton(
                        inputId = "ef_exceed_btn_customize",
                        label   = "",
                        icon    = shiny::icon(
                            name  = "triangle-bottom",
                            lib   = "glyphicon",
                            style = "padding-right: 10px;"),
                        style = "margin-bottom: 15px;"),

                    # The <fieldset> is initially hidden, and is either shown,
                    # or hidden whenever the user clicks on the action button
                    # above. This is not an input (just a static container)
                    # that does not require an inputId. It has a standard HTML
                    # id attribute used by observers below.
                    TAGS$fieldset(
                        id    = "ef_exceed_cols",
                        style = "display: none;",
                        class = "form-group shiny-input-container-inline app-input",
                        shiny::uiOutput(
                            outputId  = "ef_exceed_cols_label",
                            container = TAGS$label,
                            class     = "control-label",
                            `for`     = "ef_exceed_cols"),
                        TAGS$div(
                            class = "app-flex-row",
                            colourpicker::colourInput(
                                inputId    = "ef_exceed_col_risk",
                                label      = "",
                                value      = "red",
                                returnName = TRUE,
                                palette    = "limited") |>
                            htmltools::tagAppendAttributes(class = "app-input"),
                            colourpicker::colourInput(
                                inputId    = "ef_exceed_col_no_risk",
                                label      = "",
                                value      = "gray50",
                                returnName = TRUE,
                                palette    = "limited") |>
                            htmltools::tagAppendAttributes(class = "app-input"),
                            colourpicker::colourInput(
                                inputId    = "ef_exceed_col_bg",
                                label      = "",
                                value      = "gray70",
                                returnName = TRUE,
                                palette    = "limited") |>
                            htmltools::tagAppendAttributes(class = "app-input"),
                            colourpicker::colourInput(
                                inputId    = "ef_exceed_col_bg_threshold",
                                label      = "",
                                value      = "gray40",
                                returnName = TRUE,
                                palette    = "limited") |>
                            htmltools::tagAppendAttributes(class = "app-input")
                        )
                    ),

                    # Class is used to center variants that only shows 1 plot.
                    htmltools::tagAppendAttributes(
                        class = "app-center-plot",
                        shiny::plotOutput(
                            outputId = "ef_exceed_plot",
                            height   = PLOT_DEFAULT_HEIGHT)) |>
                        # This color is extracted from
                        # the chosen shiny theme (flatly).
                        shinycssloaders::withSpinner(
                            type  = 8L,
                            color = "#212529"),

                    shiny::uiOutput("ef_exceed_desc_sub_plot", container = TAGS$p),

                    ##### Sequential Plot --------------------------------------

                    shiny::uiOutput("ef_seq_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::plotOutput("ef_seq_plot", height = PLOT_DEFAULT_HEIGHT),

                    shiny::uiOutput("ef_seq_desc", container = TAGS$p),

                    ##### Density Plot -----------------------------------------

                    shiny::uiOutput("ef_dist_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::plotOutput("ef_dist_plot", height = PLOT_DEFAULT_HEIGHT),

                    shiny::uiOutput("ef_dist_desc", container = TAGS$p),

                    ##### Risk Band Plot ---------------------------------------

                    shiny::uiOutput("ef_risk_band_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    # FIXME: (JMP) Standardize margins and remove styling.
                    htmltools::tagAppendAttributes(
                        style = "margin-bottom: 10.5px;",
                        shiny::plotOutput(
                            outputId = "ef_risk_band_plot",
                            height   = PLOT_DEFAULT_HEIGHT)),

                    shiny::uiOutput("ef_risk_band_desc", container = TAGS$p)
                ),

                ##### Panel: Percentiles ---------------------------------------

                shiny::tabPanel(
                    value = "pe",
                    title = shiny::uiOutput("pe_tab_name", TRUE),

                    ##### Risk Decision ----------------------------------------

                    shiny::uiOutput("pe_risk_decision_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            shiny::uiOutput("pe_risk_decision_subtitle",
                                container = TAGS$h3,
                                class     = "app-panel-subtitle"),

                            shiny::uiOutput("pe_risk_decision",
                                container = TAGS$ul,
                                class     = "app-ul"),

                            shiny::uiOutput("pe_risk_meter_desc",
                                container = TAGS$p)
                        ),

                        shiny::column(width = 6L,
                            # Add a top margin to the plot <img> to match the
                            # margin of top <h3> above (see other column. This
                            # aligns both elements horizontally.
                            htmltools::tagAppendAttributes(
                                style = "margin-top: 21px",
                                shiny::plotOutput(
                                    outputId = "pe_risk_meter_plot",
                                    height   = PLOT_RISK_METER_DEFAULT_HEIGHT))
                        )
                    ),

                    ##### Parameter Estimates ----------------------------------

                    shiny::uiOutput("pe_estim_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::uiOutput("pe_estim", container = TAGS$p),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            shiny::uiOutput("pe_estim_dist_title",
                                container = TAGS$h3,
                                class     = "app-panel-subtitle"),

                            shiny::uiOutput("pe_estim_dist",
                                container = TAGS$ul,
                                class     = "app-ul")
                        ),

                        shiny::column(width = 6L,
                            shiny::uiOutput("pe_estim_pe_title",
                                container = TAGS$h3,
                                class     = "app-panel-subtitle"),

                            shiny::uiOutput("pe_estim_pe",
                                container = TAGS$ul,
                                class     = "app-ul")
                        )
                    ),

                    ##### Sequential Plot --------------------------------------

                    shiny::uiOutput("pe_seq_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::plotOutput("pe_seq_plot", height = PLOT_DEFAULT_HEIGHT),

                    shiny::uiOutput("pe_seq_desc", container = TAGS$p),

                    ##### Density Plot -----------------------------------------

                    shiny::uiOutput("pe_dist_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::plotOutput("pe_dist_plot", height = PLOT_DEFAULT_HEIGHT),

                    shiny::uiOutput("pe_dist_desc", container = TAGS$p),

                    ##### Risk Band Plot ---------------------------------------

                    shiny::uiOutput("pe_risk_band_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    # FIXME: (JMP) Standardize margins and remove styling.
                    htmltools::tagAppendAttributes(
                        style = "margin-bottom: 10.5px;",
                        shiny::plotOutput(
                            outputId = "pe_risk_band_plot",
                            height   = PLOT_DEFAULT_HEIGHT)),

                    shiny::uiOutput("pe_risk_band_desc", container = TAGS$p)
                ),

                #### Panel: Arithmetic Mean ------------------------------------

                shiny::tabPanel(
                    value = "am",
                    title = shiny::uiOutput("am_tab_name", TRUE),

                    ##### Risk Decision ----------------------------------------

                    shiny::uiOutput("am_risk_decision_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            shiny::uiOutput("am_risk_decision_subtitle",
                                container = TAGS$h3,
                                class     = "app-panel-subtitle"),

                            shiny::uiOutput("am_risk_decision",
                                container = TAGS$ul,
                                class     = "app-ul"),

                            shiny::uiOutput("am_risk_meter_desc",
                                container = TAGS$p),

                            shiny::uiOutput("am_risk_decision_alert_warn")
                        ),

                        shiny::column(width = 6L,
                            # Add a top margin to the plot <img> to match the
                            # margin of top <h3> above (see other column. This
                            # aligns both elements horizontally.
                            htmltools::tagAppendAttributes(
                                style = "margin-top: 21px",
                                shiny::plotOutput(
                                    outputId = "am_risk_meter_plot",
                                    height   = PLOT_RISK_METER_DEFAULT_HEIGHT))
                        )
                    ),

                    ##### Parameter Estimates ----------------------------------

                    shiny::uiOutput("am_estim_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::uiOutput("am_estim", container = TAGS$p),

                    shiny::fluidRow(
                        shiny::column(width = 6L,
                            shiny::uiOutput("am_estim_dist_title",
                                container = TAGS$h3,
                                class     = "app-panel-subtitle"),

                            shiny::uiOutput("am_estim_dist",
                                container = TAGS$ul,
                                class     = "app-ul")
                        ),

                        shiny::column(width = 6L,
                            shiny::uiOutput("am_estim_am_title",
                                container = TAGS$h3,
                                class     = "app-panel-subtitle"),

                            shiny::uiOutput("am_estim_am",
                                container = TAGS$ul,
                                class     = "app-ul")
                        )
                    ),

                    ##### Sequential Plot --------------------------------------

                    shiny::uiOutput("am_seq_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::plotOutput("am_seq_plot", height = PLOT_DEFAULT_HEIGHT),

                    shiny::uiOutput("am_seq_desc", container = TAGS$p),

                    ##### Density Plot -----------------------------------------

                    shiny::uiOutput("am_dist_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::plotOutput("am_dist_plot", height = PLOT_DEFAULT_HEIGHT),

                    shiny::uiOutput("am_dist_desc", container = TAGS$p),

                    ##### Risk Band Plot ---------------------------------------

                    shiny::uiOutput("am_risk_band_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    # FIXME: (JMP) Standardize margins and remove styling.
                    htmltools::tagAppendAttributes(
                        style = "margin-bottom: 10.5px;",
                        shiny::plotOutput(
                            outputId = "am_risk_band_plot",
                            height   = PLOT_DEFAULT_HEIGHT)),

                    shiny::uiOutput("am_risk_band_desc", container = TAGS$p)
                ),

                #### Panel: About ----------------------------------------------

                shiny::tabPanel(
                    value = "ab",
                    title = shiny::uiOutput("ab_tab_name", TRUE),

                    ##### About ------------------------------------------------

                    shiny::uiOutput("ab_about_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::uiOutput("ab_about", container = TAGS$p),

                    ##### How To Use This Application --------------------------

                    shiny::uiOutput("ab_how_to_use_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::uiOutput("ab_how_to_use"),

                    ##### Methodological Background ----------------------------

                    shiny::uiOutput("ab_metho_bg_title",
                        container = TAGS$h2,
                        class     = "app-panel-title"),

                    shiny::uiOutput("ab_metho_bg")
                )
            )
        )
    )
)

server <- function(input, output, session) {

    # Internationalization -----------------------------------------------------

    # Define a wrapper function that avoids having to explicitly pass
    # tr and input$lang to each transltr::Translator$translate() call.
    # The parent environment of translate must be the environment of
    # server() for lexical scoping purposes.
    translate <- \(...) TR$translate(..., lang = input$lang)

    # Update input$lang based on (optional) URL's search parameter ?lang.
    shiny::observeEvent(session$clientData$url_search, {
        lang <- shiny::parseQueryString(session$clientData$url_search)$lang

        if (!is.null(lang) && match(lang, SUPPORTED_LANGS, 0L)) {
            shiny::updateSelectInput(inputId = "lang", selected = lang)
        }
    })

    shiny::observeEvent(input$lang, {
        lang  <- input$lang
        title <- sprintf("Expostats: %s", translate("Tool 1"))

        # lang is percent-encoded to ensure it is future-proof.
        shiny::updateQueryString(sprintf("?lang=%s", htmltools::urlEncodePath(lang)))

        # See www/main.js for more information.
        session$sendCustomMessage("update_page_lang", lang)
        session$sendCustomMessage("update_window_title", title)

        ## Body ----------------------------------------------------------------

        output$top_title <- shiny::renderUI(translate("
            Tool 1: Data Interpretation for One Similarly Exposed Group
        "))
        output$top_banner <- shiny::renderUI(
            translate("Calculating... Please wait.")
        )

        ## Sidebar -------------------------------------------------------------

        shiny::updateSelectInput(
            inputId = "lang",
            label   = translate("Language:"))
        shiny::updateNumericInput(
            inputId = "oel",
            label   = translate("Exposure Limit:"))
        shiny::updateNumericInput(
            inputId = "al",
            label   = translate("Exposure Limit Multiplier:"))
        shiny::updateNumericInput(
            inputId = "conf",
            label   = translate("Credible Interval Probability:"))
        shiny::updateNumericInput(
            inputId = "psi",
            label   = translate("Overexposure Risk Threshold:"))
        shiny::updateTextAreaInput(
            inputId = "data",
            label   = translate("Measurements:"))
        shiny::updateNumericInput(
            inputId = "frac_threshold",
            label   = translate("Exceedance Fraction Threshold:"))
        shiny::updateNumericInput(
            inputId = "target_perc",
            label   = translate("Critical Percentile:"))

        bslib::update_tooltip("lang_tooltip", translate("
            Choose your preferred language.
        "))
        bslib::update_tooltip("oel_tooltip", translate("
            Use the exposure limit to assess overexposure. It must have the
            same unit as the measurement data.
        "))
        bslib::update_tooltip("al_tooltip", translate("
            Use this multiplier to modify the exposure limit. The product of
            the former and the latter is the actual exposure limit value for
            calculation purposes.
        "))
        bslib::update_tooltip("conf_tooltip", translate("
            Use this value as a probability for the credible intervals around
            parameter estimates. It must be between 0% and 100%. The default
            value is set equal to 90%. The credible interval is the Bayesian
            equivalent of the confidence interval.
        "))
        bslib::update_tooltip("psi_tooltip", translate("
            Use this value as the maximal overexposure risk. It must be
            between 0% and 100%. It represents the maximal probability that
            the overexposure limit is met. Above this value, the situation
            should trigger remedial action. INRS and BOHS suggest using 5%
            and 30%, respectively.
        "))
        bslib::update_tooltip("data_tooltip", translate("
            The measurement dataset. There must be one value per line. Values
            can be censored to the left (<), to the right (>), or interval
            censored ([X-Y]).
        "))
        bslib::update_tooltip("frac_threshold_tooltip", translate("
            Use this value as an acceptable proportion of exposures above
            the exposure limit (OEL). It must be between 0% and 100%. The
            traditional default value is 5%.
        "))
        bslib::update_tooltip("target_perc_tooltip", translate("
            Use this value to set the percentile of the exposure distribution
            that will be compared to the OEL. It must be between 0% and 100%.
            The traditional default value is 95%.
        "))
        output$sb_footer_app_version <- shiny::renderUI(
            shiny::tagList(
                translate("Tool 1"), "version", VERSION,
                TAGS$a("GitHub", href = URLS$code, target = "_blank") |>
                    as.character() |>
                    sprintf_html(fmt = "(%s)."))
        )
        output$sb_footer_copyright <- shiny::renderUI(
            shiny::tagList(
                shiny::icon("copyright-mark", lib = "glyphicon"),
                TAGS$a("Jérôme Lavoué", href = URLS$jerome_lavoue, target = "_blank"),
                YEAR,
                translate("All rights reserved."))
        )

        ## Panel: Statistics ---------------------------------------------------

        output$st_tab_name <- shiny::renderUI(translate("Statistics"))
        output$st_desc_stats_title <- shiny::renderUI(
            translate("Descriptive Statistics")
        )
        output$st_desc_stats_subtitle <- shiny::renderUI(translate("Summary"))
        output$st_desc_stats_alert_info <- shiny::renderUI(add_bs_alert_info(
            title = translate("Information"),
            TAGS$p(translate("
                Censored measurements are subject to one of the following
                procedure.")),
            TAGS$ul(
                TAGS$li(translate("
                    Interval censored measurements are imputed as the
                    mid-range.
                ")),
                TAGS$li(translate("
                    Measurements censored to the right are imputed as 9/4 of
                    the censoring point.
                ")),
                TAGS$li(sprintf_html(
                    translate("
                        Measurements censored to the left are treated using
                        robust Log-probit regression on order statistics. The
                        algorithm used is derived from %s (itself derived from
                        previous work of %s).
                    "),
                    TAGS$a("NDExpo", href = URLS$jerome_lavoue, target = "_blank"),
                    TAGS$a("Dennis Helsel", href = URLS$dennis_helsel, target = "_blank")
                ))
            )
        ))
        output$st_qq_title <- shiny::renderUI(translate("Quantile-Quantile Plot"))
        output$st_qq_desc  <- shiny::renderUI(translate("
            The points above should follow a straight line. Random deviations
            from it are expected. However, significant deviations suggest that
            the data may have to be split into distinct subsets, or that some
            outliers must be investigated.
        "))
        output$st_box_title <- shiny::renderUI(translate("Box and Whiskers Plot"))
        output$st_box_desc  <- shiny::renderUI(sprintf_html(
            translate("
                The measurements are scattered around the x-axis middle point.
                The box (outer horizontal lines) represents the distance between
                the 25%1$s and 75%1$s percentiles. The whiskers (vertical lines)
                represent the distance between the 10%1$s and 90%1$s percentiles.
                The inner black horizontal line is the median.
            "),
            as.character(TAGS$sup(translate("th")))
        ))

        ## Panel: Exceedance Fraction ------------------------------------------

        shiny::updateRadioButtons(
            inputId = "ef_exceed_btn_choose",
            label   = translate("Variants:"))
        shiny::updateActionButton(
            inputId = "ef_exceed_btn_customize",
            label   = translate("Customize Colors"))
        colourpicker::updateColourInput(
            session = session,
            inputId = "ef_exceed_col_risk",
            label   = translate("Flask Color (Exceedance):"))
        colourpicker::updateColourInput(
            session = session,
            inputId = "ef_exceed_col_no_risk",
            label   = translate("Flask Color (No Exceedance):"))
        colourpicker::updateColourInput(
            session = session,
            inputId = "ef_exceed_col_bg",
            label   = translate("Background Color (Default):"))
        colourpicker::updateColourInput(
            session = session,
            inputId = "ef_exceed_col_bg_threshold",
            label   = translate("Background Color (Threshold):"))

        output$ef_tab_name <- shiny::renderUI(translate("Exceedance Fraction"))
        output$ef_risk_decision_title <- shiny::renderUI(
            translate("Risk Analysis Based on the Exceedance Fraction")
        )
        output$ef_risk_decision_subtitle <- shiny::renderUI(
            translate("Risk Decision")
        )
        output$ef_risk_decision <- shiny::renderUI(shiny::tagList(
            TAGS$li(
                translate("
                    Overexposure is defined as the exceedance fraction being
                    greater than or equal to %s.
                "),
            ),
            TAGS$li(sprintf_html(
                translate("
                    The probability that this criterion is met is equal to %s.
                "),
            )),
            TAGS$li(sprintf_html(
                translate("
                    The probability that this criterion is met should be lower
                    than %s.
                "),
            )),
            TAGS$li(sprintf_html(
                translate("
                    Consequently, the current situation is declared to be %s.
                "),
                as.character(add_bold_text_output("ef_risk_decision_conclusion"))))
        ))
        output$ef_risk_meter_desc <- shiny::renderUI(translate("
            This risk meter shows the probability of the exposure being too
            high when compared to the occupational exposure limit. The red
            zone indicates a poorly controlled exposure.
        "))
        output$ef_estim_title <- shiny::renderUI(
            translate("Parameters Estimates")
        )
        output$ef_estim <- shiny::renderUI(
            translate("Square brackets give the underlying credible intervals.")
        )
        output$ef_estim_dist_title <- shiny::renderUI(
            translate("Distribution Parameters")
        )
        output$ef_estim_dist <- shiny::renderUI(shiny::tagList(
            TAGS$li(sprintf_html(
                translate("The geometric mean point estimate is equal to %s."),
                add_bold_text_output("ef_estim_dist_geo_mean")
            )),

            TAGS$li(sprintf_html(
                translate("The geometric standard deviation point estimate is equal to %s."),
                add_bold_text_output("ef_estim_dist_geo_sd")
            ))
        ))
        output$ef_estim_ef_title <- shiny::renderUI(
            translate("Exceedance Fraction")
        )
        output$ef_estim_ef <- shiny::renderUI(TAGS$li(sprintf_html(
            translate("The point estimate is equal to %s."),
            as.character(add_bold_text_output("ef_estim_ef_frac")))
        ))
        output$ef_exceed_title <- shiny::renderUI(translate("Exceedance Plot"))
        output$ef_exceed <- shiny::renderUI(translate("
            The following plot illustrates the proportion of exposures that
            would be above the OEL in a fictional sample of one hundred
            measurements. Each flask represents an exposure. Red flasks
            correspond to exposures that are above the exposure limit. The plot
            can be shown in one of four variations. You may choose any variant
            (an alternative way of displaying the same information) below and,
            optionally, customize colors.
        "))
        output$ef_exceed_cols_label <- shiny::renderUI(translate("Colors:"))
        output$ef_exceed_desc_sub_plot <- shiny::renderUI(
            switch(input$ef_exceed_btn_choose,
                plot1 = translate("
                    The plot on the left shows an acceptable situation for the
                    chosen exceedance threshold (traditionally 5% above the OEL).
                    The plot on the right shows the situation estimated by the
                    Bayesian model. It does not take into account estimation
                    uncertainty.
                "),
                plot2 = translate("
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
                    credible interval).
                "),
                plot3 = translate("
                    This plot shows a shaded and darker region corresponding to
                    the maximal acceptable exceedance. Red symbols outside of it
                    are unacceptable exposures. It does not take into account
                    estimation uncertainty.
                "),
                plot4 = translate("
                    This plot shows a shaded and darker region corresponding to
                    the maximal acceptable exceedance. Red symbols outside of it
                    are unacceptable exposures. It takes into account estimation
                    uncertainty with stripped symbols. The number of plain
                    symbols represents the best estimate of the number of
                    measurements above the OEL. The total number of symbols
                    (either plain or stripped) represents the maximum plausible
                    number of measurements above the OEL given estimation
                    uncertainty (using the upper limit of the underlying
                    credible interval).
                ")
            )
        )
        output$ef_seq_title <- shiny::renderUI(translate("Sequential Plot"))
        output$ef_seq_desc  <- shiny::renderUI(translate("
            This plot shows the estimated exposure distribution when assuming
            250 exposure measurements have been collected. If the measurements
            represent 8-hour TWA (Time-Weighted Average) values, this
            approximately represents a full year of exposure. The OEL is shown
            as a red line.
        "))
        output$ef_dist_title <- shiny::renderUI(translate("Density Plot"))
        output$ef_dist_desc  <- shiny::renderUI(translate("
            This plot shows the probability density function of the estimated
            distribution of exposures. The OEL is shown as a red line. The
            exceedance fraction is the area under the curve beyond the OEL
            value.
        "))
        output$ef_risk_band_title <- shiny::renderUI(translate("Risk Band Plot"))
        output$ef_risk_band_desc  <- shiny::renderUI(sprintf_html(
            translate("
                This plot shows the probability distribution of the uncertainty
                around the exceedance fraction. It shows the probability that
                its true value is
                (1) below %s,
                (2) between %s and %s, and
                (3) greater than %s.
                The red column represents the probability of an overexposure.
                The latter should be lower than the threshold shown by the
                black dashed line.
            "),
            shiny::textOutput("ef_risk_band_desc_low_val_1",  inline = TRUE),
            shiny::textOutput("ef_risk_band_desc_low_val_2",  inline = TRUE),
            shiny::textOutput("ef_risk_band_desc_high_val_1", inline = TRUE),
            shiny::textOutput("ef_risk_band_desc_high_val_2", inline = TRUE)
        ))

        ## Panel: Percentiles --------------------------------------------------

        output$pe_tab_name <- shiny::renderUI(translate("Percentiles"))
        output$pe_risk_decision_title <- shiny::renderUI(
            translate("Risk Analysis Based on Percentiles")
        )
        output$pe_risk_decision_subtitle <- shiny::renderUI(
            translate("Risk Decision")
        )
        output$pe_risk_decision <- shiny::renderUI(shiny::tagList(
            TAGS$li(sprintf_html(
                translate("
                    Overexposure is defined as the %s percentile
                    being greater than or equal to the OEL.
                "),
            )),
            TAGS$li(sprintf_html(
                translate("
                    The probability that this criterion is met is equal to %s.
                "),
            )),
            TAGS$li(sprintf_html(
                translate("
                    The probability that this criterion is met should be lower
                    than %s.
                "),
            )),
            TAGS$li(sprintf_html(
                translate("
                    Consequently, the current situation is declared to be %s.
                "),
                add_bold_text_output("pe_risk_decision_conclusion")))
        ))
        output$pe_risk_meter_desc <- shiny::renderUI(translate("
            This risk meter shows the probability of the exposure being too
            high when compared to the occupational exposure limit. The red
            zone indicates a poorly controlled exposure.
        "))
        output$pe_estim_title <- shiny::renderUI(
            translate("Parameters Estimates")
        )
        output$pe_estim <- shiny::renderUI(
            translate("Square brackets give the underlying credible intervals.")
        )
        output$pe_estim_dist_title <- shiny::renderUI(
            translate("Distribution Parameters")
        )
        output$pe_estim_dist <- shiny::renderUI(shiny::tagList(
            TAGS$li(sprintf_html(
                translate("The geometric mean point estimate is equal to %s."),
            )),

            TAGS$li(sprintf_html(
                translate("The geometric standard deviation point estimate is equal to %s."),
                add_bold_text_output("pe_estim_dist_geo_sd")
            ))
        ))
        # FIXME: (JMP) Decimals are not shown properly.
        output$pe_estim_pe_title <- shiny::renderUI({
            sprintf_html(
            "%.0f%s %s",
            input$target_perc,
            as.character(TAGS$sup(ordinal_number_suffix(input$target_perc))),
            translate("Percentile Estimate"))
        })
        output$pe_estim_pe <- shiny::renderUI(TAGS$li(sprintf_html(
            translate("The point estimate is equal to %s."),
            as.character(add_bold_text_output("pe_estim_pe_perc"))
        )))
        output$pe_seq_title <- shiny::renderUI(translate("Sequential Plot"))
        output$pe_seq_desc  <- shiny::renderUI(translate("
            This plot shows the estimated exposure distribution when assuming
            250 exposure measurements have been collected. If the measurements
            represent 8-hour TWA (Time-Weighted Average) values, this
            approximately represents a full year of exposure. The OEL is shown
            as a red dotted line and the point estimate of the selected
            percentile as a continuous blue line.
        "))
        output$pe_dist_title <- shiny::renderUI(translate("Density Plot"))
        output$pe_dist_desc  <- shiny::renderUI(translate("
            This plot shows the probability density function of the estimated
            distribution of exposures. The OEL is shown as a red dotted line
            and the point estimate of the selected percentile as a continuous
            blue line.
        "))
        output$pe_risk_band_title <- shiny::renderUI(translate("Risk Band Plot"))
        output$pe_risk_band_desc  <- shiny::renderUI(sprintf_html(
            translate("
                This plot shows the probability distribution of the uncertainty
                around the selected percentile. It shows the probability that
                its true value is
                (1) below 1%% of the OEL,
                (2) between 1%% and 10%% of the OEL,
                (3) between 10%% and 50%% of the OEL,
                (4) between 50%% and 100%% of the OEL, and
                (5) greater than the OEL.
                This is based on the classification adopted by %s. The red
                column represents the probability of an overexposure. The
                latter should be lower than the threshold shown by the black
                dashed line."
            ),
            TAGS$a("AIHA", href = URLS$aiha, target = "_blank")
        ))

        ## Panel: Arithmetic Mean ----------------------------------------------

        output$am_tab_name <- shiny::renderUI(translate("Arithmetic Mean"))
        output$am_risk_decision_title <- shiny::renderUI(
            translate("Risk Analysis Based on the Arithmetic Mean")
        )
        output$am_risk_decision_subtitle <- shiny::renderUI(
            translate("Risk Decision")
        )
        output$am_risk_decision <- shiny::renderUI(shiny::tagList(
            TAGS$li(translate("
                Overexposure is defined as the arithmetic
                mean being greater than or equal to the OEL.")),
            TAGS$li(sprintf_html(
                translate("
                    The probability that this criterion is met is equal to %s.
                "),
            TAGS$li(sprintf_html(
                translate("
                    The probability that this criterion is met should be lower
                    than %s.
                "),
            TAGS$li(sprintf_html(
                translate("
                    Consequently, the current situation is declared to be %s.
                "),
                add_bold_text_output("am_risk_decision_conclusion")))
        ))
        output$am_risk_meter_desc <- shiny::renderUI(translate("
            This risk meter shows the probability of the exposure being too
            high when compared to the occupational exposure limit. The red
            zone indicates a poorly controlled exposure.")
        )
        output$am_risk_decision_alert_warn <- shiny::renderUI(
            translate("
                The risk assessment based on AM relies on the availability of
                a long-term averaged OEL (LTA-OEL in the AIHA terminology),
                representing a cumulative burden threshold. Most current OELs
                are not created as LTA-OEL. Despite an annoying lack of precise
                definition by most organizations, they should be most often
                viewed as thresholds to be exceeded as few times as possible.
                Some authors have suggested using one-tenth of the OEL as a
                practical LTA-OEL when assessing risk using the arithmetic mean.
            ") |>
            TAGS$p() |>
            add_bs_alert_warn(title = translate("Warning"))
        )
        output$am_estim_title <- shiny::renderUI(
            translate("Parameters Estimates")
        )
        output$am_estim <- shiny::renderUI(
            translate("Square brackets give the underlying credible intervals.")
        )
        output$am_estim_dist_title <- shiny::renderUI(
            translate("Distribution Parameters")
        )
        output$am_estim_dist <- shiny::renderUI(shiny::tagList(
            TAGS$li(sprintf_html(
                translate("The geometric mean point estimate is equal to %s."),
                add_bold_text_output("am_estim_dist_geo_mean")
            )),

            TAGS$li(sprintf_html(
                translate("The geometric standard deviation point estimate is equal to %s."),
                add_bold_text_output("am_estim_dist_geo_sd")
            ))
        ))
        output$am_estim_am_title <- shiny::renderUI(translate("Arithmetic Mean"))
        output$am_estim_am       <- shiny::renderUI(TAGS$li(sprintf_html(
            translate("The point estimate is equal to %s."),
            as.character(add_bold_text_output("am_estim_am_mean"))
        )))
        output$am_seq_title <- shiny::renderUI(translate("Sequential Plot"))
        output$am_seq_desc  <- shiny::renderUI(translate("
            This plot shows the estimated exposure distribution when assuming
            250 exposure measurements have been collected. If the measurements
            represent 8-hour TWA (Time-Weighted Average) values, this
            approximately represents a full year of exposure. The OEL is shown
            as a red dotted line and the point estimate of the arithmetic mean
            as a continuous green line.
        "))
        output$am_dist_title <- shiny::renderUI(translate("Density Plot"))
        output$am_dist_desc  <- shiny::renderUI(translate("
            This plot shows the probability density function of the estimated
            distribution of exposures. The OEL is shown as a red dotted line
            and the point estimate of the arithmetic mean as a continuous green
            line.
        "))
        output$am_risk_band_title <- shiny::renderUI(translate("Risk Band Plot"))
        output$am_risk_band_desc  <- shiny::renderUI(sprintf_html(
            translate("
                This plot shows the probability distribution of the uncertainty
                around the arithmetic mean. It shows the probability that its
                true value is
                (1) below 1%% of the OEL,
                (2) between 1%% and 10%% of the OEL,
                (3) between 10%% and 50%% of the OEL,
                (4) between 50%% and 100%% of the OEL, and
                (5) greater than the OEL.
                This is based on the classification adopted by %s. The red
                column represents the probability of an overexposure. The
                latter should be lower than the threshold (black dashed line)."
            ),
            TAGS$a("AIHA", href = URLS$aiha, target = "_blank")
        ))

        ## Panel: About --------------------------------------------------------

        output$ab_tab_name    <- shiny::renderUI(translate("About"))
        output$ab_about_title <- shiny::renderUI(translate("About"))
        output$ab_about       <- shiny::renderUI({
            a_epsum <- TAGS$a(translate("School of Public Health"),
                href   = URLS$epsum[[lang]],
                target = "_blank")
            a_udm <- TAGS$a(translate("Université de Montréal"),
                href   = URLS$udm[[lang]],
                target = "_blank")
            a_code <- TAGS$a(translate("GitHub"),
                href   = URLS$code,
                target = "_blank")

            sprintf_html(
                translate("
                    This application (and related tools) are developped by the
                    Industrial Hygiene team of the Department of Environmental
                    and Occupational Health at the %s of the %s. The source
                    code is available on %s.
                "),
                a_epsum,
                a_udm,
                a_code
            )
        })
        output$ab_how_to_use_title <- shiny::renderUI(
            translate("How to Use This Application")
        )
        output$ab_how_to_use <- shiny::renderUI(shiny::tagList(
            TAGS$p(translate("
                This application eases the interpretation of industrial hygiene
                measurements. Notably, it helps with checking compliance with
                respect to an occupational exposure limit (OEL). It is based on
                a risk assessment framework recognized by prominent institutions
                such as the American Industrial Hygiene Association, the British
                and Dutch Society for Occupational Health and Safety (BOHS/NVVA),
                the French Institut national de recherche et de sécurité (INRS),
                and the European Standards Organization.
            ")),
            TAGS$p(translate("
                It assumes that input measurements (measurements) represent a
                random sample stemming from the distribution of exposures that
                underlie the sampled context. In other words, the data is
                representative of the specific exposure regimen one wishes to
                assess.
            ")),
            TAGS$p(translate("
                The application is straightforward to use. Follow these three
                steps.
            ")),
            TAGS$ul(
                TAGS$li(translate("
                    Enter your measurements under Measurements in the left panel.
                    There must be one value per line. Write them as you would in
                    your favourite text editor. You may also copy and paste
                    values stored in a spreadsheet's column. The initial dataset
                    provided for illustration purposes can be deleted as you
                    usually would in any text editor.
                ")),
                TAGS$li(translate("
                    Enter other parameters (see the left sidebar to do do).
                ")),
                TAGS$li(translate("Wait for the calculations to be performed."))
            ),
            TAGS$p(translate("Results are updated whenever an input changes.")),
            TAGS$p(translate("
                Censored values are written as <X (left censored), >X (right
                censored), or [X1-X2] (interval censored), where X is the
                censored value. It must have the same unit as other non-censored
                measurements.
            "))
        ))
        output$ab_metho_bg_title <- shiny::renderUI(
            translate("Methodological Background")
        )
        output$ab_metho_bg <- shiny::renderUI(shiny::tagList(
            TAGS$p(translate("
                This application uses a Bayesian approach to estimate the
                parameters of the log-normal distribution.
            ")),
            TAGS$ul(
                TAGS$li(translate("
                    It yields a more intuitive rationale compared
                    to traditional (frequentist) methods.
                ")),
                TAGS$li(translate("
                    It naturally integrates the treatment of non-detects.
                ")),
                TAGS$li(translate("
                    It allows the inclusion of external information in the
                    measurements (not yet leveraged by the application).
                "))
            ),
            TAGS$p(sprintf_html(
                translate("
                    The Bayesian models and data interpretation procedures used
                    by this application are derived from current best practices
                    in industrial hygiene, which are described in the following
                    scientific paper: Jérôme Lavoué, Lawrence Joseph, Peter
                    Knott, Hugh Davies, France Labrèche, Frédéric Clerc, Gautier
                    Mater, Tracy Kirkham, Expostats: A Bayesian Toolkit to Aid
                    the Interpretation of Occupational Exposure Measurements,
                    Annals of Work Exposures and Health, Volume 63, Issue 3,
                    April 2019, Pages 267-279 (%s).
                "),
                TAGS$a(URLS$expostats_paper,
                    href   = URLS$expostats_paper,
                    target = "_blank")
            )),
            TAGS$p(sprintf_html(
                translate("Additional details and references are available on %s."),
                TAGS$a("expostats.ca",
                    href   = URLS$expostats[[lang]],
                    target = "_blank")
            ))
        ))
    })

    # Reactives and Observers --------------------------------------------------

    # This observer hides inputs frac_threshold and target_perc
    # by default, and respectively shows either of them only when
    # a specific panel is opened.
    shiny::observeEvent(input$active_panel, {
        shinyjs::hide("frac_threshold")
        shinyjs::hide("target_perc")
        switch(input$active_panel,
            ef = shinyjs::show("frac_threshold"),
            pe = shinyjs::show("target_perc"))
    })

    # Each click on ef_exceed_btn_customize triggers two actions:
    #   1. the icon of the button is updated, and
    #   2. ef_exceed_cols is either shown or hidden
    #      (based on the button's state).
    # The button state's starts at 0 (hidden). Odd numbers
    # correspond to a displayed container, and even numbers
    # to a hidden container.
    shiny::observeEvent(input$ef_exceed_btn_customize, {
        icon_name <- if (input$ef_exceed_btn_customize %% 2L == 0L) {
            "triangle-bottom"
        } else {
            "triangle-top"
        }

        shiny::updateActionButton(
            inputId = "ef_exceed_btn_customize",
            icon    = shiny::icon(icon_name,
                lib   = "glyphicon",
                style = "padding-right: 10px;"))
        shinyjs::toggle("ef_exceed_cols")
    })

    user_inputs <- shiny::reactive(
        list(
            conf           = input$conf,
            psi            = input$psi,
            frac_threshold = input$frac_threshold,
            target_perc    = input$target_perc)
    )

    user_formatted_sample <- shiny::reactive(
        data.formatting.SEG(
            data.in  = input$data,
            oel      = input$oel,
            oel.mult = input$al)
    )

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

    bayesian_analysis <- shiny::reactive({
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
                n.iter           = 25000L))
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

    # Outputs Shared By Panels -------------------------------------------------

    output$ef_risk_decision_limit <-
    output$pe_risk_decision_limit <-
    output$am_risk_decision_limit <- shiny::renderText({
        return(paste0(input$psi, "%"))
    })

    output$ef_estim_dist_geo_mean <-
    output$pe_estim_dist_geo_mean <-
    output$am_estim_dist_geo_mean <- shiny::renderText({
        gm <- lapply(num_results()$gm, \(x) as.character(signif(x, 2L)))
        return(sprintf("%s [%s - %s]", gm$est, gm$lcl, gm$ucl))
    })

    output$ef_estim_dist_geo_sd <-
    output$pe_estim_dist_geo_sd <-
    output$am_estim_dist_geo_sd <- shiny::renderText({
        gsd <- lapply(num_results()$gsd, \(x) as.character(signif(x, 2L)))
        return(sprintf("%s [%s - %s]", gsd$est, gsd$lcl, gsd$ucl))
    })

    # Panel: Statistics --------------------------------------------------------

    ## Descriptive Statistics --------------------------------------------------

    output$st_desc_stats_tbl <- shiny::renderTable(
        rownames = FALSE,
        spacing  = "m",
        hover    = TRUE,
        expr     = {
            stats_df <- fun.desc.stat(
                data.simply.imputed = user_formatted_sample_imputed(),
                c.oel = user_formatted_sample()$c.oel)

            # <sup> tag cannot be used in a renderTable() call,
            # even with shiny::HTML(). As a workaround, UTF-8
            # superscript characters ᵗ and ʰ are used. This is
            # not optimal, but it is better than nothing.
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

    ## QQ Plot -----------------------------------------------------------------

    output$st_qq_plot <- shiny::renderPlot(
        fun.qqplot(
            data.simply.imputed = user_formatted_sample_imputed(),
            notcensored         = user_formatted_sample()$notcensored,
            qqplot.1            = translate("Quantile-Quantile Plot"),
            qqplot.2            = translate("Quantiles (Lognormal Distribution)"),
            qqplot.3            = translate("Quantiles (Standardized Measurements)"),
            qqplot.4            = translate("Measurement Type"),
            qqplot.5            = translate("Censored"),
            qqplot.6            = translate("Detected"))
    )

    ## Box and Whiskers Plot ---------------------------------------------------

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

    # Panel: Exceedance Fraction -----------------------------------------------

    ## Risk Decision -----------------------------------------------------------

    output$ef_risk_decision_frac        <-
    output$ef_risk_band_desc_high_val_1 <-
    output$ef_risk_band_desc_high_val_2 <- shiny::renderText(
        paste0(signif(input$frac_threshold, 3L), "%")
    )

    output$ef_risk_decision_criterion <- shiny::renderText({
        paste0(signif(num_results()$frac.risk, 3L), "%")
    })

    output$ef_risk_decision_conclusion <- shiny::renderText({
        if (num_results()$frac.risk >= user_inputs()$psi) {
            return(translate("poorly controlled"))
        }

        return(translate("adequately controlled"))
    })

    output$ef_risk_meter_plot <- shiny::renderPlot(
        dessinerRisqueMetre(
            actualProb          = num_results()$frac.risk,
            minProbUnacceptable = user_inputs()$psi)
    )

    ## Parameter Estimates -----------------------------------------------------

    output$ef_estim_ef_frac <- shiny::renderText({
        frac <- lapply(num_results()$frac, \(x) as.character(signif(x, 3L)))
        return(sprintf("%s%% [%s - %s]", frac$est, frac$lcl, frac$ucl))
    })

    ## Exceedance Plot ---------------------------------------------------------

    output$ef_exceed_plot <- shiny::renderPlot({
        seuil    <- input$frac_threshold
        results  <- num_results()
        frac_est <- ceiling(results$frac$est)
        frac_ucl <- ceiling(results$frac$ucl)

        params_plots <- paramsVariantesFracDep(
            IMAGES_DIR_REL_PATH,
            file.path(IMAGES_DIR_REL_PATH, "flask.png"),
            file.path(IMAGES_DIR_REL_PATH, "flask-lines.png"),
            input$ef_exceed_col_risk,
            input$ef_exceed_col_no_risk,
            input$ef_exceed_col_bg_threshold,
            input$ef_exceed_col_bg)

        plots <- switch(input$ef_exceed_btn_choose,
            plot1 = list(
                drawPlot(
                    params_plots,
                    fracDepasseEst = seuil,
                    titre          = translate("Acceptable Sample")),
                drawPlot(
                    params_plots,
                    fracDepasseEst = frac_est,
                    titre          = translate("Current Sample"))
            ),
            plot2 = list(
                drawPlot(
                    params_plots,
                    fracDepasseEst = seuil,
                    titre          = translate("Acceptable Sample")),
                drawPlot(
                    params_plots,
                    fracDepasseEst = frac_est,
                    fracDepasseLim = frac_ucl,
                    titre          = translate("Current Sample"))
            ),
            plot3 = list(
                drawPlot(
                    params_plots,
                    fracDepasseEst = frac_est,
                    seuil          = seuil)
            ),
            plot4 = list(
                drawPlot(
                    params_plots,
                    fracDepasseEst = frac_est,
                    fracDepasseLim = frac_ucl,
                    seuil          = seuil)
            )
        )

        return(gridExtra::grid.arrange(grobs = plots, ncol = length(plots)))
    })

    ## Sequential Plot ---------------------------------------------------------

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

    ## Density Plot ------------------------------------------------------------

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

    ## Risk Band Plot ----------------------------------------------------------

    output$ef_risk_band_desc_low_val_1 <-
    output$ef_risk_band_desc_low_val_2 <- shiny::renderText(
        paste0(signif(input$frac_threshold / 10L, 3L), "%")
    )

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

    # Panel: Percentiles -------------------------------------------------------

    ## Risk Decision -----------------------------------------------------------

    # FIXME: (JMP) Decimals are not shown properly.
    output$pe_risk_decision_perc <- shiny::renderText({
        value <- input$target_perc
        return(
            sprintf_html(
                "%.0f<sup>%s</sup>",
                value,
                ordinal_number_suffix(value)))
    })

    output$pe_risk_decision_criterion <- shiny::renderText(
        paste0(signif(num_results()$perc.risk, 3L), "%")
    )

    output$pe_risk_decision_conclusion <-shiny::renderText({
        if (num_results()$perc.risk >= user_inputs()$psi) {
            return(translate("poorly controlled"))
        }

        return(translate("adequately controlled"))
    })

    output$pe_risk_meter_plot <- shiny::renderPlot(
        dessinerRisqueMetre(
            actualProb          = num_results()$perc.risk,
            minProbUnacceptable = user_inputs()$psi)
    )

    ## Parameter Estimates -----------------------------------------------------

    output$pe_estim_pe_perc <- shiny::renderText({
        perc <- lapply(num_results()$perc, \(x) as.character(signif(x, 3L)))
        return(sprintf("%s [%s - %s]", perc$est, perc$lcl, perc$ucl))
    })

    ## Sequential Plot ---------------------------------------------------------

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

    ## Density Plot ------------------------------------------------------------

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

    ## Risk Band Plot ----------------------------------------------------------

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

    # Panel: Arithmetic Mean ---------------------------------------------------

    ## Risk Decision -----------------------------------------------------------

    output$am_risk_decision_criterion <- shiny::renderText(
        paste0(signif(num_results()$am.risk, 3L), "%")
    )

    output$am_risk_decision_conclusion <-shiny::renderText({
        msgid <- if (num_results()$am.risk >= user_inputs()$psi) {
            return(translate("poorly controlled"))
        }

        return(translate("adequately controlled"))
    })

    output$am_risk_meter_plot <- renderPlot(
        dessinerRisqueMetre(
            actualProb          = num_results()$am.risk,
            minProbUnacceptable = user_inputs()$psi)
    )

    ## Parameter Estimates -----------------------------------------------------

    output$am_estim_am_mean <- shiny::renderText({
        am <- lapply(num_results()$am, \(x) as.character(signif(x, 3L)))
        return(sprintf("%s [%s - %s]", am$est, am$lcl, am$ucl))
    })

    ## Sequential Plot ---------------------------------------------------------

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

    ## Density Plot ------------------------------------------------------------

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

    ## Risk Band Plot ----------------------------------------------------------

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
