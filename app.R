#' Tool1: Data Interpretation for One Similarly Exposed Group
#'
#' Define the application's user interface (`ui`) and [server()] logic.
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
#' For historical reasons, the application further depends on a set of scripts
#' stored in `scripts/`. These must be explicitly loaded at runtime. See script
#' `R/global.R`. It is worthwhile noting they were **not** refactored.
#'
#' [server()] is currently organized according to the source `ui` object:
#' outputs are in same order as the inputs.
#'
#' @note
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


# User Interface ---------------------------------------------------------------


# FIXME: (JMP) I would strongly consider breaking the app into Shiny modules:
# one for each panel. See shiny::moduleServer() for more information. Modules
# can be tested with shiny::testServer().


ui <- shiny::fluidPage(
    # NOTE: (JMP) I just discovered arg lang. This is awesome and could
    # naturally be integrated with transltr in a near future. This note
    # serves as a reminder.
    lang  = NULL,
    theme = shinythemes::shinytheme("flatly"),


    ## Head --------------------------------------------------------------------


    html$head(
        # Using htmltools::singleton() is overkill here.
        html$link(rel = "stylesheet", media = "all", href = "/css/main.css"),
        html$link(rel = "stylesheet", media = "all", href = "/css/banner-wait.css"),
        html$script(src = "js/exceedance-plot.js"),
    ),

    shiny::titlePanel(gett("main.title.t1")),


    ## Body --------------------------------------------------------------------


    shiny::sidebarLayout(


        ### Sidebar ------------------------------------------------------------


        # NOTE: (JMP) I removed custom width = input_width value from all
        # sidebar's inputs. I believe we should let Shiny handle containers'
        # sizes. Setting absolute values can induce unintended consequences.
        shiny::sidebarPanel(
            width = 3,

            shiny::numericInput(
                inputId = "oel",
                label   = translate("Exposure Limit"),
                value   = 100),

            add_tooltip("oel", translate("
                Use the exposure limit to assess overexposure. It
                must have the same unit as the measurement data.")),

            shiny::numericInput(
                inputId = "al",
                label   = translate("Exposure Limit Multiplier"),
                value   = 1),

            add_tooltip("al", translate("
                Use this multiplier to modify the exposure limit. The product
                of the former and the latter is the actual exposure limit value
                for calculation purposes.")),

            shiny::numericInput(
                inputId = "conf",
                label   = translate("Credible Interval Probability"),
                value   = 90),

            add_tooltip("conf", translate("
                Use this value as a probability for the credible intervals
                around parameter estimates. It must be between 0% and 100%.
                The default value is set equal to 90%. The credible interval
                is the Bayesian equivalent of the confidence interval.")),

            shiny::numericInput(
                inputId = "psi",
                label   = translate("Overexposure Risk Threshold"),
                value   = 30),

            add_tooltip("psi", translate("
                Use this value as the maximal overexposure risk. It must be
                between 0% and 100%. It represents the maximal probability that
                the overexposure limit is met. Above this value, the situation
                should trigger remedial action. INRS and BOHS suggest using 5%
                and 30%, respectively.")),

            inputTextArea(
                inputId = "data",
                label   = translate("Measurement Data"),
                value   = "28.9\n19.4\n<5.5\n149.9\n26.42\n56.1"),

            add_tooltip("data", translate("
                The measurement dataset. There must be one value per line.
                Values can be censored to the left (<) or to the right (>)."))
        ),


        ### Main ---------------------------------------------------------------


        shiny::mainPanel(


            #### Top Banner for Calculations -----------------------------------


            # It is shown whenever the Shiny engine is blocked.
            # See file www/css/wait-banner.css for styling.
            shiny::conditionalPanel(
                html$div(gett("input.8")),
                id        = "banner-wait",
                condition = r"{$("html").hasClass("shiny-busy")}"
            ),


            #### Panels --------------------------------------------------------


            shiny::tabsetPanel(


                ##### Panel: Descriptive ---------------------------------------


                shiny::tabPanel(
                    title = gett("descriptive.tab.name"),
                    html$h3(gett("descriptive.title")),


                    ##### Descriptive statistics -------------------------------


                    shiny::tableOutput("res.desc"),
                    html$h4(gett("descriptive.1")),
                    shiny::htmlOutput("descriptive.2"),


                    ##### QQ Plot ----------------------------------------------


                    html$br(),
                    html$h3(gett("descriptive.3")),
                    shiny::plotOutput("qqplot"),
                    html$br(),
                    html$p(gettt("descriptive.4")),


                    ##### Box and Whiskers Plot --------------------------------


                    html$br(),
                    html$h3(gett("descriptive.5")),
                    shiny::plotOutput("boxplot"),
                    html$p(gett("descriptive.6")),
                    html$br()
                ),


                ##### Panel: Exceedance ----------------------------------------


                shiny::tabPanel(
                    title = gett("frac.tab.name"),
                    html$h3(gett("frac.title")),


                    ###### Input: Exceedance Threshold -------------------------


                    # NOTE: (JMP) I removed custom width = input_width value
                    # from all sidebar's inputs. I believe we should let Shiny
                    # handle containers' sizes. Setting absolute values can
                    # induce unintended consequences.
                    # TODO: (JMP) Move this input to the sidebar and hide it
                    # unless the underlying panel is active/visible.
                    shiny::numericInput(
                        inputId = "frac_threshold",
                        label   = translate("frac.1"),
                        value   = 5),

                    add_tooltip("frac_threshold", translate("frac.1.tooltip")),


                    ###### Risk Decision ---------------------------------------


                    html$br(),
                    html$div(
                        style = "block",
                        class = "plot-with-text",
                        html$div(
                            style = "display:inline-block;width:55%",
                            class = "text",
                            html$h4(html$strong(gett("frac.7"))),

                            html$p("\u25B9", gett("frac.8")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(gett("frac.9"),
                                "\u2265 ",
                                shiny::textOutput("acceptableExpo1", inline = TRUE))
                            ),
                            html$p("\u25B9", gett("frac.10")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(
                                    shiny::textOutput("probrisk", inline = TRUE))
                            ),
                            html$p("\u25B9", gett("frac.11")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(
                                    "<",
                                   shiny::textOutput("frac.probSituUnacceptable", inline = TRUE))
                            ),
                            html$p("\u25B9", gett("frac.12")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(shiny::textOutput("finalrisk", inline = TRUE))
                            )
                        ),

                        html$div(
                            style = "display: inline-block;width:44%;vertical-align:top",
                            class = "plot",
                            shiny::plotOutput("risquemetre"))
                    ),


                    ###### Parameter Estimates: Distribution -------------------


                    html$h4(html$strong(gett("frac.4"))),
                    html$p(
                        shiny::textOutput("gm1.dist", inline = TRUE),
                        html$strong(shiny::textOutput("gm1", inline = TRUE))
                    ),
                    html$p(
                        shiny::textOutput("gsd1.dist", inline = TRUE),
                        html$strong(shiny::textOutput("gsd1", inline = TRUE))
                    ),


                    ###### Parameter Estimates: Exceedance Fraction ------------


                    html$br(),
                    html$h4(html$strong(gett("frac.5"))),
                    html$p(
                        gett("frac.6"),
                        html$strong(shiny::textOutput("Frac", inline = TRUE))
                    ),

                    html$p(
                        gett("frac.6.1"),
                        html$strong(shiny::textOutput("Frac.ci", inline = TRUE))
                    ),


                    ###### Exceedance Plot -------------------------------------


                    html$br(),
                    html$h4(html$strong(gett("frac.graph.1"))),

                    html$p(html$strong(gett("frac.graph.13"))),
                    html$p(gett("frac.graph.13.1")),
                    html$span(
                        class = "inline-b ital va-mid",
                        gett("frac.graph.13.4.variante")),

                    html$div(
                        class = "inline-b",
                        shiny::radioButtons(
                            inputId = "varianteFracDep",
                            label   = NULL,
                            inline  = TRUE,
                            choices = c(
                                "1" = "figure1",
                                "2" = "figure2",
                                "3" = "figure3",
                                "4" = "figure4"))
                    ),

                    # See file www/js/exceedance-plot.js for further UI logic.
                    html$fieldset(
                        id = "exceedance-plot-customize",
                        html$legend(
                            id    = "exceedance-plot-customize-legend",
                            class = "toggle-personnalisation",
                            html$span(gett("frac.graph.13.2")),
                            html$span(class = "glyphicon glyphicon-plus")
                        ),
                        html$div(
                            class = "fieldset-body",
                            html$div(
                                class = "colour-inp inline-b",
                                html$div(html$label(gett("frac.graph.13.3.1"))),
                                colourpicker::colourInput(
                                    inputId    = "couleurRisque",
                                    label      = NULL,
                                    value      = "red",
                                    returnName = TRUE,
                                    palette    = "limited")
                            ),
                            html$div(
                                class = "colour-inp inline-b",
                                html$div(html$label(gett("frac.graph.13.3.2"))),
                                colourpicker::colourInput(
                                    inputId    = "couleurAucunRisque",
                                    label      = NULL,
                                    value      = "gray50",
                                    returnName = TRUE,
                                    palette    = "limited")
                            ),
                            html$div(
                                class = "colour-inp inline-b",
                                html$div(html$label(gett("frac.graph.13.3.3"))),
                                colourpicker::colourInput(
                                    inputId    = "couleurFond",
                                    label      = NULL,
                                    value      = "gray70",
                                    returnName = TRUE,
                                    palette    = "limited")
                            ),
                            html$div(
                                class = "colour-inp inline-b",
                                html$div(html$label(gett("frac.graph.13.3.4"))),
                                colourpicker::colourInput(
                                    inputId    = "couleurSeuil",
                                    label      = NULL,
                                    value      = "gray40",
                                    returnName = TRUE,
                                    palette    = "limited")
                            )
                        )
                    ),

                    html$br(),
                    html$div(
                        class = "fig-var-desc",
                        shiny::textOutput("fracDepVarianteDesc")
                    ),

                    shinycssloaders::withSpinner(
                        shiny::plotOutput("fracDepVariantes")),


                    ###### Sequential Plot -------------------------------------


                    html$p(html$strong(gett("frac.graph.4"))),
                    html$p(gett("frac.graph.5")),
                    shiny::plotOutput("seqplot.frac"),


                    ###### Density Plot ----------------------------------------


                    html$br(),
                    html$p(html$strong(gett("frac.graph.7"))),
                    html$p(gett("frac.graph.8")),
                    shiny::plotOutput("distplot.frac"),
                    html$br(),


                    ###### Risk Band Plot --------------------------------------


                    html$p(html$strong(gett("frac.graph.11"))),
                    html$p(
                        gett("frac.graph.12.1"),
                        shiny::textOutput("frac.acceptableExpoDiv1", inline = TRUE),
                        gett("frac.graph.12.2"),
                        shiny::textOutput("frac.acceptableExpoDiv2", inline = TRUE),
                        gett("frac.graph.12.3"),
                        shiny::textOutput("acceptableExpo2", inline = TRUE),
                        gett("frac.graph.12.4"),
                        shiny::textOutput("acceptableExpo3", inline = TRUE),
                        gett("frac.graph.12.5")
                    ),

                    shiny::plotOutput("riskband.frac"),
                    html$br()
                ),


                ##### Panel: 95th Percentile -----------------------------------


                shiny::tabPanel(
                    # NOTE: (JMP) The title depends on the output passed to
                    # input target_perc. I think we can simplify this to a
                    # static name such as 'Percentiles'. We clearly see that
                    # the percentile is an input.
                    title = shiny::textOutput("perc.percentile.title", inline = TRUE),
                    html$h3(
                        gett("perc.title"),
                        shiny::textOutput("perc.percentile.h3",inline = TRUE)
                    ),


                    ###### Input: Percentile -----------------------------------


                    # NOTE: (JMP) I removed custom width = input_width value
                    # from all sidebar's inputs. I believe we should let Shiny
                    # handle containers' sizes. Setting absolute values can
                    # induce unintended consequences.
                    # TODO: (JMP) Move this input to the sidebar and hide it
                    # unless the underlying panel is active/visible.
                    shiny::numericInput(
                        inputId = "target_perc",
                        label   = translate("perc.1"),
                        value   = 95),

                    add_tooltip("target_perc" , translate("perc.1.tooltip")),


                    ###### Risk Decision ---------------------------------------


                    html$br(),
                    html$div(
                        style = "block",
                        class = "plot-with-text",
                        html$div(
                            style = "display: inline-block;width:55%",
                            class = "text",
                            html$h4(html$strong(gett("frac.7"))),
                            html$p("\u25B9", gett("frac.8")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(shiny::textOutput("perc.percentile.risk.decision", inline = TRUE),
                                "\u2265 ",
                                gett("OEL"))
                            ),
                            html$p("\u25B9", gett("frac.10")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(shiny::textOutput("probrisk.perc", inline = TRUE))
                            ),
                            html$p("\u25B9", gett("frac.11")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(
                                    "<",
                                    shiny::textOutput("perc.probSituUnacceptable", inline = TRUE))
                            ),
                            html$p("\u25B9", gett("frac.12")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(shiny::textOutput("finalrisk.perc", inline = TRUE))
                            )
                        ),
                        html$div(
                            style = "display: inline-block;width:44%;vertical-align:top",
                            class = "plot",
                            shiny::plotOutput("risquemetre2")
                        )
                    ),

                    html$h4(html$strong(gett("frac.4"))),


                    ###### Parameter Estimates: Distribution -------------------


                    html$p(
                        shiny::textOutput("gm1.perc", inline = TRUE),
                        html$strong(shiny::textOutput("gm2", inline = TRUE))),
                    html$p(
                        shiny::htmlOutput("gsd1.perc", inline = TRUE),
                        html$strong(shiny::textOutput("gsd2", inline = TRUE))),
                    html$br(),


                    ###### Parameter Estimates: 95th percentile ----------------


                    html$h4(
                        html$strong(gett("perc.5"),
                        shiny::textOutput("perc.percentile.param.estimates", inline = TRUE))),
                    html$p(
                        gett("frac.6"),
                        html$strong(shiny::textOutput("Perc", inline = TRUE))),
                    html$p(
                        gett("frac.6.1"),
                        html$strong(shiny::textOutput("Perc.ci", inline = TRUE))),
                    html$br(),


                    ###### Sequential Plot -------------------------------------


                    html$h4(html$strong(gett("frac.graph.1"))),
                    html$p(html$strong(gett("frac.graph.4"))),
                    html$p(gett("perc.graph.5")),
                    shiny::plotOutput("seqplot.perc"),
                    html$br(),


                    ###### Density Plot ----------------------------------------


                    html$p(html$strong(gett("frac.graph.7"))),
                    html$p(gett("perc.graph.8")),
                    shiny::plotOutput("distplot.perc"),
                    html$br(),


                    ###### Risk Band Plot --------------------------------------


                    html$p(html$strong(gett("frac.graph.11"))),
                    html$p(
                        gett("perc.graph.12.1"),
                        shiny::textOutput("perc.percentile.risk.band", inline = TRUE),
                        shiny::htmlOutput("perc.graph.12.2", inline = TRUE)),
                    shiny::plotOutput("riskband.perc"),
                    html$br()
                ),


                ##### Panel: Arithmetic Mean -----------------------------------


                shiny::tabPanel(
                    title = gett("am.tab.name"),
                    html$h3(gett("am.title")),
                    html$br(),


                    ###### Quick Glance ----------------------------------------


                    html$h4(html$strong(gett("frac.2"))),
                    html$p(gett("frac.3")),


                    ###### Risk Decision ---------------------------------------


                    html$br(),
                    html$div(
                        style = "block",
                        class = "plot-with-text",
                        html$div(
                            style = "display: inline-block;width:55%",
                            class = "text",
                            html$h4(html$strong(gett("frac.7"))),
                            html$p("\u25B9", gett("frac.8")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(gett("AM"),
                                "\u2265 ",
                                gett("OEL"))
                            ),
                            html$p("\u25B9", gett("frac.10")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(
                                    shiny::textOutput("probrisk.AM", inline = TRUE)
                                )
                            ),
                            html$p("\u25B9", gett("frac.11")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(
                                    "<",
                                    shiny::textOutput("am.probSituUnacceptable", inline = TRUE)
                                )
                            ),
                            html$p("\u25B9", gett("frac.12")),
                            html$p(
                                "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
                                html$strong(shiny::textOutput("finalrisk.AM", inline = TRUE))
                            )
                        ),
                        html$div(
                            style = "display: inline-block;width:44%;vertical-align:top",
                            class = "plot",
                            shiny::plotOutput("risquemetre.am")
                        ),
                        html$p(
                            style = "font-size: 13px",
                            html$strong(gett("am.13")),
                            gett("am.13.1")
                        ),
                        html$br()
                    ),


                    ###### Parameter Estimates: Distribution -------------------


                    html$h4(html$strong(gett("frac.4"))),
                    html$p(
                        shiny::textOutput("gm1.AM", inline = TRUE),
                        html$strong(shiny::textOutput("gm3", inline = TRUE))
                    ),
                    html$p(
                        shiny::textOutput("gsd1.AM", inline = TRUE),
                        html$strong(shiny::textOutput("gsd3",inline = TRUE))
                    ),
                    html$br(),


                    ###### Parameter Estimates: Arithmetic Mean ----------------


                    html$h4(html$strong(gett("am.5"))),
                    html$p(
                        gett("frac.6"),
                        html$strong(shiny::textOutput("AM", inline = TRUE))
                    ),
                    html$p(
                        gett("frac.6.1"),
                        html$strong(shiny::textOutput("AM.ci", inline = TRUE))
                    ),
                    html$br(),
                    html$h4(html$strong(gett("frac.graph.1"))),


                    ###### Sequential Plot -------------------------------------


                    html$p(html$strong(gett("frac.graph.4"))),
                    html$p(gett("am.graph.5")),
                    shiny::plotOutput("seqplot.AM"),


                    ###### Density Plot ----------------------------------------


                    html$br(),
                    html$p(html$strong(gett("frac.graph.7"))),
                    html$p(gett("am.graph.8")),
                    shiny::plotOutput("distplot.AM"),
                    html$br(),


                    ###### Risk Band Plot --------------------------------------


                    html$p(html$strong(gett("frac.graph.11"))),
                    html$p(shiny::htmlOutput("am.graph.12")),
                    shiny::plotOutput("riskband.am"),
                    html$br()
                ),


                ##### Panel: Instructions --------------------------------------


                shiny::tabPanel(
                    title = gett("inst.tab.name"),
                    html$h3(gett("inst.title")),
                    html$br(),
                    html$p(gett("inst.1")),
                    html$p(gett("inst.2")),
                    html$p(gett("inst.3")),
                    html$p(gett("inst.5"))
                ),


                ##### Panel: About ---------------------------------------------


                shiny::tabPanel(
                    title = gett("about.tab.name"),
                    html$h3(gett("about.title")),
                    html$br(),
                    html$p(shiny::htmlOutput("about.1")),
                    html$p(gett("about.2"))
                ),


                ##### Panel: Methodological Background -------------------------


                shiny::tabPanel(
                    title = gett("back.tab.name"),
                    html$h3(gett("back.title")),
                    html$br(),
                    html$p(shiny::htmlOutput("back.1.intro")),
                    html$p(shiny::htmlOutput("back.2", inline = TRUE))
                )
            )
        )
    )
)


# Server logic -----------------------------------------------------------------


# FIXME: (JMP) It would be best to document all inputs and constants in ad
# dedicated R script using roxygen2.

# FIXME: (JMP) I would strongly consider enforcing a strict naming convention
# for objects, inputs, and outputs. snake_case and camelCase work best with R.
# Dots should be avoided as they can be confused with S3 methods. I chose
# snake_case until further notice.

# FIXME: (JMP) Some symbols (I/O names, function names, and variables)
# are in French. English is mixed with French. This must be fixed, we
# should really use a consistent source language.

# FIXME: (JMP) Many numeric values are passed to signif() which yields a
# variable number of (significant) digits. Is this truly needed? For UX
# purposes, I suggest always keeping 2 digits?

# FIXME: (JMP) The app heavily relies on functions defined in scripts. These
# must be revamped (those containing plot functions being the top priority).
# Theis contents should be split into a proper set of functions stored in
# multiple R files in R/. Each function should be documented and manually
# tested (at least).


server <- function(input, output, session) {
    # NOTE: (JMP) This reactive value is called by bayesian.analysis
    # below. It has no other reference in the code. This could later
    # be refactored by removing it, and passing 1L to fun.bayes.jags().
    # TODO: (JMP) Ask JL what to do with this. This is a weird case.
    uninformed.prior <- shiny::reactive({
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

    user.input <- shiny::reactive({
        return(
            list(
                conf = input$conf,
                psi  = input$psi,
                # The following input is located in panel Exceedance.
                frac_threshold = input$frac_threshold,
                # The following input is located in panel 95th Percentile.
                target_perc = input$target_perc))
    })

    formatted.sample <- shiny::reactive({
        return(
            data.formatting.SEG(
                data.in  = input$Data,
                oel      = input$oel,
                oel.mult = input$al))
    })

    bayesian.analysis <- shiny::reactive({
        on.exit(progress$close())
        progress <- shiny::Progress$new()
        progress$set(
            value   = 0L,
            message = gett("server.1"))

        # FIXME: (JMP) This is really weird. All Bayesian functions
        # accept a function that updates a Progress object implicitly.
        # This is error-prone and requires a fix. Binding 'progress'
        # may not exist, or could be out of scope in some cases.
        updateProgress <- function(detail = NULL) {
            progress$inc(amount = 1/50, detail = detail)
        }

        user_sample <- formatted.sample()
        return(
            fun.bayes.jags(
                observations     = user_sample$data ,
                notcensored      = user_sample$notcensored,
                leftcensored     = user_sample$leftcensored,
                rightcensored    = user_sample$rightcensored,
                intcensored      = user_sample$intcensored,
                seed             = user_sample$seed,
                c.oel            = user_sample$c.oel,
                n.iter           = 25000L,
                uninformed.prior = uninformed.prior(),
                updateProgress   = updateProgress))
    })

    num.res <- shiny::reactive({
        bayesian_outputs <- bayesian.analysis()
        user_inputs      <- user.input()
        return(
            all.numeric(
                mu.chain       = bayesian_outputs$mu.chain,
                sigma.chain    = bayesian_outputs$sigma.chain,
                c.oel          = formatted.sample()$c.oel,
                conf           = user_inputs$conf,
                frac_threshold = user_inputs$frac_threshold,
                target_perc    = user_inputs$target_perc))
    })


    ## Shared Outputs ----------------------------------------------------------


    output$frac.probSituUnacceptable <-
    output$perc.probSituUnacceptable <-
    output$am.probSituUnacceptable   <- shiny::renderText({
        paste(input$psi, "%", sep = "")
    })

    output$gm1.dist <-
    output$gm1.perc <-
    output$gm1.AM   <- shiny::renderText({
        return(gettt("frac.18", gett("gm.lwr"), paste0(input$conf, "%")))
    })

    output$gm1 <-
    output$gm2 <-
    output$gm3 <- shiny::renderText({
        gm <- lapply(num.res()$gm, \(x) as.character(signif(x, 2L)))
        return(sprintf("%s [ %s - %s ]", gm$est, gm$lcl, gm$ucl))
    })

    output$gsd1.dist <-
    output$gsd1.perc <-
    output$gsd1.AM   <- shiny::renderText({
        return(gettt("frac.18", gett("gsd.lwr"), paste0(input$conf, "%")))
    })

    output$gsd1 <-
    output$gsd2 <-
    output$gsd3 <- shiny::renderText({
        gsd <- lapply(num.res()$gsd, \(x) as.character(signif(x, 2L)))
        return(sprintf("%s [ %s - %s ]", gsd$est, gsd$lcl, gsd$ucl))
    })


    ## Panel: Descriptive ------------------------------------------------------


    # NOTE: (JMP) this rective value was commented in one version,
    # but it is called below, so I reactivated it (and formatted it).
    data.imputed <- reactive({
        user_sample <- formatted.sample()
        return(
            simple.censored.treatment(
                observations.formatted = user_sample$data,
                notcensored            = user_sample$notcensored,
                leftcensored           = user_sample$leftcensored,
                rightcensored          = user_sample$rightcensored,
                intcensored            = user_sample$intcensored))
    })


    ### Descriptive statistics -------------------------------------------------


    output$res.desc <- shiny::renderTable(rownames = FALSE, {
        result <- fun.desc.stat(
            data.simply.imputed = data.imputed(),
            c.oel = formatted.sample()$c.oel)

        result$parameter <- c(
            gett("n"),
            gett("res.desc.2"),
            gett("res.desc.3"),
            gett("res.desc.4"),
            gett("res.desc.5"),
            gett("res.desc.6"),
            gett("res.desc.7"),
            gett("res.desc.8"),
            gett("res.desc.9"),
            gett("res.desc.10"),
            gett("res.desc.11"),
            gett("res.desc.12"),
            gett("res.desc.13"))

        return(result)
    })

    # FIXME: (JMP) Better integration of <a> tags
    # with source text once it is reintroduced.
    output$descriptive.2 <- shiny::renderUI({
        htmltools::HTML(
            gettt("descriptive.2",
                html$a("NDexpo",
                  href   = "http://www.expostats.ca/site/app-local/NDExpo/",
                  target = "_blank"),
                html$a("Dennis Helsel",
                  href   = "http://www.practicalstats.com/info2use/books.html",
                  target = "_blank")))
    })


    ### QQ Plot ----------------------------------------------------------------


    # FIXME: (JMP) I would rename all arguments of this
    # function and use shorter and more semantic names.
    output$qqplot <- shiny::renderPlot({
        return(
            fun.qqplot(
                data.simply.imputed = data.imputed(),
                notcensored         = formatted.sample()$notcensored,
                # FIXME: these formals args must be renamed. This is horrible.
                qqplot.1            = gett("qqplot.1"),
                qqplot.2            = gett("qqplot.2"),
                qqplot.3            = gett("qqplot.3"),
                qqplot.4            = gett("qqplot.4"),
                qqplot.5            = gett("qqplot.5"),
                qqplot.6            = gett("qqplot.6")))
    })


    ### Box and Whiskers Plot --------------------------------------------------


    # FIXME: (JMP) I would rename all arguments of this
    # function and use shorter and more semantic names.
    output$boxplot <- shiny::renderPlot({
        user_sample <- formatted.sample()
        return(
            fun.boxplot(
                data.simply.imputed = data.imputed(),
                notcensored         = user_sample$notcensored,
                c.oel               = user_sample$c.oel,
                # FIXME: these formals args must be renamed. This is horrible.
                boxplot.1           = gett("d.boxplot.1"),
                boxplot.2           = gett("d.boxplot.2"),
                boxplot.3           = gett("d.boxplot.3"),
                boxplot.4           = gett("d.boxplot.4"),
                boxplot.5           = gett("d.boxplot.5")))
    })


    ## Panel: Exceedance -------------------------------------------------------


    toutesVariantesFD <- shiny::reactive({
        numerics       <- num.res()
        fracDepasseEst <- ceiling(numerics$frac$est)
        fracDepasseLim <- ceiling(numerics$frac$ucl)

        paramsVariantes <- paramsVariantesFracDep(
            images_dir,
            file.path(images_dir, "flask.png"),
            file.path(images_dir, "flask-lines.png"),
            input$couleurRisque,
            input$couleurAucunRisque,
            input$couleurSeuil,
            input$couleurFond)

        seuil <- input$frac_threshold

        # FIXME: (JMP) dalist? Find a better semantic name.
        # Maybe 'out', 'plots'?
        dalist <- list(
            figure1 = list(
                drawPlot(
                    paramsVariantes,
                    fracDepasseEst = seuil,
                    titre = gett("frac.graph.13.4.soustitre1")),
                drawPlot(
                    paramsVariantes,
                    fracDepasseEst = fracDepasseEst,
                    titre = gett("frac.graph.13.4.soustitre2"))),
            figure2 = list(
                drawPlot(
                    paramsVariantes,
                    fracDepasseEst = seuil,
                    titre = gett("frac.graph.13.4.soustitre1")),
                drawPlot(
                    paramsVariantes,
                    fracDepasseEst = fracDepasseEst,
                    fracDepasseLim = fracDepasseLim,
                    titre = gett("frac.graph.13.4.soustitre2"))),
            figure3 = list(
                drawPlot(
                    paramsVariantes,
                    fracDepasseEst = fracDepasseEst,
                    seuil = seuil)),
            figure4 = list(
                drawPlot(
                    paramsVariantes,
                    fracDepasseEst = fracDepasseEst,
                    fracDepasseLim = fracDepasseLim,
                    seuil = seuil)))

        leng <- length(dalist[[input$varianteFracDep]])

        # The JS handler function is defined in www/js/exceedance-plot.js.
        session$sendCustomMessage("exceedance-plot-handler", leng);
        return(dalist)
    })


    ### Shared Outputs ---------------------------------------------------------


    output$acceptableExpo1 <-
    output$acceptableExpo2 <-
    output$acceptableExpo3 <- shiny::renderText({
        return(sprintf("%.2f%%", input$frac_threshold))
    })


    ### Risk Decision ----------------------------------------------------------


    # See subsection Panel: Exceedance - Shared Outputs
    # above for output$acceptableExpo1.

    # FIXME: (JMP) Use a dedicated formatting function.
    output$probrisk <- shiny::renderText({
        return(paste0(signif(num.res()$frac.risk, 3L), "%"))
    })

    # See section Shared Outputs above for output$frac.probSituUnacceptable.

    # FIXME: (JMP) output$finalrisk, output$finalrisk.perc, and
    # output$finalrisk.AM are almost identical calls. They could
    # be encapsulated into a single function with two inputs.
    output$finalrisk <- shiny::renderText({
        msgid <- if (num.res()$frac.risk >= user.input()$psi) {
            "server.2"
        } else {
            "server.3"
        }

        return(gett(msgid))
    })

    # FIXME: (JMP): All risk meters (output$risquemetre, output$risquemetre2,
    # and output$riskmetre.am) are generated using the exact same inputs.
    # Code can be further reduced by setting these inputs as default ones.
    output$risquemetre <- shiny::renderPlot({
        return(
            dessinerRisqueMetre(
                actualProb          = num.res()$frac.risk,
                minProbUnacceptable = user.input()$psi,
                colorProb           = "darkblue",
                actualProb2         = NULL,
                colorProb2          = "#4863A0"))
    })


    ### Parameter Estimates: Distribution --------------------------------------


    # See section Shared Outputs above for
    #  - output$gm1.dist,
    #  - output$gm1,
    #  - output$gsd1.dist, and
    #  - output$gsd1.


    ### Parameter Estimates: Exceedance Fraction -------------------------------


    # TODO: (JMP) Outputs Frac, Perc, and AM share the same format.
    # Only inputs vary. Encapsulate logic into a formatting function.
    output$Frac <- shiny::renderText({
        return(paste0(signif(num.res()$frac$est, 3L), "%"))
    })

    # TODO: (JMP) Outputs Frac.ci, Perc.ci, and AM.ci share the same format.
    # Only inputs vary. Encapsulate logic into a formatting function.
    output$Frac.ci <- shiny::renderText({
        numerics <- num.res()
        return(
            sprintf(
                "[ %.3f - %.3f ]",
                signif(numerics$frac$lcl, 3L),
                signif(numerics$frac$ucl, 3L)))
    })


    ### Exceedance Plot --------------------------------------------------------

    # FIXME: (JMP) This does not scale accordingly on
    # first clicks on varianteFracDep.
    output$fracDepVariantes <- shiny::renderPlot({
        variant <- input$varianteFracDep

        shinyjs::removeClass("fracDepVariantes", "app-half-width")
        switch(variant,
            figure3 = shinyjs::addClass("fracDepVariantes", "app-half-width"),
            figure4 = shinyjs::addClass("fracDepVariantes", "app-half-width"))

        ptlist <- toutesVariantesFD()[[variant]]
        return(gridExtra::grid.arrange(grobs = ptlist, ncol = length(ptlist)))
    })
    })


    ### Sequential Plot --------------------------------------------------------


    output$seqplot.frac <- shiny::renderPlot({
        numerics <- num.res()
        return(
            sequential.plot.frac(
                gm        = numerics$gm$est,
                gsd       = numerics$gsd$est,
                frac      = numerics$frac$est,
                c.oel     = formatted.sample()$c.oel,
                # FIXME: these formals args must be renamed. This is horrible.
                seqplot.1 = gett("seqplot.1"),
                seqplot.2 = gett("seqplot.2"),
                seqplot.6 = gett("seqplot.7")))
    })


    ### Density Plot -----------------------------------------------------------


    output$distplot.frac <- shiny::renderPlot({
        bayesian_outputs <- bayesian.analysis()
        return(
            distribution.plot.frac(
                gm         = exp(median(bayesian_outputs$mu.chain)),
                gsd        = exp(median(bayesian_outputs$sigma.chain)),
                frac       = num.res()$frac$est ,
                c.oel      = formatted.sample()$c.oel,
                # FIXME: these formals args must be renamed. This is horrible.
                distplot.1 = gett("distplot.1"),
                distplot.2 = gett("distplot.2"),
                distplot.3 = gett("distplot.3"),
                distplot.4 = gett("distplot.4"),
                distplot.5 = gett("distplot.5")))
    })


    ### Risk Band Plot ---------------------------------------------------------


    output$frac.acceptableExpoDiv1 <-
    output$frac.acceptableExpoDiv2 <- shiny::renderText({
        return(paste0(input$frac_threshold / 10, "%"))
    })

    # See subsection Panel: Exceedance - Shared Outputs above for
    #  - output$acceptableExpo2 and
    #  - output$acceptableExpo3.

    output$riskband.frac <- shiny::renderPlot({
        bayesian_outputs <- bayesian.analysis()
        user_inputs      <- user.input()
        return(
            riskband.plot.frac(
                mu.chain       = bayesian_outputs$mu.chain,
                sigma.chain    = bayesian_outputs$sigma.chain,
                c.oel          = formatted.sample()$c.oel,
                frac_threshold = user_inputs$frac_threshold,
                psi            = user_inputs$psi,
                # FIXME: these formals args must be renamed. This is horrible.
                riskplot.1     = gett("riskplot.1"),
                riskplot.2     = gett("riskplot.2")))
    })


    ## Panel: 95th Percentile --------------------------------------------------


    ### Shared Outputs ---------------------------------------------------------


    output$perc.percentile.title           <-
    output$perc.percentile.h3              <-
    output$perc.percentile.risk.decision   <-
    output$perc.percentile.param.estimates <-
    output$perc.percentile.risk.band       <- shiny::renderText({
        return(percText(input$target_perc))
    })


    ### Risk Decision ----------------------------------------------------------


    # See subsection Panel: 95th Percentile - Shared Outputs above
    # for output$perc.percentile.title, output$perc.percentile.h3,
    # and perc.percentile.risk.decision.

    # FIXME: (JMP) Use a dedicated formatting function.
    output$probrisk.perc <- shiny::renderText({
        return(paste0(signif(num.res()$perc.risk, 3L), "%"))
    })

    # See section Shared Outputs above for output$perc.probSituUnacceptable.

    # FIXME: (JMP) output$finalrisk, output$finalrisk.perc, and
    # output$finalrisk.AM are almost identical calls. They could
    # be encapsulated into a single function with two inputs.
    output$finalrisk.perc <-shiny::renderText({
        msgid <- if (num.res()$perc.risk >= user.input()$psi) {
            "server.2"
        } else {
            "server.3"
        }

        return(gett(msgid))
    })

    # FIXME: (JMP): All risk meters (output$risquemetre, output$risquemetre2,
    # and output$riskmetre.am) are generated using the exact same inputs.
    # Code can be further reduced by setting these inputs as default ones.
    output$risquemetre2 <- shiny::renderPlot({
        return(
            dessinerRisqueMetre(
                actualProb          = num.res()$perc.risk,
                minProbUnacceptable = user.input()$psi,
                colorProb           = "darkblue",
                actualProb2         = NULL,
                colorProb2          = "#4863A0"))
    })


    ### Parameter Estimates: Distribution --------------------------------------


    # See section Shared Outputs above for
    #  - output$gm2.dist,
    #  - output$gm2,
    #  - output$gsd2.dist, and
    #  - output$gsd2.


    ### Parameter Estimates: 95th percentile -----------------------------------


    # See subsection Panel: 95th Percentile - Shared Outputs
    # above for output$perc.percentile.param.estimates.

    # TODO: (JMP) Outputs Frac, Perc, and AM share the same format.
    # Only inputs vary. Encapsulate logic into a formatting function.
    output$Perc <- shiny::renderText({
        # FIXME: (JMP) Call to paste0 below is missing a second argument.
        # Based on the context, it should likely be "%". Am I right? Ask JL.
        return(paste0(signif(num.res()$perc$est, 3L)))
    })

    # TODO: (JMP) Outputs Frac.ci, Perc.ci, and AM.ci share the same format.
    # Only inputs vary. Encapsulate logic into a formatting function.
    output$Perc.ci <- shiny::renderText({
        numerics <- num.res()
        return(
            sprintf(
                "[ %.3f - %.3f ]",
                signif(numerics$perc$lcl, 3L),
                signif(numerics$perc$ucl, 3L)))
    })


    ### Sequential Plot --------------------------------------------------------


    output$seqplot.perc <- shiny::renderPlot({
        numerics <- num.res()
        return(
            sequential.plot.perc(
                gm          = numerics$gm$est,
                gsd         = numerics$gsd$est,
                perc        = numerics$perc$est,
                c.oel       = formatted.sample()$c.oel,
                target_perc = user.input()$target_perc,
                # FIXME: these formals args must be renamed. This is horrible.
                seqplot.1   = gett("seqplot.1"),
                seqplot.3   = gett("seqplot.3"),
                seqplot.4   = gett("seqplot.4"),
                seqplot.6   = gett("seqplot.7")))
    })


    ### Density Plot -----------------------------------------------------------


    output$distplot.perc <- shiny::renderPlot({
        bayesian_outputs <- bayesian.analysis()
        return(
            distribution.plot.perc(
                gm          = exp(median(bayesian_outputs$mu.chain)),
                gsd         = exp(median(bayesian_outputs$sigma.chain)),
                perc        = num.res()$perc$est,
                target_perc = user.input()$target_perc,
                c.oel       = formatted.sample()$c.oel,
                # FIXME: these formals args must be renamed. This is horrible.
                distplot.1  = gett("distplot.1"),
                distplot.2  = gett("distplot.2"),
                distplot.4  = gett("distplot.4"),
                distplot.5  = gett("distplot.5"),
                distplot.6  = gett("distplot.6")))
    })


    ### Risk Band Plot ---------------------------------------------------------


    # See subsection Panel: 95th Percentile - Shared Outputs
    # above for output$perc.percentile.risk.band.

    # FIXME: (JMP) Better integration of <a> tags
    # with source text once it is reintroduced.
    output$perc.graph.12.2 <- shiny::renderUI({
        htmltools::HTML(
            gettt("perc.graph.12.2",
                html$a("AIHA",
                    href   = "https://www.aiha.org",
                    target = "_blank")))
    })

    output$riskband.perc <- shiny::renderPlot({
        bayesian_outputs <- bayesian.analysis()
        user_inputs      <- user.input()
        return(
            riskband.plot.perc(
                mu.chain    = bayesian_outputs$mu.chain,
                sigma.chain = bayesian_outputs$sigma.chain,
                c.oel       = formatted.sample()$c.oel,
                target_perc = user_inputs$target_perc,
                psi         = user_inputs$psi,
                # FIXME: these formals args must be renamed. This is horrible.
                riskplot.2  = gett("riskplot.2"),
                riskplot.3  = gett("riskplot.3"),
                riskplot.4  = gett("riskplot.4"),
                riskplot.5  = gett("riskplot.5"),
                riskplot.6  = gett("riskplot.6"),
                riskplot.7  = gett("riskplot.7"),
                riskplot.8  = gett("riskplot.8")))
    })


    ## Panel: Arithmetic Mean --------------------------------------------------


    ### Risk Decision ----------------------------------------------------------


    # FIXME: (JMP) Use a dedicated formatting function.
    output$probrisk.AM <- shiny::renderText({
        return(paste0(signif(num.res()$am.risk, 3L), "%"))
    })

    # See section Shared Outputs above for output$am.probSituUnacceptable.

    # FIXME: (JMP) output$finalrisk, output$finalrisk.perc, and
    # output$finalrisk.AM are almost identical calls. They could
    # be encapsulated into a single function with two inputs.
    output$finalrisk.AM <-shiny::renderText({
        msgid <- if (num.res()$am.risk >= user.input()$psi) {
            "server.2"
        } else {
            "server.3"
        }

        return(gett(msgid))
    })

    # FIXME: (JMP): All risk meters (output$risquemetre, output$risquemetre2,
    # and output$riskmetre.am) are generated using the exact same inputs.
    # Code can be further reduced by setting these inputs as default ones.
    output$risquemetre.am <- renderPlot({
        return(
            dessinerRisqueMetre(
                actualProb          = num.res()$am.risk,
                minProbUnacceptable = user.input()$psi,
                colorProb           = "darkblue",
                actualProb2         = NULL,
                colorProb2          = "#4863A0"))
    })


    ### Parameter Estimates: Distribution --------------------------------------


    # See section Shared Outputs above for
    #  - output$gm1.AM,
    #  - output$gm3,
    #  - output$gsd1.AM, and
    #  - output$gsd3.


    ### Parameter Estimates: Arithmetic Mean -----------------------------------


    # TODO: (JMP) Outputs Frac, Perc, and AM share the same format.
    # Only inputs vary. Encapsulate logic into a formatting function.
    output$AM <- shiny::renderText({
        # FIXME: (JMP) Call to paste0 below is missing a second argument.
        # Based on the context, it should likely be "%". Am I right? Ask JL.
        return(paste0(signif(num.res()$am$est, 3L)))
    })

    # TODO: (JMP) Outputs Frac.ci, Perc.ci, and AM.ci share the same format.
    # Only inputs vary. Encapsulate logic into a formatting function.
    output$AM.ci <- shiny::renderText({
        numerics <- num.res()
        return(
            sprintf(
                "[ %.3f - %.3f ]",
                signif(numerics$am$lcl, 3L),
                signif(numerics$am$ucl, 3L)))
    })


    ### Sequential Plot --------------------------------------------------------


    output$seqplot.AM <- shiny::renderPlot({
        numerics <- num.res()
        return(
            sequential.plot.am(
                gm        = numerics$gm$est,
                gsd       = numerics$gsd$est,
                am        = numerics$am$est,
                c.oel     = formatted.sample()$c.oel,
                # FIXME: these formals args must be renamed. This is horrible.
                seqplot.1 = gett("seqplot.1"),
                seqplot.3 = gett("seqplot.3"),
                seqplot.5 = gett("seqplot.5"),
                seqplot.6 = gett("seqplot.7")))
    })


    ### Density Plot -----------------------------------------------------------


    output$distplot.AM <- shiny::renderPlot({
        bayesian_outputs <- bayesian.analysis()
        return(
            distribution.plot.am(
                gm         = exp(median(bayesian_outputs$mu.chain)),
                gsd        = exp(median(bayesian_outputs$sigma.chain)),
                am         = num.res()$am$est,
                c.oel      = formatted.sample()$c.oel,
                # FIXME: these formals args must be renamed. This is horrible.
                distplot.1 = gett("distplot.1"),
                distplot.2 = gett("distplot.2"),
                distplot.4 = gett("distplot.4"),
                distplot.5 = gett("distplot.5"),
                distplot.7 = gett("distplot.7")))
    })


    ### Risk Band Plot ---------------------------------------------------------


    # FIXME: (JMP) Better integration of <a> tags
    # with source text once it is reintroduced.
    output$am.graph.12 <- shiny::renderUI({
        htmltools::HTML(
            gettt("am.graph.12",
                html$a("AIHA",
                    href   = "https://www.aiha.org",
                    target = "_blank")))
    })

    output$riskband.am <- shiny::renderPlot({
        bayesian_outputs <- bayesian.analysis()
        return(
            riskband.plot.am(
                mu.chain    = bayesian_outputs$mu.chain,
                sigma.chain = bayesian_outputs$sigma.chain,
                c.oel       = formatted.sample()$c.oel,
                psi         = user.input()$psi,
                # FIXME: these formals args must be renamed. This is horrible.
                riskplot.2  = gett("riskplot.2"),
                riskplot.3  = gett("riskplot.3"),
                riskplot.4  = gett("riskplot.4"),
                riskplot.5  = gett("riskplot.5"),
                riskplot.6  = gett("riskplot.6"),
                riskplot.7  = gett("riskplot.7"),
                riskplot.9  = gett("riskplot.9")))
    })


    ## Panel: Instructions -----------------------------------------------------


    # Static panel. It has no server logic.


    ## Panel: About ------------------------------------------------------------


    # FIXME: (JMP) Better integration of <a> tags
    # with source text once it is reintroduced.
    output$about.1 <- shiny::renderUI({
        # FIXME: (JMP) This does not really work and should be replaced
        # when source text is integrated back into the app.
        htmltools::HTML(
            gettt("about.1",
                html$a(
                    gett("ESPUM"),
                    href   = "https://www.espum.umontreal.ca",
                    target = "_blank"),
                html$a(
                    "Université de Montréal",
                    href   = "https://www.umontreal.ca"),
                    target = "_blank"))
    })


    ## Panel: Methodological Background ----------------------------------------


    # FIXME: (JMP) Better integration of <a> tags
    # with source text once it is reintroduced.
    output$back.1.intro <- shiny::renderUI({
        htmltools::HTML(
            gettt("back.1",
                html$a(
                    gett("this"),
                    target = "_blank",
                    href   = "https://academic.oup.com/annweh/article/63/3/267/5248301")))
    })

    # FIXME: (JMP) Better integration of <a> tags
    # with source text once it is reintroduced.
    output$back.2 <- shiny::renderUI({
        # FIXME: (JMP) This does not work and should be replaced
        # when source text is integrated back into the app.
        # TODO: (JMP) The ifelse() should check future input input$lang.
        htmltools::HTML(
            gettt("back.2",
            html$a(
                gett("here"),
                target = "_blank",
                href   = ifelse(
                    test = TRUE,
                    yes  = "http://www.expostats.ca/site/en/info.html",
                    no   = "http://www.expostats.ca/site/info.html"))))
    })
}


# Instantiation ----------------------------------------------------------------


app <- shiny::shinyApp(ui, server)
