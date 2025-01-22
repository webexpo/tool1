#' Tool1: Data Interpretation for One Similarly Exposed Group
#'
#' This script includes both the UI and server logic. The current refactoring
#' focuses on providing an explicit namespace for each function call. The
#' overall structure will change in the future.
#'
#' @usage
#' source("app.R")
#'
#' @author Jérôme Lavoué (<jerome.lavoue@@umontreal.ca>)
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)


# Setup (Temporary) ------------------------------------------------------------


# This section will eventually be refactored into proper R scripts.


# Alias to htmltools' list containing
# functions used to create HTML5 tags.
html <- htmltools::tags

# CSS to include in <head>.
# TODO: (JMP) Store these statements in a real CSS file later.
mycss <- "
    div.shiny-plot-output { margin: 0 auto }
    div.plot-1 { width: 50% !important }
    div.plot-2 { width: 100% !important }
    div.shiny-plot-output > img { width: 100% }
    div.width-100, div.width-100 > * { width: 100% !important }
    div.width-140, div.width-140 > * { width: 140px !important }
    div.colour-inp { width: 24% !important }
    div.colour-inp > div > label { font-size: 12px }
    .inline-b { display: inline-block !important }
    .ital { font-style: italic !important }
    .va-mid { vertical-align: middle !important }
    div.float-l { float: left !important }
    div.float-r { float: right !important }
    #fieldset { border: 1px dotted grey !important; padding: 5px !important }
    fieldset > legend { font-size: 14px !important; margin-bottom: 2px !important }
    div.fieldset-body { display: none }
    legend.toggle-personnalisation > * { cursor: pointer; font-size: 15px !important; font-style: italic }
    div[data-shiny-input-type=colour] { margin-bottom: 2px !important }
    div[data-shiny-input-type=colour] input { height: 35px }
    div.fig-var-desc { text-align: justify; font-size: 14px !important; margin-left: 20px; margin-right: 20px }
    div.shiny-input-container { margin-bottom: 2px !important }"

# TODO: (JMP) Store this function in its own script later.
# FIXME: (JMP) This function initializes the text area in the
# sidebar but also sets Google Analytics in <head>? Why? Fix.
inputTextArea <- function(inputId, value = "", nrows, ncols) {
    return(
        htmltools::tagList(
            htmltools::singleton(html$head(html$script(src = "textarea.js"))),
            htmltools::singleton(html$head(html$script(src = "ga-id.js"))),
            htmltools::singleton(html$head(html$script(src = "google-analytics.js"))),
            htmltools::singleton(html$body(html$noscript(src = "gatm.js"))),
            html$textarea(
                id    = inputId,
                rows  = nrows,
                cols  = ncols,
                as.character(value),
                "28.9\n19.4\n<5.5\n149.9\n26.42\n56.1")))
}

# TODO: (JMP) Store this function in its own script later.
tooltip <- function(param, txt="[0 &lt; valid &le; 100]") {
  return(
    shinyBS::bsTooltip(
        id        = param,
        title     = txt,
        placement = "right",
        options   = list(container = "body")))
}

inputWidth <- "110px"


# User Interface ---------------------------------------------------------------


ui <- shiny::fluidPage(
    # NOTE: (JMP) I just discovered arg lang. This is awesome and could
    # naturally be integrated with transltr in a near future. This note
    # serves as a reminder.
    lang  = NULL,
    title = gett("main.title.t1"),
    theme = shinythemes::shinytheme("flatly"),


    ## Header ------------------------------------------------------------------


    html$head(html$style(htmltools::HTML(mycss))),
    shiny::titlePanel(gett("main.title.t1"), NULL),


    ## Body --------------------------------------------------------------------


    shiny::sidebarLayout(


        ### Sidebar ------------------------------------------------------------


        shiny::sidebarPanel(
            width = 3,
            htmltools::singleton(
                html$head(
                    html$script(
                        'Shiny.addCustomMessageHandler("handler1", function(message) {
                            $fig = $("#fracDepVariantes");
                            $fig.removeClass("plot-2 plot-1");
                            $fig.addClass("plot-"+ message);
                            });'
                    )
                )
            ),

            htmltools::singleton(
                html$head(
                    html$script('
                        $(document).ready(function() {
                            $("legend.toggle-personnalisation").click(function() {
                                $fieldset = $(this).closest("fieldset");
                                $icon = $fieldset.find(".glyphicon");
                                $fbody = $fieldset.find(".fieldset-body");
                                $fbody.toggleClass("show");
                                var show = $fbody.hasClass("show");
                                if ( show ) {
                                $fbody.show("blind");
                                } else {
                                $fbody.hide("blind");
                                }
                                $icon.removeClass("glyphicon-plus glyphicon-minus");
                                $icon.addClass("glyphicon-" + (show ? "minus" : "plus"));
                            });
                        });'
                    )
                )
            ),

            html$h4(gett("input.1")),
            html$br(),
            shiny::numericInput("oel",
                label = gett("input.2"),
                value = 100,
                width = inputWidth),
            tooltip("oel", gett("input.2.tooltip")),

            shiny::numericInput("al",
                label = gett("input.3"),
                value = 1,
                width = inputWidth),
            tooltip("al", gett("input.3.tooltip")),

            shiny::numericInput("conf",
                label = gett("input.4"),
                value = 90,
                width = inputWidth),
            tooltip("conf", gett("input.4.tooltip")),

            shiny::numericInput("psi",
                label = gett("input.5"),
                value = 30,
                width = inputWidth),
            tooltip("psi", gett("input.5.tooltip")),

            html$h4(gett("input.6")),
            inputTextArea("Data", NULL, nrows = 10L, ncols = 10L)
        ),


        ### Main ---------------------------------------------------------------


        shiny::mainPanel(


            #### Waiting Message -----------------------------------------------


            html$head(
                html$style(
                    type="text/css",
                    "#waitmessage { position: fixed; top: 0px; left: 0px; width: 100%; padding: 5px 0px 5px 0px; text-align: center; font-weight: bold; font-size: 100%; color: #000000; background-color: #CCFF66; z-index: 105; } #probsituacceptable, [for='probsituacceptable'] {display: none;} input[type='number'] { -moz-appearance:textfield; } input::-webkit-outer-spin-button, input::-webkit-inner-spin-button { -webkit-appearance: none; } ")
            ),

            shiny::conditionalPanel(
                html$div(gett("input.8")),
                id        = "waitmessage",
                condition = "$('html').hasClass('shiny-busy')"
            ),


            #### Panels --------------------------------------------------------


            shiny::tabsetPanel(


                ##### Panel: Descriptive ---------------------------------------


                shiny::tabPanel(
                    title = gett("descriptive.tab.name"),
                    html$h3(gett("descriptive.title")),
                    shiny::tableOutput("res.desc"),

                    html$h4(gett("descriptive.1")),
                    shiny::htmlOutput("descriptive.2"),

                    html$br(),
                    html$h3(gett("descriptive.3")),
                    shiny::plotOutput("qqplot"),
                    html$br(),
                    html$p(gettt("descriptive.4")),

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


                    shiny::numericInput(
                        inputId = "frac_threshold",
                        label   = gett("frac.1"),
                        width   = "160px",
                        value   = 5),

                    tooltip("frac_threshold", gett("frac.1.tooltip")),


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
                                   shiny::textOutput("probSituUnacceptable3", inline = TRUE))
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

                    html$fieldset(
                        html$legend(
                            class = "toggle-personnalisation",
                            html$span(gett("frac.graph.13.2")),
                            html$span(class = "glyphicon glyphicon-plus")
                        ),
                        html$div(
                            class="fieldset-body",
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

                    shinycssloaders::withSpinner(shiny::plotOutput("fracDepVariantes")),


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
                        shiny::textOutput("acceptableExpoDiv10_1", inline = TRUE),
                        gett("frac.graph.12.2"),
                        shiny::textOutput("acceptableExpoDiv10_2", inline = TRUE),
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
                    title = shiny::textOutput("percentile1", inline = TRUE),
                    html$h3(
                        gett("perc.title"),
                        shiny::textOutput("percentile2",inline = TRUE)
                    ),


                    ###### Input: Percentile -----------------------------------


                    shiny::numericInput(
                        inputId = "target_perc",
                        label   = gett("perc.1"),
                        value   = 95,
                        width   = "160px"),
                    tooltip("target_perc" , gett("perc.1.tooltip")),


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
                                html$strong(shiny::textOutput("percentile5", inline = TRUE),
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
                                    shiny::textOutput("probSituUnacceptable1", inline = TRUE))
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
                        shiny::textOutput("percentile3", inline = TRUE))),
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
                        shiny::textOutput("percentile10", inline = TRUE),
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
                                    shiny::textOutput("probSituUnacceptable2", inline = TRUE)
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


# Instantiation ----------------------------------------------------------------


# Pass this object to shiny::runApp()
# to launch the application.
app <- shiny::shinyApp(ui, server)
