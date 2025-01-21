#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#
#
#
#     UI FOR TOOL1 : SEG ANALYSIS TOOL
#
#
#
#

library(shiny)
library(shinyBS)
library(ggplot2)
library(shinythemes)


inputTextarea <- function(inputId, value="", nrows, ncols) {
  tagList(
    singleton(tags$head(tags$script(src = "textarea.js"))),
    tags$textarea(id = inputId,
                  class = "inputTextarea",
                  rows = nrows,
                  cols = ncols,
                  as.character(value),
                  '28.9\n19.4\n<5.5\n149.9\n26.42\n56.1')
  )
}

source("./langParams.R")

tt <- function(param, txt="[0 &lt; valid &le; 100]") {
  return(bsTooltip(param, txt, "right", options = list(container = "body")))
}

inputWidth <- "110px"

shinyUI(fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel(gett("main.title.t1")),
  sidebarLayout(
    sidebarPanel(
      h4(gett("input.1")),
      #h4(textOutput("locale", inline=TRUE)),
      br(),
      numericInput("oel", gett("input.2"), 100, width=inputWidth) ,
      tt("oel", gett("input.2.tooltip")),
      
      numericInput("al", gett("input.3"), 1, width=inputWidth),
      tt("al", gett("input.3.tooltip")),
      
      numericInput("conf", gett("input.4"), 90, width=inputWidth),
      tt("conf", gett("input.4.tooltip")),
      
      
      numericInput("psi", gett("input.5"), 30, width=inputWidth),
      tt("psi", gett("input.5.tooltip")),
      
      h4(gett("input.6")),
      inputTextarea('Data', '',10,10 ),
      #br(),
      #br(),
      #br(),
      #br(),
      #radioButtons("prior.sigma", gett("prior.1"),
                   #choiceNames = list(
                     #gett("prior.a"),
                     #gett("prior.b")
                   #),
                   #selected=1,
                   #choiceValues = list(
                     #1, 0
                   #)),
      #tt("prior_sigma_1", gett("prior.1.tooltip")),
      #tt("prior_sigma_0", gett("prior.1.tooltip")),
      #actionButton("go",gett("input.7")),
      #bsTooltip("go", gett("input.7.tooltip"),  "right", options = list(container = "body"))
      width = 3),
    
    mainPanel(
      
      ####waiting message
      
      tags$head(tags$style(type="text/css", " #waitmessage { position: fixed; top: 0px; left: 0px; width: 100%; padding: 5px 0px 5px 0px; text-align: center; font-weight: bold; font-size: 100%; color: #000000; background-color: #CCFF66; z-index: 105; } #probsituacceptable, [for='probsituacceptable'] {display: none;} input[type='number'] { -moz-appearance:textfield; } input::-webkit-outer-spin-button, input::-webkit-inner-spin-button { -webkit-appearance: none; } ")),
      
      
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div(gett("input.8"),id="waitmessage")),
      tabsetPanel( 
        
        
        ############DESCRIPTIVE PANEL
        
        tabPanel(gett("descriptive.tab.name"),
                 h3(gett('descriptive.title')),
                 tableOutput('res.desc'),
                 
                 h4(gett('descriptive.1')),
                 
                 htmlOutput('descriptive.2'),
                 
                 br(),
                 h3(gett("descriptive.3")),
                 plotOutput('qqplot'),
                 br(),
                 p(gettt("descriptive.4")),
                 
                 ##### NEW FOR BOXPLOT
                 br(),
                 h3(gett("descriptive.5")),
                 plotOutput('boxplot'),
                 p(gett("descriptive.6")),
                 br()
                 
                 ####END OF NEW FOR BOXPLOT
        ),
        
        
        ############EXCEEDANCE FRACTION PANEL
        
        tabPanel(gett('frac.tab.name'),
                 h3(gett("frac.title")),
                 numericInput("frac_threshold", gett("frac.1"), width="160px", 5),
                 
                 tt("frac_threshold", gett("frac.1.tooltip")),
                 
                 #h4(strong(gett("frac.2"))),
                 
                 #p(gett("frac.3")),
                 
                 br(),
                 div(style="block",class="plot-with-text",
                  div(style="display: inline-block;width:55%",class="text",
                      h4(strong(gett("frac.7"))),
                      p("\u25B9", gett("frac.8")),
                      p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(gett("frac.9") ,"\u2265 ", textOutput("acceptableExpo1",inline=TRUE))),
                      p("\u25B9", gett("frac.10")),
                      p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probrisk",inline=TRUE))),
                      p("\u25B9", gett("frac.11")),
                      p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong('<', textOutput("probSituUnacceptable3",inline=TRUE))),
                      p("\u25B9", gett("frac.12")),
                      p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk", inline=TRUE)))
                  ),
                  div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre"))
                 ),
                 
                 h4(strong(gett("frac.4"))),
                 
                 p(textOutput("gm1.dist", inline=TRUE), strong(textOutput("gm1",inline=TRUE))),
                 p(textOutput("gsd1.dist", inline=TRUE), strong(textOutput("gsd1",inline=TRUE))),
                 br(),
                 
                 h4(strong(gett("frac.5"))),
                 p(gett("frac.6"),strong(textOutput("Frac",inline=TRUE))),
                 
                 p(gett("frac.6.1"), strong(textOutput("Frac.ci",inline=TRUE))),
                 
                 br(),
                 
                 #h4(strong(gett("frac.7"))),
                 #p(gett("frac.8"), strong(gett("frac.9") ,"\u2265 ", textOutput("acceptableExpo1",inline=TRUE))),
                 #p(gett("frac.10"),strong(textOutput("probrisk",inline=TRUE))),
                 #p(gett("frac.11"), strong('<', textOutput("probSituUnacceptable3",inline=TRUE))),
                 #p(gett("frac.12"),strong(textOutput("finalrisk",inline=TRUE))),
                 #br(),
                 ####graphs
                 
                 #calendar
                 h4(strong(gett("frac.graph.1"))),
                 p(strong(gett("frac.graph.2"))),
                 p(gett("frac.graph.3")),
                 plotOutput("calendarplot"),
                 br(),
                 
                 #sequential
                 p(strong(gett("frac.graph.4"))),
                 p(gett("frac.graph.5")),
                 p(gett("frac.graph.6")),
                 plotOutput("seqplot.frac"),
                 
                 ##density
                 br(),
                 p(strong(gett("frac.graph.7"))),
                 p(gett("frac.graph.8")),
                 p(gett("frac.graph.9")),
                 plotOutput("distplot.frac"),
                 sliderInput("iter.frac", label=gett("frac.graph.10"), min=1, max=50, value=1, step = 1,width = "100%",animate=animationOptions(interval = 4000)),
                 br(),
                 
                 ##risk band
                 p(strong(gett("frac.graph.11"))),
                 p(gett("frac.graph.12.1"), textOutput("acceptableExpoDiv10_1", inline=TRUE), gett("frac.graph.12.2"), textOutput("acceptableExpoDiv10_2", inline=TRUE), gett("frac.graph.12.3"), textOutput("acceptableExpo2",inline=TRUE), gett("frac.graph.12.4"), textOutput("acceptableExpo3",inline=TRUE),gett("frac.graph.12.5")),
                 plotOutput("riskband.frac"),
                 br()
                 
                 
        ),
        
        
        ############95th percenetile PANEL
        
        tabPanel(
          textOutput("percentile1",inline=TRUE),
          h3(gett('perc.title'), textOutput("percentile2",inline=TRUE)),
          numericInput("target_perc", gett("perc.1"), 95, width="160px"),
          tt("target_perc" , gett("perc.1.tooltip")),
          
          #h4(strong(gett("frac.2"))),
          #p(gett("frac.3")),
          
          br(),
          div(style="block",class="plot-with-text",
              div(style="display: inline-block;width:55%",class="text",
                  h4(strong(gett("frac.7"))),
                  p("\u25B9", gett("frac.8")),
                  p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("percentile5",inline=TRUE), "\u2265 ",  gett("OEL"))),
                  p("\u25B9", gett("frac.10")),
                  p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probrisk.perc",inline=TRUE))),
                  p("\u25B9", gett("frac.11")),
                  p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong('<', textOutput("probSituUnacceptable1",inline=TRUE))),
                  p("\u25B9", gett("frac.12")),
                  p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk.perc",inline=TRUE)))
              ),
              div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre2"))
          ),
          
          h4(strong(gett("frac.4"))),
          
          ##distribution parameters
          
          p(textOutput("gm1.perc", inline=TRUE), strong(textOutput("gm2", inline=TRUE))),
          p(htmlOutput("gsd1.perc", inline=TRUE), strong(textOutput("gsd2",inline=TRUE))),
          br(),
          
          ## estimation des percentiles
          
          h4(strong(gett("perc.5"), textOutput("percentile3",inline=TRUE))),
          
          p(gett("frac.6"),strong(textOutput("Perc",inline=TRUE))),
          
          p(gett("frac.6.1"), strong(textOutput("Perc.ci",inline=TRUE))),
          br(),
          
          ##graphs
          
          h4(strong(gett("frac.graph.1"))),
          p(strong(gett("frac.graph.4"))),
          p(gett("perc.graph.5")),
          p(gett("frac.graph.6")),
          plotOutput("seqplot.perc"),
          br(),
          
          
          
          p(strong(gett("frac.graph.7"))),
          p(gett("perc.graph.8")),
          p(gett("perc.graph.9")),
          plotOutput("distplot.perc"),
          sliderInput("iter.perc", label=gett("frac.graph.10"), min=1, max=50, value=1, step = 1,width = "100%",animate=animationOptions(interval = 4000)),
          br(),
          
          
          p(strong(gett("frac.graph.11"))),
          
          p(gett("perc.graph.12.1"), textOutput("percentile10",inline=TRUE), htmlOutput("perc.graph.12.2", inline=TRUE)),
          plotOutput("riskband.perc"),
          br()
          
          
        ),
        
        
        ############Arithmetic mean
        
        tabPanel(gett('am.tab.name'),
                 h3(gett('am.title')),
                 br(),
                 h4(strong(gett("frac.2"))),
                 p(gett("frac.3")),
                 
                 br(),
                 div(style="block",class="plot-with-text",
                     div(style="display: inline-block;width:55%",class="text",
                         h4(strong(gett("frac.7"))),
                         p("\u25B9", gett("frac.8")),
                         p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(gett("AM") ,"\u2265 ", gett("OEL"))),
                         p("\u25B9", gett("frac.10")),
                         p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probrisk.AM",inline=TRUE))),
                         p("\u25B9", gett("frac.11")),
                         p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong('<', textOutput("probSituUnacceptable2",inline=TRUE)) ),
                         p("\u25B9", gett("frac.12")),
                         p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk.AM",inline=TRUE)))
                     ),
                     div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.am")),
                     p(style="font-size: 13px",strong(gett("am.13")),gett("am.13.1")),
                     br()
                 ),
                 
                 #parameter estimates distribution
                 
                 h4(strong(gett("frac.4"))),
                 
                 p(textOutput("gm1.AM", inline=TRUE), strong(textOutput("gm3", inline=TRUE))),
                 p(textOutput("gsd1.AM", inline=TRUE), strong(textOutput("gsd3",inline=TRUE))),
                 br(),
                 
                 ## parameter estimates AM
                 
                 h4(strong(gett("am.5"))),
                 
                 p(gett("frac.6"),strong(textOutput("AM",inline=TRUE))),
                 
                 p(gett("frac.6.1"),strong(textOutput("AM.ci",inline=TRUE))),
                 
                 
                 
                 br(),
                 
                 ## GRAPHS
                 
                 
                 h4(strong(gett("frac.graph.1"))),
                 
                 #sequential
                 
                 p(strong(gett("frac.graph.4"))),
                 p(gett("am.graph.5")),
                 p(gett("frac.graph.6")),
                 
                 
                 plotOutput("seqplot.AM"),
                 
                 #density
                 
                 br(),
                 p(strong(gett("frac.graph.7"))),
                 
                 p(gett("am.graph.8")),
                 
                 p(gett("am.graph.9")),
                 plotOutput("distplot.AM"),
                 
                 sliderInput("iter.am", label=gett("frac.graph.10"), min=1, max=50, value=1, step = 1,width = "100%",animate=animationOptions(interval = 4000)),
                 br(),
                 
                 #risk band
                 
                 p(strong(gett("frac.graph.11"))),
                 p(htmlOutput("am.graph.12")),
                 plotOutput("riskband.am"),
                 br()
                 
                 
        ),
        
        ############INSTRUCTIONS
        
        tabPanel(gett("inst.tab.name"),
                 h3(gett('inst.title')),
                 br(),
                 p(gett("inst.1")),
                 p(gett("inst.2")),
                 p(gett("inst.3")),
                 
                 p(gett("inst.5"))
                 
        ),
        
        ############About
        
        tabPanel(gett("about.tab.name"),
                 h3(gett("about.title")),
                 br(),
                 p(htmlOutput("about.1")),
                 p(gett("about.2"))),
        
        
        ############BACKGROUND
        
        tabPanel(gett("back.tab.name"),
                 h3(gett("back.title")),
                 br(),
                 p(gett("back.1")),
                 p(htmlOutput("back.2", inline=TRUE))
                 
                 
                 
                 
        )
        
      )
    )
  )
))


