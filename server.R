#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#
#
#
#     SERVER FOR TOOL1 : SEG ANALYSIS TOOL
#
#
#
#

###################    libraries



library(shiny)
library(rjags)
library(png)
library(ggimage)
library(gridExtra)

######################     scripts to source

setwd("./scripts")

##SEG SPECIFIC FUNCTIONS

source("SEG/Data formatting functions_SEG.R")

##COMMON

source("Common/Simple censored imputation functions.R")

source("Common/Descriptive numerical output functions.R")

source("Common/Descriptive graphs functions.R")

source("Common/Bayesian engine functions.R")

source("Common/Numerical output functions.R")

source("Common/Main graph functions.R")

setwd("..")

####################   SHINY STUFF

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  ###data formatting
  
  formatted.sample <- reactive({ 
    
    data.in <- input$Data
    
    oel <- input$oel
    
    oel.mult=input$al
    
    result <- data.sample.formatted <-data.formatting.SEG(data.in=data.in,
                                                          oel=oel, 
                                                          oel.mult = oel.mult)
    return(result)
    
  })
  
  ### input parameters
  
  user.input <- reactive({ 
    
    conf <- input$conf
    
    psi <- input$psi
    
    frac_threshold = input$frac_threshold
    
    target_perc = input$target_perc
    
    
    result <-list( conf=conf , 
                   psi = psi , 
                   frac_threshold = frac_threshold ,
                   target_perc = target_perc )
    
    return(result)
    
  })
  
  ###### simple imputation for descriptive statistics and graphs
  
  data.imputed <- reactive({ 
    
    X <- formatted.sample()
    
    result <-simple.censored.treatment(observations.formatted=X$data,
                                       notcensored=X$notcensored,
                                       leftcensored=X$leftcensored,
                                       rightcensored=X$rightcensored,
                                       intcensored=X$intcensored)
    
    return(result)
    
  })
  
  #####descriptive statistics
  
  
  #####descriptive table
  
  output$res.desc <- renderTable({
    
    X <- data.imputed() 
    
    Y <- formatted.sample()
    
    result <- fun.desc.stat(data.simply.imputed=X , c.oel = Y$c.oel )
    
    please <- gett('res.desc.2')
    
    
    result$parameter <-c(
      gett('n'),
      gett('res.desc.2'),
      gett('res.desc.3'),
      gett('res.desc.4'),
      gett('res.desc.5'),
      gett('res.desc.6'),
      gett('res.desc.7'),
      gett('res.desc.8'),
      gett('res.desc.9'),
      gett('res.desc.10'),
      gett('res.desc.11'),
      gett('res.desc.12'),
      gett('res.desc.13')
    )
    
    return(result)
    
    
  },include.rownames=FALSE)
  
  #######simple qqplot
  
  
  output$qqplot <- renderPlot({
    
    
    X <- data.imputed() 
    
    
    Y <- formatted.sample()
    
    p <- fun.qqplot(data.simply.imputed= X  , 
                    notcensored = Y$notcensored,
                    qqplot.1=gett("qqplot.1"),
                    qqplot.2=gett("qqplot.2"),
                    qqplot.3=gett("qqplot.3"),
                    qqplot.4=gett("qqplot.4"),
                    qqplot.5=gett("qqplot.5"),
                    qqplot.6=gett("qqplot.6"))
    
    print(p)
    
  })
  
  #################bayesian analysis
  
  bayesian.analysis <- reactive({ 
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = gett("server.1"), value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(detail = NULL) {
      progress$inc(amount = 1/50, detail = detail)
    }
    
    
    X <- formatted.sample()
    
    uninformed.prior <- uninformed.prior()
    
    result <-fun.bayes.jags(observations=X$data , 
                            notcensored=X$notcensored, 
                            leftcensored=X$leftcensored ,
                            rightcensored=X$rightcensored ,
                            intcensored=X$intcensored , 
                            seed=X$seed , 
                            c.oel=X$c.oel , n.iter=25000 , uninformed.prior,
                            updateProgress = updateProgress) 
    
    return(result)
    
  })
  
  ######### numerical ouputs
  
  uninformed.prior <- reactive({
    prior <- "1" # input$prior.sigma
    return(!strtoi(prior))
  })
  
  output$foo <- renderText({ input$prior.sigma})
  
  num.res <- reactive({ 
    
    
    X <- bayesian.analysis()
    
    Y <- formatted.sample()
    
    Z <- user.input()
    
    
    
    result <- all.numeric(mu.chain=X$mu.chain ,
                          sigma.chain=X$sigma.chain ,
                          conf=Z$conf ,
                          c.oel=Y$c.oel ,
                          frac_threshold=Z$frac_threshold ,
                          target_perc=Z$target_perc)
    
    
    return(result)
    
  })
  
  #      
  #                    
  ######################################## EXCEEDANCE
  #      
  #      
  
  #point estimate
  output$Frac <-renderText({
    
    X <- num.res()
    
    res4<-X$frac$est
    
    return(paste(signif(res4,3)," %",sep=""))
    
  })
  
  #confidence level
  output$conf.out <-renderText({
    
    X <- user.input()
    
    return(paste(X$conf," %",sep=""))
    
  })
  
  
  output$conf.out.2 <-renderText({
    
    X <- user.input()
    
    return(paste(X$conf," %",sep=""))
    
  })
  
  
  #confidence interval
  output$Frac.ci <-renderText({
    
    X <- num.res()
    
    lcl <-X$frac$lcl
    ucl <-X$frac$ucl
    
    return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
    
  })
  
  #risk probability
  output$probrisk <-renderText({
    
    X <- num.res()
    
    risk <-X$frac.risk
    
    return(paste(signif(risk,3)," %",sep=""))
    
  })
  
  #risk decision
  output$finalrisk <-renderText({
    
    X <- num.res()
    Y <- user.input()
    
    risk <-X$frac.risk
    
    if (risk>=Y$psi) return(gett("server.2"))
    
    else return(gett("server.3"))
  })
  
  
  ##gm point estimate + confidence interval
  
  output$gm1 <-renderText({
    
    X <- num.res()
    
    est <-X$gm$est
    
    lcl <-X$gm$lcl
    
    ucl <-X$gm$ucl
    
    return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
    
    
  })
  
  #confidence level
  output$conf.out.7 <-renderText({
    
    X <- user.input()
    
    return(paste(X$conf," %",sep=""))
    
  })
  
  
  output$gm1.dist <- renderText({
    gettt("frac.18",
            gett("gm.lwr"),
            paste(input$conf," %",sep=""))
  })
  
  output$gsd1.dist <- renderText({
    gettt("frac.18",
            gett("gsd.lwr"),
            paste(input$conf," %",sep=""))
  })
  
  output$gm1.frac <- renderText({
    gettt("frac.18",
          gett("gm.lwr"),
          paste(input$conf," %",sep=""))
  })
  
  output$gsd1.frac <- renderText({
    gettt("frac.18",
          gett("gsd.lwr"),
          paste(input$conf," %",sep=""))
  })
  
  output$gm1.perc <- renderText({
    gettt("frac.18",
          gett("gm.lwr"),
          paste(input$conf," %",sep=""))
  })
  
  output$gsd1.perc <- renderUI({
    gettt("frac.18",
          gett("gsd.lwr"),
          paste(input$conf," %",sep=""))
  })
  
  output$gm1.AM <- renderText({
    gettt("frac.18",
          gett("gm.lwr"),
          paste(input$conf," %",sep=""))
  })
  
  output$gsd1.AM <- renderText({
    gettt("frac.18",
          gett("gsd.lwr"),
          paste(input$conf," %",sep=""))
  })
  
  
  ##gsd point estimate + confidence interval
  
  output$gsd1 <-renderText({
    
    X <- num.res()
    
    est <-X$gsd$est
    
    lcl <-X$gsd$lcl
    
    ucl <-X$gsd$ucl
    
    return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
    
  })
  
  #confidence level
  output$conf.out.8 <-renderText({
    
    X <- user.input()
    
    return(paste(X$conf," %",sep=""))
    
  })
  
  #############  CALENDAR PLOT
  
  
  output$calendarplot <- renderPlot({
    
    X <- num.res()
    
    graph1 <- calendar.plot(frac.est=X$frac$est,
                            calplot.1=gett("calplot.1"),
                            calplot.2=gett("calplot.2"),
                            calplot.3=gett("calplot.3"))
    
    print(graph1)
    
  })
  
  ############# SEQUENTIAL PLOT
  
  
  output$seqplot.frac <- renderPlot({
    
    X <- num.res()
    Y <- formatted.sample()
    Z <- data.imputed()
    
    
    graph2 <- sequential.plot.frac(gm=X$gm$est , 
                                   gsd=X$gsd$est ,
                                   frac=X$frac$est , 
                                   c.oel=Y$c.oel ,
                                   seqplot.1=gett("seqplot.1"),
                                   seqplot.2=gett("seqplot.2"),
                                   seqplot.6=gett("seqplot.7"))
    
    suppressWarnings(print(graph2))
    
  })
  
  
  ############# DISTRIBUTION PLOT
  
  
  output$distplot.frac <- renderPlot({
    
    X <- num.res()
    Y <- formatted.sample()
    Z <- bayesian.analysis()
    
    ### exceedance best estimate
    
    frac.dep.est <-X$frac$est
    
    ###mu and sigma
    
    mu <- median(Z$mu.chain)
    sigma <-median(Z$sigma.chain)
    
    
    ####graph
    
    graph4 <- distribution.plot.frac(gm=exp(mu) , 
                                     gsd=exp(sigma)  ,
                                     frac=frac.dep.est , 
                                     c.oel=Y$c.oel,
                                     distplot.1=gett("distplot.1"),
                                     distplot.2=gett("distplot.2"),
                                     distplot.3=gett("distplot.3"),
                                     distplot.4=gett("distplot.4"),
                                     distplot.5=gett("distplot.5"))
    
    suppressWarnings(print(graph4))
    
  })
  
  ############ RISK BAND GRAPH
  
  
  output$riskband.frac <- renderPlot({
    
    X <- formatted.sample()
    Y <- bayesian.analysis()
    Z <- user.input()
    
    
    graph8 <-riskband.plot.frac(mu.chain=Y$mu.chain,
                                sigma.chain=Y$sigma.chain,
                                c.oel=X$c.oel,
                                frac_threshold = Z$frac_threshold,
                                psi=Z$psi,
                                riskplot.1=gett("riskplot.1"),
                                riskplot.2=gett("riskplot.2"))
    
    suppressWarnings(print(graph8))
  })
  
  
  
  #
  #             
  ##########################################    95th percentile
  #
  #
  
  
  #point estimate
  output$Perc <-renderText({
    
    X <- num.res()
    
    res5<-X$perc$est
    
    return(paste(signif(res5,3)))
    
  })
  
  #confidence level
  output$conf.out.3 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  
  output$conf.out.4 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  output$am.crit <- renderText({ return(gett("am.crit")) })
  
  #confidence interval
  output$Perc.ci <-renderText({
    
    X <- num.res()
    
    lcl <-X$perc$lcl
    
    ucl <-X$perc$ucl
    
    return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
    
  })
  
  
  #risk probability
  output$probrisk.perc <-renderText({
    
    X <- num.res()
    
    risk <-X$perc.risk
    
    return(paste(signif(risk,3)," %",sep=""))
    
  })
  
  #risk decision
  output$finalrisk.perc <-renderText({
    
    X <- num.res()
    Y <- user.input()
    
    risk <-X$perc.risk
    
    if (risk>=Y$psi) return(gett("server.2"))
    
    else return(gett("server.3"))
    
    
  })
  
  ##gm point estimate + confidence intervall
  
  output$gm2 <-renderText({
    
    X <- num.res()
    
    est <-X$gm$est
    
    lcl <-X$gm$lcl
    
    ucl <-X$gm$ucl
    
    return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
    
    
  })
  
  #confidence level
  output$conf.out.98 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  ##gsd point estimate + confidence intervall
  
  output$gsd2 <-renderText({
    
    X <- num.res()
    
    est <-X$gsd$est
    
    lcl <-X$gsd$lcl
    
    ucl <-X$gsd$ucl
    
    return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
    
  })   
  
  #confidence level
  output$conf.out.10 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  
  ############# SEQUENTIAL PLOT
  
  
  output$seqplot.perc <- renderPlot({
    
    X <- num.res()
    Y <- formatted.sample()
    Z <- data.imputed()
    W <- user.input()
    
    
    graph2 <- sequential.plot.perc(gm=X$gm$est , 
                                   gsd=X$gsd$est ,
                                   perc=X$perc$est , 
                                   c.oel=Y$c.oel ,
                                   target_perc=W$target_perc,
                                   seqplot.1=gett("seqplot.1"),
                                   seqplot.3=gett("seqplot.3"),
                                   seqplot.4=gett("seqplot.4"),
                                   seqplot.6=gett("seqplot.7"))
    
    suppressWarnings(print(graph2))
    
  })
  
  
  #############  DISTRIBUTION PLOT
  
  
  output$distplot.perc <- renderPlot({
    
    X <- num.res()
    Y <- formatted.sample()
    Z <- bayesian.analysis()
    W <- user.input()
    
    ### percentile best estimate
    
    perc.est <-X$perc$est
    
    ###mu and sigma
    
    mu <- median(Z$mu.chain)
    sigma <-median(Z$sigma.chain)
    
    #graph
    
    graph4 <- distribution.plot.perc(gm=exp(mu) , 
                                     gsd=exp(sigma)  ,
                                     perc=perc.est , 
                                     target_perc=W$target_perc ,
                                     c.oel=Y$c.oel,
                                     distplot.1=gett("distplot.1"),
                                     distplot.2=gett("distplot.2"),
                                     distplot.4=gett("distplot.4"),
                                     distplot.5=gett("distplot.5"),
                                     distplot.6=gett("distplot.6"))
    
    suppressWarnings(print(graph4))
    
  })
  
  
  
  ############# RISK BAND GRAPH
  
  
  output$riskband.perc <- renderPlot({
    
    X <- formatted.sample()
    Y <- bayesian.analysis()
    Z <- user.input()
    
    
    graph8 <-riskband.plot.perc(mu.chain=Y$mu.chain,
                                sigma.chain=Y$sigma.chain,
                                c.oel=X$c.oel,
                                target_perc = Z$target_perc,
                                psi=Z$psi,
                                riskplot.2=gett("riskplot.2"),
                                riskplot.3=gett("riskplot.3"),
                                riskplot.4=gett("riskplot.4"),
                                riskplot.5=gett("riskplot.5"),
                                riskplot.6=gett("riskplot.6"),
                                riskplot.7=gett("riskplot.7"),
                                riskplot.8=gett("riskplot.8"))
    
    suppressWarnings(print(graph8))
    
  })
  
  
  
  
  #
  #             
  ##########################################    ARITHMETIC MEAN
  #
  #
  
  
  #point estimate
  output$AM <-renderText({
    
    X <-num.res()
    
    est <- X$am$est
    
    return(paste(signif(est,3)))
    
  })
  
  #confidence level
  output$conf.out.5 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  
  output$conf.out.6 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  
  #confidence interval
  output$AM.ci <-renderText({
    
    X <-num.res()
    
    lcl <- X$am$lcl
    
    ucl <- X$am$ucl
    
    return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
    
  })
  
  
  #risk probability
  output$probrisk.AM <-renderText({
    
    X <-num.res()
    
    risk <-X$am.risk
    
    return(paste(signif(risk,3)," %",sep=""))
    
  })
  
  #risk decision
  output$finalrisk.AM <-renderText({
    
    X <-num.res()
    Y <- user.input()
    
    risk <-X$am.risk
    
    if (risk>=Y$psi) return(gett("server.2"))
    
    else return(gett("server.3"))
    
    
  })
  
  ##gm point estimate + confidence interval
  
  output$gm3 <-renderText({
    
    X <-num.res()
    
    est <-X$gm$est
    
    lcl <-X$gm$lcl
    
    ucl <-X$gm$ucl
    
    return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
    
    
  })
  
  #confidence level
  output$conf.out.11 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  ##gsd point estimate + confidence interval
  
  output$gsd3 <-renderText({
    
    X <-num.res()
    
    est <-X$gsd$est
    
    lcl <-X$gsd$lcl
    
    ucl <-X$gsd$ucl
    
    return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
    
  })
  
  #confidence level
  output$conf.out.12 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  
  #############     SEQUENTIAL PLOT
  
  
  output$seqplot.AM <- renderPlot({
    
    X <- num.res()
    Y <- formatted.sample()
    Z <- data.imputed()
    
    
    
    graph6 <- sequential.plot.am(gm=X$gm$est , 
                                 gsd=X$gsd$est ,
                                 am=X$am$est , 
                                 c.oel=Y$c.oel ,
                                 seqplot.1=gett("seqplot.1"),
                                 seqplot.3=gett("seqplot.3"),
                                 seqplot.5=gett("seqplot.5"),
                                 seqplot.6=gett("seqplot.7"))
    suppressWarnings(print(graph6))
    
  })
  
  
  #############graphique de type distribution
  
  
  output$distplot.AM <- renderPlot({
    
    
    X <- num.res()
    Y <- formatted.sample()
    Z <- bayesian.analysis()
    
    
    ### AM best estimatee
    
    am.est <-X$am$est
    
    ###mu and sigma
    
    mu <- median(Z$mu.chain)
    sigma <-median(Z$sigma.chain)
    
    
    #graph
    
    graph4 <- distribution.plot.am(gm=exp(mu) , 
                                   gsd=exp(sigma)  ,
                                   am=am.est , 
                                   c.oel=Y$c.oel,
                                   distplot.1=gett("distplot.1"),
                                   distplot.2=gett("distplot.2"),
                                   distplot.4=gett("distplot.4"),
                                   distplot.5=gett("distplot.5"),
                                   distplot.7=gett("distplot.7"))
    
    suppressWarnings(print(graph4))
  })
  
  
  
  #############  RISK BAND PLOT
  
  
  output$riskband.am <- renderPlot({
    
    
    X <- formatted.sample()
    Y <- bayesian.analysis()
    Z <- user.input()
    
    
    graph8 <-riskband.plot.am(mu.chain=Y$mu.chain,
                              sigma.chain=Y$sigma.chain,
                              c.oel=X$c.oel,
                              psi=Z$psi,
                              riskplot.2=gett("riskplot.2"),
                              riskplot.3=gett("riskplot.3"),
                              riskplot.4=gett("riskplot.4"),
                              riskplot.5=gett("riskplot.5"),
                              riskplot.6=gett("riskplot.6"),
                              riskplot.7=gett("riskplot.7"),
                              riskplot.9=gett("riskplot.9"))
    
    suppressWarnings(print(graph8))
  })
  
  
  ######## RISKMETERS
  
  ## exceedance
  output$risquemetre <- renderPlot({
    
    X <-num.res()
    Y <- user.input()
    
    dessinerRisqueMetre(actualProb=X$frac.risk, 
                        minProbUnacceptable=Y$psi, 
                        colorProb="darkblue",
                        actualProb2=NULL, 
                        colorProb2="#4863A0")
  })
  
  ## percentile
  
  output$risquemetre2 <- renderPlot({
    X <-num.res()
    Y <- user.input()
    
    dessinerRisqueMetre(actualProb=X$perc.risk, 
                        minProbUnacceptable=Y$psi, 
                        colorProb="darkblue",
                        actualProb2=NULL, 
                        colorProb2="#4863A0")
  })
  
  ## arithmetic mean
  
  output$risquemetre.am <- renderPlot({
    
    X <-num.res()
    Y <- user.input()
    
    dessinerRisqueMetre(actualProb=X$am.risk, 
                        minProbUnacceptable=Y$psi, 
                        colorProb="darkblue",
                        actualProb2=NULL, 
                        colorProb2="#4863A0")
    
  })
  
  
  
  
  ####### HTML STUFF TO COMMENT BY DM  
  
  
  output$acceptableExpo1 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
  output$acceptableExpo2 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
  output$acceptableExpo3 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
  output$probSituUnacceptable1 <-renderText({return(paste(input$psi,"%",sep="")) })
  output$probSituUnacceptable2 <-renderText({return(paste(input$psi,"%",sep="")) })
  output$probSituUnacceptable3 <-renderText({return(paste(input$psi,"%",sep="")) })
  
  output$acceptableExpoDiv10_1 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
  output$acceptableExpoDiv10_2 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
  output$acceptableExpoDiv10_3 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
  output$acceptableExpoDiv10_4 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
  output$acceptableExpoDiv10_5 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
  output$acceptableExpoDiv10_6 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
  
  
  ##### STILL USEFULL ?
  
  percText <- function(perc) {
    rem <- perc %% 10
    if ( rem == 1) {
      suff <- gett("st")
    } else
      if ( rem == 2 ) {
        suff <- gett("nd")
      } else
        if ( rem == 3 ) {
          suff <- gett("rd")
        } else {
          suff <- gett("th")
        }
    return (paste0(perc, suff, " ", gett("percentile") ) )
  }
  
  output$percentile1 <-renderText({percText(input$target_perc)})
  output$percentile2 <-renderText({percText(input$target_perc)})
  output$percentile3 <-renderText({percText(input$target_perc)})
  output$percentile4 <-renderText({percText(input$target_perc)})
  output$percentile5 <-renderText({percText(input$target_perc)})
  output$percentile6 <-renderText({percText(input$target_perc)})
  output$percentile7 <-renderText({percText(input$target_perc)})
  output$percentile8 <-renderText({percText(input$target_perc)})
  output$percentile9 <-renderText({percText(input$target_perc)})
  output$percentile10 <-renderText({percText(input$target_perc)})
  output$percentile11 <-renderText({percText(input$target_perc)})
  output$percentile12 <-renderText({percText(input$target_perc)})
  output$percentile13 <-renderText({percText(input$target_perc)})
  
  output$locale <-renderText({
    loc <- Sys.getlocale("LC_MESSAGES")
    return(loc)
    
  })
  
  output$descriptive.2 <- renderUI({
    HTML(
      gettt("descriptive.2",
            a('NDexpo',href='http://www.expostats.ca/site/app-local/NDExpo/', target="_blank"),
            a('Dennis Helsel',href='http://www.practicalstats.com/info2use/books.html',target="_blank")
      )
    )
  })
  
  output$perc.graph.12.2 <- renderUI({ HTML(gettt("perc.graph.12.2", a('AIHA',href='https://www.aiha.org',target="_blank"))) })
  
  output$am.graph.12 <- renderUI({ HTML(gettt("am.graph.12", a('AIHA',href='https://www.aiha.org',target="_blank"))) })
  
  output$back.2 <- renderUI({ HTML(gettt("back.2", a(gett('here'),href=(ifelse(tolower(substr(getwd(), nchar(getwd())-2+1, nchar(getwd())))=="en",'http://www.expostats.ca/site/en/info.html','http://www.expostats.ca/site/info.html')), target="_blank"))) })
  
  output$about.1 <- renderUI({
    HTML(
      gettt("about.1",
            a(gett("ESPUM"),href='https://www.espum.umontreal.ca',target="_blank"), a("Université de Montréal",href='https://www.umontreal.ca'),target='_blank')
    )
  })
  
  output$back.1.intro <- renderUI({
    HTML(
      gettt('back.1',
        a(gett("this"),href='https://academic.oup.com/annweh/article/63/3/267/5248301',target="_blank")
      )
    )
  })
  ##### boxplot
  output$boxplot <- renderPlot({
    
    
    X <- data.imputed() 
    
    
    Y <- formatted.sample()
    
    p <- fun.boxplot( data.simply.imputed = X, 
                      notcensored = Y$notcensored, 
                      c.oel =Y$c.oel,
                      boxplot.1=gett("d.boxplot.1"),
                      boxplot.2=gett("d.boxplot.2"),
                      boxplot.3=gett("d.boxplot.3"),
                      boxplot.4=gett("d.boxplot.4"),
                      boxplot.5=gett("d.boxplot.5"))
    
    print(p)
    
  })
  
  toutesVariantesFD <- reactive({
    icone <- "fiole.png"
    iconeIncert <- "fioleHach.png"
    imageSubdir <- 'img'
    slash <- '/'
    imageDir <- paste(getSrcDirectory(function(dummy) {dummy}), imageSubdir, sep = slash)
    cheminImagePD <- paste(imageDir, icone, sep = slash)
    cheminImageIncert <- paste(imageDir, iconeIncert, sep = slash)
    
    X <- num.res()
    fracDepasseEst = ceiling(X$frac$est)
    fracDepasseLim = ceiling(X$frac$ucl)
    seuil=input$frac_threshold
  
    paramsVariantes <- paramsVariantesFracDep(imageDir,
                                              cheminImagePD,
                                              cheminImageIncert,
                                              input$couleurRisque,
                                              input$couleurAucunRisque,
                                              input$couleurSeuil,
                                              input$couleurFond)
    dalist <- list(
      figure1=list(
        drawPlot(paramsVariantes, fracDepasseEst = seuil, titre = gett("frac.graph.13.4.soustitre1")),
        drawPlot(paramsVariantes, fracDepasseEst = fracDepasseEst, titre = gett("frac.graph.13.4.soustitre2"))
      ),
      figure2=list(
        drawPlot(paramsVariantes, fracDepasseEst = seuil, titre = gett("frac.graph.13.4.soustitre1")),
        drawPlot(paramsVariantes, fracDepasseEst = fracDepasseEst, fracDepasseLim = fracDepasseLim, titre = gett("frac.graph.13.4.soustitre2"))
      ),
      figure3=list(
        drawPlot(paramsVariantes, fracDepasseEst = fracDepasseEst, seuil = seuil)
      ),
      figure4=list(
        drawPlot(paramsVariantes, fracDepasseEst = fracDepasseEst, fracDepasseLim = fracDepasseLim, seuil = seuil)
      )
    )
    leng <- length(dalist[[input$varianteFracDep]])
    session$sendCustomMessage("handler1", leng );
    return(dalist)
  })
  
  output$fracDepVariantes <- renderPlot({
    variantes <- toutesVariantesFD()
    ptlist <- variantes[[input$varianteFracDep]]
    nplots <- length(ptlist)
    marg <- c(1,1,1,1)
    margin <- theme(plot.margin = unit(marg, "cm"))
    grid.arrange(grobs = lapply(ptlist, "+", margin), ncol=nplots)
  })
  
  output$fracDepVarianteDesc <-renderText({
    
    t <- input$varianteFracDep
    
    return(gett(paste0("frac.graph.13.4.desc.", t)))
    
  })
  
  ###### END OF NEW FOR BOXPLOT
})




