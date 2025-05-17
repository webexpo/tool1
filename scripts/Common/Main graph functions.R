######################################################
#
#
#
# graphical output functions
#
# V1.03    4 juin 2018 correction des graphes sequentiel - n<50
# V1.02    2 mai 2018 script delphines Bosson
# V1.01    22 march 2018 adding psi to the riskband plots
# V1.00   20 march 2018
#
#
#
######################################################

#library(ggplot2)
#library(colorspace)
#library(colorRamps)

##
#
#  INPUT : output from various other functions, descriptive, MCMC chains, numerical results
#
##


###########        CALENDAR PLOT          ##################


calendar.plot <-function(frac.est,
                         pal_col = TRUE,
                         calplot.1="Week",
                         calplot.2="Week days",
                         calplot.3="Month"
) {


  frac.dep <-frac.est

  #Color
  if(pal_col == TRUE){
    vec_color <- c( "green4","red4")
  }else{
    vec_color <- c( "gray40","gray5")
  }

  nmonths<-12
  nweeks<- 4
  ndays<-5
  Sys.setlocale("LC_TIME", "C")
  vec.months <-format(ISOdatetime(2000,1:12,1,0,0,0),"%b")[1:nmonths]
  vec.days=substr(weekdays(as.Date(4,"1970-01-01",tz="GMT")+0:6)[1:ndays], 1, 3)



  results <- data.frame(month = factor(rep(vec.months,each=ndays*nweeks), levels=vec.months),
                        week = paste(calplot.1,rep(rep(1:nweeks, each=ndays), nmonths),sep=" "),
                        day = factor(rep(vec.days, nmonths*nweeks), levels=vec.days),
                        dep=as.factor(rbinom(n=nmonths*nweeks*ndays, size=1, prob=frac.dep/100)))

  graph1 <- ggplot(results, aes(day, month))+geom_tile(aes(fill=dep),colour="gray80")+
    facet_wrap(~week, ncol=nweeks)+
    scale_fill_manual(values= vec_color)+
    xlab(calplot.2)+
    ylab(calplot.3)+
    theme(legend.position = "none") +
    theme(aspect.ratio = 4/(1+sqrt(5)))+
    theme(axis.title.x=element_text(vjust=-0.5,size=20))+
    theme(axis.title.y=element_text(size=20,angle=90))+
    theme(axis.text.x=element_text(size=11, color="grey30"))+
    theme(axis.text.y=element_text(size=11,angle=0,color="grey30"))+
    theme(strip.text.x = element_text(size = 15, colour = "grey20", angle = 0))

  # ggsave("Calendar_article_fig1_BW.tiff", graph1, dpi = 600, width=11, height=8.5)
  # ggsave("Calendar_article_fig1_BW.pdf", graph1, dpi = 600, width=11, height=8.5)
  # ggsave("Calendar_article_fig1_BW.png", graph1, dpi = 600, width=11, height=8.5)

  return(graph1)

}


###########        SEQUENTIAL PLOT        ##################


###FRAC


sequential.plot.frac <-function( gm , gsd , frac , c.oel ,
                                 pal_col = TRUE,
                                 seqplot.1="Concentration",
                                 seqplot.2="Exceedance Fraction",
                                 seqplot.6="Measurement Index") {

  mu <-log(gm)

  sigma<-log(gsd)

  data <-data.frame(seq=1:250,
                    x=exp(rnorm(250,mu,sigma)))

  frac.dep <-frac

  #Color
  if(pal_col == TRUE){
    rect_line <- "red"
    vec_color <- c("blue", "red")
    vec_fill <- c("blue", "red")
  }else{
    rect_line <- "black"
    vec_color <- c("black", "black")
    vec_fill <- c("white", "white")
  }



  #####graph


  graph2 <- ggplot(data=data, aes(y=x,x=seq))+
    geom_abline(intercept=c.oel, slope=0,size=1.3,color = rect_line)+
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=c.oel, ymax=Inf, alpha=0.3, fill = rect_line)+
    geom_point(color= "gray15",size=1.8, shape = 16)+
    annotate("text", x = 130, y = c.oel*1.65, colour = "black",
             label = paste0(seqplot.2,": ",signif(frac.dep,3),"%"),
             size=6)+
    ylim(c(0,2*c.oel))+

    theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
    theme(axis.text.x = element_text(size = 14),axis.title.x = element_text(size = 16,vjust=-1.55))+
    labs(x = seqplot.6, y=seqplot.1)+
    theme(aspect.ratio= 0.7) +


    theme(legend.position="none") + theme(axis.line = element_line(size = 3, colour = "grey80")) +
    theme(axis.ticks = element_line(size = 2))

  return(graph2)
}




##### PERC

sequential.plot.perc <-function(gm , gsd , perc , target_perc, target_perc_suffix,
                                c.oel ,
                                pal_col = TRUE,
                                seqplot.1="Concentration",
                                seqplot.3="OEL",
                                seqplot.4="Percentile",
                                seqplot.6="Measurement Index") {

  mu <-log(gm)

  sigma<-log(gsd)

  data <-data.frame(seq=1:250,
                    x=exp(rnorm(250,mu,sigma)))

  #Color
  if(pal_col == TRUE){
    rect_line <- "blue"
    vec_color <- c("blue", "red")
    vec_fill <- c("blue", "red")
  }else{
    rect_line <- "black"
    vec_color <- c("black", "black")
    vec_fill <- c("white", "white")
  }



  #####graph


  graph2 <- ggplot(data=data, aes(y=x,x=seq))+
    geom_abline(intercept=perc, slope=0,size=1.3,color = vec_color[1]) +
    geom_abline(intercept=c.oel, slope=0,size=1.1,color = vec_color[2], linetype="dotted") +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=0, ymax=perc, alpha=0.2, fill= rect_line)+
    geom_point(color= "gray15",size=1.8, shape = 16)+
    ylim(c(0,pmax(1.2*perc,1.2*c.oel)))+

    theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
    theme(axis.text.x = element_text(size = 14),axis.title.x = element_text(size = 16,vjust=-1.55))+
    labs(x = seqplot.6, y=seqplot.1)+
    theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)))+
    theme(aspect.ratio=4/5)+

    annotate("text", x = 120, y = (c.oel*1.1),
             label = seqplot.3,
             size=6,hjust=0.5,color=vec_color[2]) +
    annotate("text", x = 120, y = (perc*1.1),
             label = paste0(target_perc, target_perc_suffix, " ", seqplot.4),
             size=6,hjust=0.5,color=vec_color[1]) +



    theme(legend.position="none") + theme(axis.line = element_line(size = 3, colour = "grey80")) +
    theme(axis.ticks = element_line(size = 2))



  return(graph2)

}

##### AM



sequential.plot.am <-function(gm , gsd , am , c.oel ,
                              pal_col = TRUE,
                              seqplot.1="Concentration",
                              seqplot.3="OEL",
                              seqplot.5="Arithmetic mean",
                              seqplot.6="Measurement Index") {

  mu <-log(gm)

  sigma<-log(gsd)

  data <-data.frame(seq=1:250,
                    x=exp(rnorm(250,mu,sigma)))


  #Color
  if(pal_col == TRUE){
    rect_line <- "green4"
    vec_color <- c("blue", "red")
    vec_fill <- c("blue", "red")
  }else{
    rect_line <- "black"
    vec_color <- c("black", "black")
    vec_fill <- c("white", "white")
  }



  #####graph


  graph2 <- ggplot(data=data, aes(y=x,x=seq))+
    geom_abline(intercept=am, slope=0,size=1.3,color = rect_line)+
    geom_abline(intercept=c.oel, slope=0,size=1.1,color= vec_color[2],linetype="dotted")+
    annotate("text", x = 120, y = (c.oel*1.1), label = seqplot.3, size=6,hjust=0.5,color= vec_color[2]) +
    annotate("text", x = 120, y = (am*1.1), label = seqplot.5, size=6,hjust=0.5,color = rect_line) +
    geom_point(color= "gray15",size=1.8, shape = 16) +
    ylim(c(0,pmax(1.5*c.oel,1.5*am)))+

    theme(axis.text.y=element_text(size=14),axis.title.y=element_text(size=16,vjust=+0.55))+
    theme(axis.text.x = element_text(size = 14),axis.title.x = element_text(size = 16,vjust=-1.55))+
    labs(x=seqplot.6, y=seqplot.1) +
    theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)))+
    theme(aspect.ratio=4/5) +



    theme(legend.position="none") + theme(axis.line = element_line(size = 3, colour = "grey80")) +
    theme(axis.ticks = element_line(size = 2))

  return(graph2)

}


##########       DISTRIBUTION PLOT           #################


####### FRAC

              distribution.plot.frac <-function( gm , gsd , frac , c.oel,

                                                 distplot.1="Concentration",
                                                 distplot.2="Density",
                                                 distplot.3="Exceedance Fraction",
                                                 distplot.4="OEL outside of graphical limits.",
                                                 distplot.5="OEL") {

              mu <-log(gm)

              sigma<-log(gsd)


               min <-exp(qnorm( 0.001, mu , sigma ))
               max <-exp(qnorm( 0.98 , mu , sigma ))

               frac.dep <-frac

               mydata <-data.frame(x=seq(min,max,length=5000))
               mydata$y<- dlnorm(mydata$x,meanlog=mu,sdlog=sigma)

               graph3 <-ggplot(data=mydata, aes(x=x))+
                 geom_line(aes(y=y),colour='black',size=1.25)+
                 ylim(0,max(mydata$y))+
                 xlab(distplot.1)+
                 ylab(distplot.2)+
                 theme(legend.position = "none") +
                 theme(aspect.ratio = 2/(1+sqrt(5)))  +
                 theme(axis.title.x=element_text(vjust=0,size=16))+
                 theme(axis.title.y=element_text(size=16,angle=90))+
                 theme(axis.text.x=element_text(size=18))+
                 theme(axis.text.y=element_text(size=14,angle=90))#+


               if (c.oel>max) {

                 graph3 <-graph3+ annotate("text", x = (max-min)/2, y = (max(mydata$y)/1.5),
                                           label = paste0(distplot.3, ": ", signif(frac.dep,3), "%"),
                                           size=6,hjust=0.5)+

                   annotate("text", x = (max-min)/2, y = (max(mydata$y)/2.5),
                            label = (distplot.4),
                            size=6,hjust=0.5)}

               if  (c.oel<=max)  {

                 graph3 <-graph3+geom_segment(aes(x = c.oel, y = 0,
                                                  xend = c.oel,
                                                  yend = dlnorm(c.oel,mu,sigma)),size=1.3,color="red")+
                   geom_area(aes(x=pmax(x,c.oel),y=y),fill='red',alpha=I(1/3))+
                   annotate("text", x = (max-min)/2, y = (max(mydata$y)/1.5),
                            label =  paste0(distplot.3, ": ", signif(frac.dep,3), "%"),
                            size=6,hjust=0.5)+
                   annotate("text", x = c.oel*1.05, y = dlnorm(c.oel,mu,sigma)*1.3,
                            label = distplot.5,
                            size=6,color="red")}


               return(graph3)

              }


####### PERC

  distribution.plot.perc <-function( gm , gsd , perc , target_perc , target_perc_suffix, c.oel ,
                                     distplot.1="Concentration",
                                     distplot.2="Density",
                                     distplot.4="OEL outside of graphical limits.",
                                     distplot.5="OEL",
                                     distplot.6="Percentile") {

        mu <-log(gm)

        sigma<-log(gsd)

        min <-exp(qnorm( 0.001, mu , sigma ))
        max <-exp(qnorm( 0.98 , mu , sigma ))

        mydata <-data.frame(x=seq(min,max,length=5000))
        mydata$y<- dlnorm(mydata$x,meanlog=mu,sdlog=sigma)

        graph3 <-ggplot(data=mydata, aes(x=x))+
          geom_line(aes(y=y),colour='black',size=1.25)+
          ylim(0,max(mydata$y))+
          xlab(distplot.1)+
          ylab(distplot.2)+
          theme(legend.position = "none") +
          theme(aspect.ratio = 2/(1+sqrt(5)))  +
          theme(axis.title.x=element_text(vjust=0,size=16))+
          theme(axis.title.y=element_text(size=16,angle=90))+
          theme(axis.text.x=element_text(size=18))+
          theme(axis.text.y=element_text(size=14,angle=90))+
          geom_segment(aes(x = perc, y = 0,
                         xend = perc,
                         yend = (max(mydata$y)/2.5)),size=1.3,color="blue")+
          geom_area(aes(x=pmin(x,perc),y=y),fill='blue',alpha=I(1/3))+
          annotate("text", x = perc, y = (max(mydata$y)/2.05),
                   label = paste0(target_perc, target_perc_suffix, " ", distplot.6),
                   size=6,color="blue")

        if(c.oel>0.75*max) xannot <-(c.oel/2)

        if (c.oel>max) {

          graph3 <-graph3+annotate("text", x = (max-min)/2, y = (max(mydata$y)/1.25),
                                   label = distplot.4,
                                   size=6,color="red")}

        if  (c.oel<=max)  {

          graph3 <-graph3+geom_segment(aes(x = c.oel, y = 0,xend = c.oel,
                                            yend = (max(mydata$y)/1.3)),size=1.3,color="red"  ,linetype="dotted")+
          annotate("text", x = c.oel, y = (max(mydata$y)/1.25),
                   label = distplot.5,
                   size=6,color="red")}


        return(graph3)

      }



  ####### AM

  distribution.plot.am <-function( gm , gsd , am , c.oel ,
                                   distplot.1="Concentration",
                                   distplot.2="Density",
                                   distplot.4="OEL outside of graphical limits.",
                                   distplot.5="OEL",
                                   distplot.7="Arithmetic mean") {

    mu <-log(gm)

    sigma<-log(gsd)

    min <-exp(qnorm( 0.001, mu , sigma ))
    max <-exp(qnorm( 0.98 , mu , sigma ))

    mydata <-data.frame(x=seq(min,max,length=5000))
    mydata$y<- dlnorm(mydata$x,meanlog=mu,sdlog=sigma)

    graph7 <-ggplot(data=mydata, aes(x=x))+
      geom_line(aes(y=y),colour='black',size=1.25)+
      ylim(0,max(mydata$y))+
      xlab(distplot.1)+
      ylab(distplot.2)+
      theme(legend.position = "none") +
      theme(aspect.ratio = 2/(1+sqrt(5)))  +
      theme(axis.title.x=element_text(vjust=0,size=16))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.x=element_text(size=18))+
      theme(axis.text.y=element_text(size=14,angle=90))


    if (c.oel>max) {

      graph7 <-graph7+geom_segment(aes(x = am, y = 0,
                                       xend = am,
                                       yend = (max(mydata$y)/2.25)),size=1.3,color="green4")+
        annotate("text", x = (max-min)/2, y = (max(mydata$y)/1.15),
                 label = distplot.4,
                 size=6,color="red")+
        annotate("text", x = am, y = (max(mydata$y)/2),
                 label = distplot.7,
                 size=6,color="green4")

      }

    if  (c.oel<=max)  {

      graph7 <-graph7+geom_segment(aes(x = am, y = 0,
                                       xend = am,
                                       yend = (max(mydata$y)/2.25)),size=1.3,color="green4")+
        geom_segment(aes(x = c.oel, y = 0,
                         xend = c.oel,
                         yend = (max(mydata$y)/1.25)),size=1.3,color="red",linetype="dotted")+

        annotate("text", x = c.oel, y = (max(mydata$y)/1.25),
                 label = distplot.5,
                 size=6,color="red")+
        annotate("text", x = am, y = (max(mydata$y)/1.95),
                 label = distplot.7,
                 size=6,color="green4")

      }


    return(graph7)

  }



##########       RISKBAND PLOT        #################


#### FRAC

riskband.plot.frac <-function(mu.chain , sigma.chain , c.oel , frac_threshold , psi=30,

                              riskplot.1="Exceedance Category",
                              riskplot.2="Probability"
                              ) {



  mu <-mu.chain

  sigma<-sigma.chain

  frac.chain <-100*(1-pnorm((log(c.oel)-mu)/sigma))

  bande1 <- frac_threshold/10

    C1 <-100*length(frac.chain[frac.chain<bande1])/length(frac.chain)
    C2 <-100*length(frac.chain[frac.chain>=bande1 & frac.chain<frac_threshold])/length(frac.chain)
    C3 <-100*length(frac.chain[frac.chain>=frac_threshold ])/length(frac.chain)

    # ≤ and ≥ may not render in all IDEs. These are Unicode
    # characters U+2264 (&leq;) (Less-Than or Equal To) and
    # U+2265 (&geq;) (Greater-Than Or Equal To).
    cats <- factor(c('C1','C2','C3'),labels=c(paste0('EF ≤ ', bande1, '%'),paste0(bande1, '% < EF ≤ ', frac_threshold, '%'),paste0('EF ≥ ', frac_threshold, '%')))

    data <-data.frame(perc=c(C1,C2,C3),cat=cats)

    graph8<- ggplot(data,aes(x=cats,y=perc))
    graph8 <-graph8+
      geom_bar(stat="identity",fill=c('green4','yellow','red'))+
      theme(aspect.ratio=0.6)+
      xlab(riskplot.1)+
      ylab(riskplot.2) +
      theme(axis.title.x=element_text(size=16,vjust=-1))+
      theme(axis.text.x=element_text(size=16))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.y=element_text(size=13,angle=90,hjust=0.5))+
      theme(legend.position = "none")+
      geom_text(x=1:3,y=c(C1,C2,C3)+5,label=paste(signif(c(C1,C2,C3),3),'%',sep=''),size=5,colour='grey28') +
      scale_y_continuous(breaks=c(0,20,40,60,80,100),limits=c(0,110))

    graph8 <-graph8+geom_segment(aes(x = 2.5, y = psi,
                     xend = 3.5,
                     yend = psi),size=1.25,color="black",linetype=2)

  return(graph8)

}



#### PERC

  riskband.plot.perc <-function(mu.chain , sigma.chain , c.oel , target_perc, psi=30,
                                # ≤ may not render in all IDEs. This is Unicode
                                # character U+2264 (&leq;) (Less-Than or Equal To).
                                riskplot.2  = "Probability",
                                riskplot.3  = "≤ 1% OEL",
                                riskplot.4  = "1% < OEL ≤ 10%",
                                riskplot.5  = "10% < OEL ≤ 50%",
                                riskplot.6  = "50% < OEL ≤ 100%",
                                riskplot.7  = "> OEL",
                                riskplot.8  = "Critical Percentile Category") {

    mu <-mu.chain

    sigma<-sigma.chain

    perc.chain <-exp(mu+qnorm(target_perc/100)*sigma)

    C1 <-100*length(perc.chain[perc.chain<0.01*c.oel])/length(perc.chain)
    C2 <-100*length(perc.chain[perc.chain>=0.01*c.oel & perc.chain<0.1*c.oel])/length(perc.chain)
    C3 <-100*length(perc.chain[perc.chain>=0.1*c.oel & perc.chain<0.5*c.oel])/length(perc.chain)
    C4 <-100*length(perc.chain[perc.chain>=0.5*c.oel & perc.chain<1*c.oel])/length(perc.chain)
    C5 <-100*length(perc.chain[perc.chain>=1*c.oel ])/length(perc.chain)


    cats <- factor(c('C1','C2','C3','C4','C5'),labels=c(riskplot.3,riskplot.4,riskplot.5,riskplot.6,riskplot.7))

    data <-data.frame(perc=c(C1,C2,C3,C4,C5),cat=cats)


    graph9<- ggplot(data,aes(x=cats,y=perc))
    graph9 <-graph9+
      geom_bar(stat="identity",fill=c('darkcyan','chartreuse4','darkseagreen2','orange','red'))+
      theme(aspect.ratio=0.6)+
      theme(axis.title.x=element_text(size=16,vjust=-1))+
      theme(axis.text.x=element_text(size=13))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.y=element_text(size=13,angle=90,hjust=0.5))+
      theme(legend.position = "none")+
      geom_text(x=1:5,y=c(C1,C2,C3,C4,C5)+5,label=paste(signif(c(C1,C2,C3,C4,C5),3),'%',sep=''),size=5,colour='grey28') +
      scale_x_discrete(riskplot.8, labels = cats, guide = guide_axis(n.dodge = 2L)) +
      scale_y_continuous(riskplot.2, breaks=c(0,20,40,60,80,100),limits=c(0,110))

    graph9 <-graph9+geom_segment(aes(x = 4.5, y = psi,
                                     xend = 5.5,
                                     yend = psi),size=1.25,color="black",linetype=2)

    return(graph9)

  }


###### AM

  riskband.plot.am <-function(mu.chain , sigma.chain , c.oel , psi=30,
                              riskplot.2  = "Probability",
                              riskplot.3  = "≤ 1% OEL",
                              riskplot.4  = "1% < OEL ≤ 10%",
                              riskplot.5  = "10% < OEL ≤ 50%",
                              riskplot.6  = "50% < OEL ≤ 100%",
                              riskplot.7  = "> OEL",
                              riskplot.9  = "Arithmetic Mean Category") {

    mu <-mu.chain

    sigma<-sigma.chain

    am.chain <-exp(mu+0.5*sigma^2)

    C1 <-100*length(am.chain[am.chain<0.01*c.oel])/length(am.chain)
    C2 <-100*length(am.chain[am.chain>=0.01*c.oel & am.chain<0.1*c.oel])/length(am.chain)
    C3 <-100*length(am.chain[am.chain>=0.1*c.oel & am.chain<0.5*c.oel])/length(am.chain)
    C4 <-100*length(am.chain[am.chain>=0.5*c.oel & am.chain<1*c.oel])/length(am.chain)
    C5 <-100*length(am.chain[am.chain>=1*c.oel ])/length(am.chain)


    cats <- factor(c('C1','C2','C3','C4','C5'),labels=c(riskplot.3,riskplot.4,riskplot.5,riskplot.6,riskplot.7))

    data <-data.frame(perc=c(C1,C2,C3,C4,C5),cat=cats)


    graph10<- ggplot(data,aes(x=cats,y=perc))
    graph10 <-graph10+
      geom_bar(stat="identity",fill=c('darkcyan','chartreuse4','darkseagreen2','orange','red'))+
      theme(aspect.ratio=0.6)+
      theme(axis.title.x=element_text(size=16,vjust=-1))+
      theme(axis.text.x=element_text(size=13))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.y=element_text(size=13,angle=90,hjust=0.5))+
      theme(legend.position = "none")+
      geom_text(x=1:5,y=c(C1,C2,C3,C4,C5)+5,label=paste(signif(c(C1,C2,C3,C4,C5),3),'%',sep=''),size=5,colour='grey28') +
      scale_x_discrete(riskplot.9, labels = cats, guide = guide_axis(n.dodge = 2L)) +
      scale_y_continuous(riskplot.2, breaks=c(0,20,40,60,80,100),limits=c(0,110))

    graph10 <-graph10+geom_segment(aes(x = 4.5, y = psi,
                                     xend = 5.5,
                                     yend = psi),size=1.25,color="black",linetype=2)

    return(graph10)

  }



  ####################################      RISKMETER                 #################################

  dessinerRisqueMetre <- function(actualProb, minProbUnacceptable=5, colorProb="darkblue", actualProb2=NULL, colorProb2="#4863A0")
  {
    #mat <- matrix(c(1,2), nrow=1)
    #png("C:/Users/p0070611/AppData/Local/Temp/layout1.png")
    #layout(mat)
    #x <- rnorm(200)
    #y <- 25 - 22*x + 5*x^2 + rnorm(200)
    #boxplot(x, horizontal=TRUE, axes=FALSE)
    #plot.new()
    ##little rescale
    par(mar = rep(0, 4))
    actualProb <- actualProb/100

    minProbUnacceptable <-minProbUnacceptable/100

     # external circle (this will be used for the black border)
    border_cir = circle(c(0,0), radius=1, npoints = 100)

    # gray border circle
    external_cir = circle(c(0,0), radius=0.97, npoints = 100)
    numSlices <- 20000

    from <- 5*pi/4
    to <- prob2rads(minProbUnacceptable)
    gradient_out2 = ticks(c(0,0), from=from, to=to, radius=0.89, numSlices)
    gradient_in2 = ticks(c(0,0), from=from, to=to, radius=0.1, numSlices)

    gradient_out = ticks(c(0,0), from=to, to=-pi/4, radius=0.89, numSlices)
    gradient_in = ticks(c(0,0), from=to, to=-pi/4, radius=0.1, numSlices)

    # label to be displayed
    par(ps = 7, cex = 1, cex.main = 1)
    label = ""

    plot(border_cir$x, border_cir$y, type="n", asp=1, axes=FALSE,
         xlim=c(-1.1,1.1), ylim=c(-1.1,1.1),
         xlab="", ylab="", xaxs="i", yaxs="i")

    # red slice
    #polygon(c(Sred$x, 0), c(Sred$y, 0),
    #        border = "#DC3912", col = "#DC3912", lty = NULL)

    # add gray border
    lines(external_cir$x, external_cir$y, col="gray85", lwd=20)

    # add external border
    lines(border_cir$x, border_cir$y, col="gray20", lwd=2)

    #gradient
    arrows(x0=gradient_out$x, y0=gradient_out$y,
           x1=gradient_in$x, y1=gradient_in$y, length=0, lwd=1,
           col=colorRampPalette(c("coral", "red"))(dim(gradient_out)[1]))

    #gradient
    arrows(x0=gradient_out2$x, y0=gradient_out2$y,
           x1=gradient_in2$x, y1=gradient_in2$y, length=0, lwd=1,
           col=colorRampPalette(c("green", "yellow"))(dim(gradient_out2)[1]))

    tio <- ticks2(0.5, 1, 26, .89)
    tii <- ticks2(0.5, 1, 26, .85)
    arrows(x0=tio$x, y0=tio$y, x1=tii$x, y1=tii$y, length=0, lwd=1, col="black")

    tio2 <- ticks2(0, .5, 51, .89)
    tii2 <- ticks2(0, .5, 51, .85)
    arrows(x0=tio2$x, y0=tio2$y, x1=tii2$x, y1=tii2$y, length=0, lwd=1, col="black")

    tio3 <- ticks2(0, .5, 11, .89)
    tii3 <- ticks2(0, .5, 11, .82)
    arrows(x0=tio3$x, y0=tio3$y, x1=tii3$x, y1=tii3$y, length=0, lwd=1, col="black")

    # add value
    #windowsFonts(
    #  A=windowsFont("Arial Black"),
    #  B=windowsFont("Bookman Old Style"),
    #  C=windowsFont("Comic Sans MS"),
    #  D=windowsFont("Symbol")
    #)
    #text(0, -0.65, paste("p =", signif(actualProb*100,3), "%") , cex=2.5)
    # add label of variable
    #text(0, 0.43, label, cex=3.8)

    #add acceptable limit
    #axy <- slice2xy(minProbUnacceptable, .89)
    #arrows(0, 0, axy$x, axy$y, col="white", lwd=3, lty=1, length=0)
    #arrows(0, 0, axy$x, axy$y, col="black", lwd=1, lty=3, length=0)

    # add needle
    needleRadius <- .89
    arrows(0, 0, slice2xy(actualProb, needleRadius)$x, slice2xy(actualProb, needleRadius)$y, col=colorProb, lwd=5, length=0)
    p1 <- signif(actualProb*100,3)
    text(0, -0.45, if ( is.null(actualProb2) ) bquote(paste(italic(p)*' = ', .(p1), "%")) else bquote(paste(italic(p)['1']*' = ', .(p1), "%")), col=colorProb, cex=2)

    if ( !is.null(actualProb2) ) {
      arrows(0, 0, slice2xy(actualProb2, needleRadius)$x, slice2xy(actualProb2, needleRadius)$y, col=colorProb2, lwd=5, length=0)
      p2 <- signif(actualProb2*100,3)
      text(0, -0.65, bquote(paste(italic(p)['2']*' = ', .(p2), "%")),col=colorProb2, cex=2)
    }

    seuilTriDelta <- 0.03
    rc <- prob2rads(minProbUnacceptable)
    zc <- slice2xy(minProbUnacceptable, 1.05)
    zcl <- rads2xy(rc-seuilTriDelta, 1.20)
    zcr <- rads2xy(rc+seuilTriDelta, 1.20)
    triangleSeuilX <- c(zc$x, zcl$x, zcr$x, zc$x)
    triangleSeuilY <- c(zc$y, zcl$y, zcr$y, zc$y)

    #textRad <- 0.15
    #flipLabel <- rc <= (pi/2)
    #text(slice2xy(minProbUnacceptable, textRad)$x, slice2xy(minProbUnacceptable, textRad)$y, labels=paste("SEUIL", "=", minProbUnacceptable), col="black", cex=1.2, adj=ifelse(flipLabel,0,1), srt=57.2958*(rc+ifelse(flipLabel,0,pi) ))

    polygon(triangleSeuilX, triangleSeuilY,
            border = "black", col = "red", lwd=1, lty = 1)

    # add central blue point
    points(0, 0, col="#2e9ef3", pch=19, cex=3)

    sapply(seq(0, 100, 20), plotSpeed)
    sapply(c(5,10,15,25,30,50,70), function(x) { plotSpeed(x, .83, 1)})
  }

dessinerRisqueMetre.G <- function(actualProb, minProbUnacceptable=0.05, minProbUnsafe=0.05, colorProb="darkblue", actualProb2=NULL, colorProb2="#4863A0")
{
  # external circle (this will be used for the black border)
  border_cir = circle(c(0,0), radius=1, npoints = 100)

  # gray border circle
  external_cir = circle(c(0,0), radius=0.97, npoints = 100)
  numSlices <- 20000

  from <- 5*pi/4
  toUnsafe <- prob2rads(minProbUnsafe)
  to <- prob2rads(minProbUnacceptable)
  gradient_safe_out2 = ticks(c(0,0), from=from, to=toUnsafe, radius=0.89, numSlices)
  gradient_safe_in2 = ticks(c(0,0), from=from, to=toUnsafe, radius=0.1, numSlices)

  gradient_unsafe_out = ticks(c(0,0), from=toUnsafe, to=to, radius=0.89, numSlices)
  gradient_unsafe_in = ticks(c(0,0), from=toUnsafe, to=to, radius=0.1, numSlices)

  gradient_unacceptable_out = ticks(c(0,0), from=to, to=-pi/4, radius=0.89, numSlices)
  gradient_unacceptable_in = ticks(c(0,0), from=to, to=-pi/4, radius=0.1, numSlices)

  # label to be displayed
  par(ps = 7, cex = 1, cex.main = 1)
  label = ""

  plot(border_cir$x, border_cir$y, type="n", asp=1, axes=FALSE,
       xlim=c(-1.1,1.1), ylim=c(-1.1,1.1),
       xlab="", ylab="", xaxs="i", yaxs="i")

   # red slice
  #polygon(c(Sred$x, 0), c(Sred$y, 0),
  #        border = "#DC3912", col = "#DC3912", lty = NULL)

  # add gray border
  lines(external_cir$x, external_cir$y, col="gray85", lwd=20)

  # add external border
  lines(border_cir$x, border_cir$y, col="gray20", lwd=2)

  #gradient
  arrows(x0=gradient_unacceptable_out$x, y0=gradient_unacceptable_out$y,
         x1=gradient_unacceptable_in$x, y1=gradient_unacceptable_in$y, length=0, lwd=1,
         col=colorRampPalette(c("coral", "red"))(dim(gradient_unacceptable_out)[1]))


  #gradient
  arrows(x0=gradient_unsafe_out$x, y0=gradient_unsafe_out$y,
         x1=gradient_unsafe_in$x, y1=gradient_unsafe_in$y, length=0, lwd=1,
         col=colorRampPalette(c("yellow", "orange"))(dim(gradient_unsafe_out)[1]))

    #gradient
  arrows(x0=gradient_safe_out2$x, y0=gradient_safe_out2$y,
         x1=gradient_safe_in2$x, y1=gradient_safe_in2$y, length=0, lwd=1,
         col=colorRampPalette(c("green", "lightgreen"))(dim(gradient_safe_out2)[1]))


  tio <- ticks2(0.5, 1, 26, .89)
  tii <- ticks2(0.5, 1, 26, .85)
  arrows(x0=tio$x, y0=tio$y, x1=tii$x, y1=tii$y, length=0, lwd=1, col="black")

  tio2 <- ticks2(0, .5, 51, .89)
  tii2 <- ticks2(0, .5, 51, .85)
  arrows(x0=tio2$x, y0=tio2$y, x1=tii2$x, y1=tii2$y, length=0, lwd=1, col="black")

  tio3 <- ticks2(0, .5, 11, .89)
  tii3 <- ticks2(0, .5, 11, .82)
  arrows(x0=tio3$x, y0=tio3$y, x1=tii3$x, y1=tii3$y, length=0, lwd=1, col="black")

  # add value
  #windowsFonts(
  #  A=windowsFont("Arial Black"),
  #  B=windowsFont("Bookman Old Style"),
  #  C=windowsFont("Comic Sans MS"),
  #  D=windowsFont("Symbol")
  #)
  #text(0, -0.65, paste("p =", signif(actualProb*100,3), "%") , cex=2.5)
  # add label of variable
  #text(0, 0.43, label, cex=3.8)

  #add acceptable limit
  #axy <- slice2xy(minProbUnacceptable, .89)
  #arrows(0, 0, axy$x, axy$y, col="white", lwd=3, lty=1, length=0)
  #arrows(0, 0, axy$x, axy$y, col="black", lwd=1, lty=3, length=0)

  # add needle
  needleRadius <- .89
  arrows(0, 0, slice2xy(actualProb, needleRadius)$x, slice2xy(actualProb, needleRadius)$y, col=colorProb, lwd=5, length=0)
  p1 <- signif(actualProb*100,3)
  text(0, -0.45, if ( is.null(actualProb2) ) bquote(paste(italic(p)*' = ', .(p1), "%")) else bquote(paste(italic(p)['1']*' = ', .(p1), "%")), col=colorProb, cex=2)

  if ( !is.null(actualProb2) ) {
    arrows(0, 0, slice2xy(actualProb2, needleRadius)$x, slice2xy(actualProb2, needleRadius)$y, col=colorProb2, lwd=5, length=0)
    p2 <- signif(actualProb2*100,3)
    text(0, -0.65, bquote(paste(italic(p)['2']*' = ', .(p2), "%")),col=colorProb2, cex=2)
  }

  seuilTriDelta <- 0.03
  rc <- prob2rads(minProbUnacceptable)
  zc <- slice2xy(minProbUnacceptable, 1.05)
  zcl <- rads2xy(rc-seuilTriDelta, 1.20)
  zcr <- rads2xy(rc+seuilTriDelta, 1.20)
  triangleSeuilX <- c(zc$x, zcl$x, zcr$x, zc$x)
  triangleSeuilY <- c(zc$y, zcl$y, zcr$y, zc$y)

  #textRad <- 0.15
  #flipLabel <- rc <= (pi/2)
  #text(slice2xy(minProbUnacceptable, textRad)$x, slice2xy(minProbUnacceptable, textRad)$y, labels=paste("SEUIL", "=", minProbUnacceptable), col="black", cex=1.2, adj=ifelse(flipLabel,0,1), srt=57.2958*(rc+ifelse(flipLabel,0,pi) ))

  polygon(triangleSeuilX, triangleSeuilY,
          border = "black", col = "red", lwd=1, lty = 1)

  # add central blue point
  points(0, 0, col="#2e9ef3", pch=19, cex=3)

  sapply(seq(0, 100, 20), plotSpeed)
  sapply(c(5,10,15,25,30,50,70), function(x) { plotSpeed(x, .83, 1)})
}


  ###############################        support functions           ##########################

  # function to create a circle
  circle <- function(center=c(0,0), radius=1, npoints=100)
  {
    r = radius
    tt = seq(0, 2*pi, length=npoints)
    xx = center[1] + r * cos(tt)
    yy = center[1] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  logScaleFactor <- function(p) {
    factor <- log10((p*100)+10) - 1
    factor <- ifelse(factor > 1, 1, factor)
    return (factor)
  }

  prob2rads <- function(p0)
  {
    p <- p0
    p <- logScaleFactor(p)
    #p <- p0
    rad <- (5*pi/4) - (3 * p * pi/2)
    return(rad)
  }

  rads2xy <- function(r, radius) {
    return (list(x = radius * cos(r), y = radius * sin(r)))
  }

  # function to get slices
  slice2xy <- function(p, radius)
  {
    r = prob2rads(p)
    return (rads2xy(r, radius))
  }

  # function to get major and minor tick marks
  ticks <- function(center=c(0,0), from=0, to=2*pi, radius=0.9, npoints=5)
  {
    r = radius
    tt = seq(from, to, length=npoints)
    xx = center[1] + r * cos(tt)
    yy = center[1] + r * sin(tt)
    heatColors <- heat.colors(npoints, .5)
    return(data.frame(x = xx, y = yy, clr = heatColors))
  }


  # add values 0 and 100
  # coordinates of min and max values (0, 100)
  plotSpeed <- function(sp, radi=.78, lwid=2) {
    endPoint <- sp %% 100 == 0
    xy <- slice2xy(sp/100, ifelse(endPoint, .7, .7))
    mto <- slice2xy(sp/100, .9)
    mti <- slice2xy(sp/100, radi)
    arrows(x0=mto$x, y0=mto$y,
           x1=mti$x, y1=mti$y, length=0, lwd=lwid, col='black')
    text(xy$x, xy$y, labels=paste0(sp, "%"), col="black", cex=ifelse(endPoint, 2, 1.5))
    return(xy)
  }

  ticks2 <- function(pFrom, pTo, npoints=5, radius=0.9)
  {
    r <- radius
    ps <- seq(pFrom, pTo, length=npoints)
    xx = sapply(ps, function(p) { return (slice2xy(p, radius)$x)})
    yy = sapply(ps, function(p) { return (slice2xy(p, radius)$y)})
    return(data.frame(x = xx, y = yy))
  }

######################## GRAPHIQUE D'ILLUSTRATION DE FRACTION DE DÉPASSEMENT (GRAPHIQUE À FIOLES) ################################

  paramsVariantesFracDep <- function(imageDir,
                                     cheminImageParDefaut,
                                     cheminImageIncertitude,
                                     couleurRisque,
                                     couleurAucunRisque,
                                     couleurSeuil,
                                     couleurFond,
                                     opacityIncertitude=1)
  {

    img <- png::readPNG(cheminImageParDefaut)
    imgi <- png::readPNG(cheminImageIncertitude)
    imgs <- list(img1 = img,
                 img2 = imgi,
                 img3 = img)

    posAlpha <- 4
    noirSup <- 0.5
    samplePixel <- img[1,1,]
    imageHeight <- dim(img)[1]
    imageWidth <- dim(img)[2]
    addAlpha <-  length(samplePixel) >= posAlpha

    coulRisque <- col2rgb(couleurRisque, addAlpha) / 255
    coulAucunRisque <- col2rgb(couleurAucunRisque, addAlpha) / 255
    coulFond <- couleurFond
    coulSeuil <- couleurSeuil
    coulDanger <- coulRisque
    coulDanger[4] <- opacityIncertitude

    for ( i in 1:imageHeight ) {
      for ( j in 1:imageWidth ) {
        pixelATransparent <- addAlpha && img[i,j,posAlpha] == 0
        pixelBTransparent <- addAlpha && imgi[i,j,posAlpha] == 0

        if ( !pixelATransparent ) {
          imgs$img1[i,j,] <- coulRisque
          imgs$img3[i,j,] <- coulAucunRisque
        }
        if ( !pixelBTransparent) {
          imgs$img2[i,j,] <- coulDanger
        }
      }
    }

    cheminImg1 <- paste(imageDir, 'img1.png', sep = '/')
    png::writePNG(imgs$img1, cheminImg1 )
    cheminImg2 <- paste(imageDir, 'img2.png', sep = '/')
    png::writePNG(imgs$img2, cheminImg2)
    cheminImg3 <- paste(imageDir, 'img3.png', sep = '/')
    png::writePNG(imgs$img3, cheminImg3)
    imgs <- imgs
    imageHeight <- dim(img)[1]
    imageWidth <- dim(img)[2]
    imageRatio <- imageHeight / imageWidth
    imgs <- list(img1 = cheminImg1, img2 = cheminImg2, img3 = cheminImg3)

    return(list(imgs = imgs,
                couleurFond = coulFond,
                couleurSeuil = coulSeuil))
  }

  drawPlot <- function(imageParams, fracDepasseEst, fracDepasseLim=fracDepasseEst, seuil=0, titre = NULL) {
    if ( fracDepasseLim < fracDepasseEst ) {
      fracDepasseLim = fracDepasseEst
    }

    image_seq <- c(rep(imageParams$imgs$img1, fracDepasseEst), #premiere sequence pour frac estimate
                   rep(imageParams$imgs$img2, fracDepasseLim-fracDepasseEst), #sequence des fioles hachurées
                   rep(imageParams$imgs$img3, 100 - fracDepasseLim)) #le reste des fioles

    #### Creation tableau pour graph
    df_graph <- data.frame(X = rep(1:10, 10),
                           Y = rep(10:1, each = 10),
                           SEG = c(rep(1, seuil), rep(0, 100 - seuil)),
                           image_seq)

    if (seuil == 0) {
      gradientHigh <- imageParams$couleurFond
    } else {
      gradientHigh <- imageParams$couleurSeuil
    }

    #### Dessin graph
    ggp <- ggplot(df_graph) + geom_raster(aes(X, Y, fill = SEG)) +
      scale_fill_gradient(high = gradientHigh, low = imageParams$couleurFond) +
      theme_void() + theme(legend.position = "none") +
      geom_image(aes(X, Y, image = image_seq), size = .08)

    if ( !is.null(titre) ) {
      ggp <- ggp +
        ggtitle(titre) +
        theme(plot.title = element_text(hjust = 0.5, margin=margin(b = 0, unit = "pt"), size=12))
    }
    return(ggp)
  }

