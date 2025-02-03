######################################################
#
#
#
# graphical output functions - between worker specific graphs
#
# V1.01    5 june 2018  graphiques de Delphine Bosson
# V1.00   20 march 2018
#
#
#
#
######################################################

library(ggplot2)

##
#
#  INPUT : output from one of the bayesian output functions + user.parameter set
#
##

##############################   BOXPLOT BY WORKER GRAPH   ###########################

  boxplot.by.worker <-function(data.formatted ,
                               data.simply.imputed ,
                               bayesian.output.B ,
                               worker.list=data.formatted$data$worker,
                               boxplot.work.1="Worker",
                               boxplot.work.2="Concentration",
                               boxplot.work.3="OEL",
                               pal_col = TRUE) {

    #bayesian.output.B <-res1
    #data.formatted <-data.sample.formatted
    #data.simply.imputed <-data.sample.imputed
    #worker.list<-data.formatted$data$worker

    worker.table <-data.formatted$data

    c.oel<-data.formatted$c.oel

    ########################simulated data

    data.g <- bayesian.output.B$worker.index

    data.g$mu <-numeric(length(data.g[,1]))

    for (i in 1:length(data.g[,1])) data.g$mu[i]<-median(bayesian.output.B$mu.workers[i,])

    data.g$sw <- median(bayesian.output.B$sw)

    data.g <-data.g[is.element(data.g$worker,worker.list),]

    dat <-data.frame(value=numeric(0), group=character(0),stringsAsFactors=F)

    for (i in 1:length(data.g[,1])) dat <- rbind(dat, data.frame(value=exp(rnorm(1000,data.g$mu[i],data.g$sw[i])),      group=rep(data.g$worker[i],1000),stringsAsFactors=F))


    ######observed data


    data.fin <-data.formatted$data

    data.fin$x.orig <-data.fin$x

    data.fin$x <-data.simply.imputed$imputed$data$xfin

    data.fin$censored=!data.formatted$notcensored

    data.fin$id <-data.formatted$data$worker

    data.fin <-data.fin[is.element(data.fin$worker,worker.list),]


    #####min max for graph

    min.val <-exp(min(data.g$mu-1.5*data.g$sw))
    max.val <-exp(max(data.g$mu-1.5*data.g$sw))


    ####graph



    f <- function(x) {
      r <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
      names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
      r
    }

    p1 <- ggplot(dat, aes(x=group, y=value, fill = group, color=group, group=group),ylim=c(min.val,max.val))

    if(pal_col == TRUE){

      p1 <- p1 + geom_boxplot(lwd = 0.8, alpha = 0.1) +
        geom_point(position=position_jitter(width=0.3), alpha=0.2) +
        geom_hline(yintercept = c.oel,colour = "red", size = 2)


    }else{

      p1 <- p1 + geom_boxplot(color= "gray20", lwd = 0.8, fill= alpha(paste0("gray", round(seq(5, 25, length.out = length(unique(dat$group))))), 0.3)) +
        geom_point(position=position_jitter(width=0.3), alpha=0.2, color = "black") +
        geom_hline(yintercept = c.oel,colour = "black", size = 2)
    }


    p1 <- p1 + geom_point(data=data.fin, fill="white", colour = "black", shape=20, size = 4, aes(x=id, y=x, pch=censored, group=id)) +
      scale_y_log10(breaks=c(.01,.1,1,10,100),labels=c(.01,.1,1,10,100)) +
      labs(x = boxplot.work.1, y = boxplot.work.2) +
      theme(axis.title.x=element_text(size=16,vjust=-1)) +
      theme(axis.text.x=element_text(size=14)) +
      theme(axis.title.y=element_text(size=16,angle=90)) +
      theme(axis.text.y=element_text(size=14,angle=0)) +
      theme(aspect.ratio=0.6) +
      theme(legend.position = "none") +
      annotate("text", 2.5, c.oel*1.3, label = boxplot.work.3, size=5) +
      theme(axis.line = element_line(size = 3, colour = "grey80")) +
      theme(axis.ticks = element_line(size = 2))   +
      coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)

    return(p1)

  }


  ##############################   INDIVIDUAL RISK BAND GRAPH   ###########################

  ######### FRAC

  individual.riskband.plot.frac <-function( mu.chain ,
                                            sw.chain ,
                                            sb.chain ,
                                            c.oel ,
                                            psi ,
                                            frac_threshold,
                                            riskplot.work.1="Probability of Individual non compliance category",
                                            riskplot.work.2="Probability")


  {

    qn <- qnorm(1-(frac_threshold/100))

    frac.ind <- 100*(1-pnorm((log(c.oel)-mu.chain-qn*sw.chain)/sb.chain))

    bande1 <- psi/100
    bande2 <- psi/10
    bande3 <- psi
    C1 <-100*length(frac.ind[frac.ind<bande1])/length(frac.ind)
    C2 <-100*length(frac.ind[frac.ind>=bande1 & frac.ind<bande2])/length(frac.ind)
    C3 <-100*length(frac.ind[frac.ind>=bande2 & frac.ind<bande3 ])/length(frac.ind)
    C4 <-100*length(frac.ind[frac.ind>=bande3 ])/length(frac.ind)

    cats <- factor(c('C1','C2','C3',"C4"),labels=c(paste0('<', bande1, '%'),paste0(bande1, '-', bande2, '%'), paste0(bande2, '-', bande3, '%'), paste0('>', bande3, '%')))

    data <-data.frame(perc=c(C1,C2,C3,C4),cat=cats)

    p1<- ggplot(data,aes(x=cats,y=perc))
    p1 <-p1+
      geom_bar(stat="identity",fill=c('green4',"greenyellow",'yellow','red'))+
      theme(aspect.ratio=0.6)+
      xlab(riskplot.work.1)+
      ylab(riskplot.work.2) +
      theme(axis.title.x=element_text(size=16,vjust=-1))+
      theme(axis.text.x=element_text(size=16))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.y=element_text(size=13,angle=90,hjust=0.5))+
      theme(legend.position = "none")+
      geom_text(x=1:4,y=c(C1,C2,C3,C4)+5,label=paste(signif(c(C1,C2,C3,C4),3),'%',sep=''),size=5,colour='grey28') +
      scale_y_continuous(breaks=c(0,20,40,60,80,100),limits=c(0,110))


    return(p1)
  }


####### PERC

  individual.riskband.plot.perc <-function( mu.chain ,
                                            sw.chain ,
                                            sb.chain ,
                                            c.oel ,
                                            psi ,
                                            target_perc,
                                            riskplot.work.1="Probability of Individual non compliance category",
                                            riskplot.work.2="Probability")


  {

    qn <- qnorm(target_perc/100)

    frac.ind <- 100*(1-pnorm((log(c.oel)-mu.chain-qn*sw.chain)/sb.chain))

    bande1 <- psi/100
    bande2 <- psi/10
    bande3 <- psi
    C1 <-100*length(frac.ind[frac.ind<bande1])/length(frac.ind)
    C2 <-100*length(frac.ind[frac.ind>=bande1 & frac.ind<bande2])/length(frac.ind)
    C3 <-100*length(frac.ind[frac.ind>=bande2 & frac.ind<bande3 ])/length(frac.ind)
    C4 <-100*length(frac.ind[frac.ind>=bande3 ])/length(frac.ind)

    cats <- factor(c('C1','C2','C3',"C4"),labels=c(paste0('<', bande1, '%'),paste0(bande1, '-', bande2, '%'), paste0(bande2, '-', bande3, '%'), paste0('>', bande3, '%')))

    data <-data.frame(perc=c(C1,C2,C3,C4),cat=cats)

    p1<- ggplot(data,aes(x=cats,y=perc))
    p1 <-p1+
      geom_bar(stat="identity",fill=c('green4',"greenyellow",'yellow','red'))+
      theme(aspect.ratio=0.6)+
      xlab(riskplot.work.1)+
      ylab(riskplot.work.2) +
      theme(axis.title.x=element_text(size=16,vjust=-1))+
      theme(axis.text.x=element_text(size=16))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.y=element_text(size=13,angle=90,hjust=0.5))+
      theme(legend.position = "none")+
      geom_text(x=1:4,y=c(C1,C2,C3,C4)+5,label=paste(signif(c(C1,C2,C3,C4),3),'%',sep=''),size=5,colour='grey28') +
      scale_y_continuous(breaks=c(0,20,40,60,80,100),limits=c(0,110))


    return(p1)

  }


  ######AM

  individual.riskband.plot.am <-function( mu.chain ,
                                          sw.chain ,
                                          sb.chain ,
                                          c.oel ,
                                          psi,
                                          riskplot.work.1="Probability of Individual non compliance category",
                                          riskplot.work.2="Probability" )


  {

  frac.ind <- 100*(1-pnorm((log(c.oel)-mu.chain-0.5*sw.chain^2)/sb.chain))

  bande1 <- psi/100
  bande2 <- psi/10
  bande3 <- psi
  C1 <-100*length(frac.ind[frac.ind<bande1])/length(frac.ind)
  C2 <-100*length(frac.ind[frac.ind>=bande1 & frac.ind<bande2])/length(frac.ind)
  C3 <-100*length(frac.ind[frac.ind>=bande2 & frac.ind<bande3 ])/length(frac.ind)
  C4 <-100*length(frac.ind[frac.ind>=bande3 ])/length(frac.ind)

  cats <- factor(c('C1','C2','C3',"C4"),labels=c(paste0('<', bande1, '%'),paste0(bande1, '-', bande2, '%'), paste0(bande2, '-', bande3, '%'), paste0('>', bande3, '%')))

  data <-data.frame(perc=c(C1,C2,C3,C4),cat=cats)

  p1<- ggplot(data,aes(x=cats,y=perc))
  p1 <-p1+
    geom_bar(stat="identity",fill=c('green4',"greenyellow",'yellow','red'))+
    theme(aspect.ratio=0.6)+
    xlab(riskplot.work.1)+
    ylab(riskplot.work.2) +
    theme(axis.title.x=element_text(size=16,vjust=-1))+
    theme(axis.text.x=element_text(size=16))+
    theme(axis.title.y=element_text(size=16,angle=90))+
    theme(axis.text.y=element_text(size=13,angle=90,hjust=0.5))+
    theme(legend.position = "none")+
    geom_text(x=1:4,y=c(C1,C2,C3,C4)+5,label=paste(signif(c(C1,C2,C3,C4),3),'%',sep=''),size=5,colour='grey28') +
    scale_y_continuous(breaks=c(0,20,40,60,80,100),limits=c(0,110))


  return(p1)

  }

