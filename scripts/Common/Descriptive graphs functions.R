#########################################################################
#
#
# Descriptive graphs, using simple imputation procedure for censored data
#
# # V1.02   19 june 2018 -  teak to boxplot
# V1.01   22 march 2018 -  adding a boxplot to descriptive graphs
# V1.00   20 march 2018
#
####################################################################

##
#
# INPUT : ouput from simple.censored.treatment + vector indicating non censorship
#
##

#library(ggplot2)

############# for individual workers or data with no stratification but censorship

fun.qqplot <-function( data.simply.imputed , notcensored ,
                qqplot.1 = "Quantile-Quantile Plot",
                qqplot.2 = "Quantiles (Lognormal Distribution)",
                qqplot.3 = "Quantiles (Standardized Observations)",
                qqplot.4 = "Measurement Type",
                qqplot.5 = "Censored",
                qqplot.6 = "Detected") {


  res2 <-fun.graph.NDexpo(data.simply.imputed$imputed$data)


  data.f <-data.frame(x=res2$x,
                      y=res2$y,
                      is.censored=notcensored[res2$reorder],
                      x.prime=res2$x.prime,
                      stringsAsFactors=F)

  if (length(data.f[data.f$is.censored==0,1])==0) {

    p<-ggplot(data.f,aes(x=x,y=y),main=qqplot.1)
    p<-p+ suppressWarnings(geom_point(size=4,colour='darkgreen'))
    p<-p+geom_abline(intercept=0, slope=1)+

      xlab(qqplot.2)+ylab(qqplot.3)+
      theme(axis.title.x=element_text(size=14,vjust=0))+
      theme(axis.text.x=element_text(size=14))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.y=element_text(size=13,angle=90))+
      theme(legend.position='top',
            legend.title=element_text(size=14),
            legend.text=element_text(size=14) ) +
      theme(aspect.ratio= 0.7)

  }

  if (length(data.f[data.f$is.censored==0,1])!=0) {

    p<-ggplot(data.f,aes(x=x,y=y,colour=is.censored),main=qqplot.1)
    p<-p+ suppressWarnings(geom_point(size=4))+
      scale_colour_manual(values=c("salmon","darkgreen"),
                          name=qqplot.4,
                          labels=c(qqplot.5,qqplot.6))
    p<-p+geom_abline(intercept=0, slope=1) +

      xlab(qqplot.2)+ylab(qqplot.3) +
      theme(aspect.ratio= 0.7)
      theme(axis.title.x=element_text(size=14,vjust=0))+
      theme(axis.text.x=element_text(size=14))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.y=element_text(size=13,angle=90))+
      theme(legend.position='top',
            legend.title=element_text(size=14),
            legend.text=element_text(size=14) )


  }

  return(p)

}

###########   boxplot with jitter


fun.boxplot <-function( data.simply.imputed , notcensored , c.oel,
                        boxplot.1 = "Measurement Type",
                        boxplot.2 = "Concentration",
                        boxplot.3 = "Occupational Exposure Limit",
                        boxplot.4 = "Censored",
                        boxplot.5 = "Not Censored",
                        boxplot.6 = "Measurements") {

  #data.simply.imputed <- data.sample.imputed

  #notcensored <-data.sample.formatted$notcensored

 # c.oel<-data.sample.formatted$c.oel

  data.f <-data.frame(x=data.simply.imputed$imputed$data$xfin ,
                      is.censored=notcensored,
                      stringsAsFactors=F)

  f <- function(x) {
    r <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
  }

  if (length(notcensored[!notcensored])==0) {

  p1 <-ggplot(data.f, aes(x=boxplot.6, y=x),ylim=c(min.val,max.val))
  p1 <-p1+geom_point(position=position_jitter(width=0.3), alpha=0.9, size=3 , color="blue1")
  p1<-p1+stat_summary(fun.data = f, geom="boxplot",size=1.0,alpha=0, outlier.size=0)
  p1<-p1+stat_summary(fun.data = f, geom="boxplot",size=1.0,alpha=0, outlier.size=0,color=alpha("black",0.3))
  p1 <-p1+scale_y_log10()+xlab('')+ylab(boxplot.2)
  p1 <-p1+geom_hline(yintercept = c.oel,colour = "red", linewidth = 1)
  p1<-p1+annotate("text", 0.8, c.oel*1.3, label = boxplot.3,size=5)

  }

  if (length(notcensored[!notcensored])!=0) {

    p1 <-ggplot(data.f, aes(x=boxplot.6, y=x),ylim=c(min.val,max.val))
    p1 <-p1+geom_point(position=position_jitter(width=0.3), alpha=0.9, aes(color=notcensored), size=3)
    p1<-p1+stat_summary(fun.data = f, geom="boxplot",size=1.0,alpha=0, outlier.size=0)
    p1<-p1+stat_summary(fun.data = f, geom="boxplot",size=1.0,alpha=0, outlier.size=0,color=alpha("black",0.3))
    p1 <-p1+scale_y_log10()+xlab('')+ylab(boxplot.2)
    p1 <- p1+scale_color_manual(values=c("brown1","blue1"),
                                name=boxplot.1,
                                labels=c( boxplot.4 , boxplot.5 ))
    p1 <-p1+geom_hline(yintercept = c.oel,colour = "red", linewidth = 1)
    p1<-p1+annotate("text", 0.8, c.oel*1.3, label = boxplot.3,size=5)

  }

  p1 <- p1 + theme(aspect.ratio=1.5)+
    theme(axis.title.x=element_text(size=14,vjust=0))+
    theme(axis.text.x=element_text(size=14))+
    theme(axis.title.y=element_text(size=16,angle=90))+
    theme(axis.text.y=element_text(size=13,angle=90))

    return(p1)
}

#
#
#
#
############ support functions
#
#
#
#

##############################preparation du graphique QQ

fun.graph.NDexpo <-function(res)    {

  #res is the data element of the fun.NDexpo output


  ######defining some quantities

  n <-dim(res)[1]


  data <-data.frame(x=numeric(n),y=numeric(n))

  ###cr?ation des observations standardis?es (param?tres calcu?s ? partir des valeur d?tect?s)

  normal.scores <-qnorm(res$pp[res$is.censored==1])

  regression <-suppressWarnings(lm(res$yfin[res$is.censored==1]~normal.scores))

  data$y <-(res$yfin[order(res$pp)]-regression$coefficients[1])/regression$coefficients[2]


  ##plotting positions and normal scores

  pi<-res$pp[order(res$pp)]  ##probabilité cumulée attendue (formule de blom)

  is.censored <-res$is.censored[order(res$pp)]

  data$x <-qnorm(pi)   #quantiles de la standard normale correspondant aux probabilité cumulées

  ####donn?es pour la simulation

  pi.prime<-((1:length(data$y))-0.375)/(n+0.25)  ##probabilité cumulée attendue (formule de blom)

  data$x.prime <-qnorm(pi.prime)   #quantiles de la standard normale correspondant aux probabilité cumulées

  data$is.censored <-rep('censored',n)
  data$is.censored[is.censored==1] <-'detected'
  data$reorder <- order(res$pp)

  return(data)

}
