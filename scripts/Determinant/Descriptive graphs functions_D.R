#########################################################################
#
#
# Descriptive graphs, using simple imputation procedure for censored data
#
#  determinant analysis tools
#
# V1.00   20 march 2018
#
####################################################################


##### requires the common descriptive graph sourced for support functiond

##
#
# INPUT : ouput from simple.censored.treatment
#
##

library(ggplot2)


############# for the group

fun.qqplot.group.D <-function(data.simply.imputed ,
                              notcensored ,
                              cats,

                              qqplot.1="Quantile-quantile plot",
                              qqplot.2="Quantile (lognormal dist)",
                              qqplot.3="Quantile (standardized obs)",
                              qqplot.4="Measurement type",
                              qqplot.5="Censored",
                              qqplot.6="Detected") {


  #data.simply.imputed <- data.sample.imputed
  #notcensored <- data.sample.formatted$notcensored
  #cats <- data.sample.formatted$var

  res2 <-fun.graph.NDexpo(data.simply.imputed$imputed$data)


  data.f <-data.frame(x=res2$x,
                      y=res2$y,
                      is.censored=notcensored[res2$reorder],
                     category=cats[res2$reorder],
                      x.prime=res2$x.prime,
                      stringsAsFactors=F)

  if (length(data.f[data.f$is.censored==0,1])==0) {

    p<-ggplot(data.f,aes(x=x,y=y),main=qqplot.1)
    p<-p+ suppressWarnings(geom_point(size=4,aes(colour=category)))
    p<-p+geom_abline(intercept=0, slope=1)+

      xlab(qqplot.2)+ylab(qqplot.3)+
      theme(axis.title.x=element_text(size=14,vjust=0))+
      theme(axis.text.x=element_text(size=14))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.y=element_text(size=13,angle=90))+
      theme(legend.position='top',
            legend.title=element_text(size=14),
            legend.text=element_text(size=14) )

  }

  if (length(data.f[data.f$is.censored==0,1])!=0) {

    p<-ggplot(data.f,aes(x=x,y=y,colour=category),main=qqplot.1)
    p<-p+ suppressWarnings(geom_point(size=4,aes(pch=is.censored)))+
      scale_shape_manual(values=c(1,16),
                         name=qqplot.4,
                         labels=c(qqplot.5 , qqplot.6 ))
    p<-p+geom_abline(intercept=0, slope=1) +
      xlab(qqplot.2)+ylab(qqplot.3)+
      theme(axis.title.x=element_text(size=14,vjust=0))+
      theme(axis.text.x=element_text(size=14))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.y=element_text(size=13,angle=90))+
      theme(legend.position='top',
            legend.title=element_text(size=14),
            legend.text=element_text(size=14) )


  }

  print(p)

}




