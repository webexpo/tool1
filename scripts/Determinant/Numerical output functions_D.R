######################################################
#
#
#
# Numerical output functions - DETERMINANTS
#
#
#
# V1.00   20 march 2018
#
#
#
######################################################


##
#
#  INPUT : output from one of the bayesian output functions for the ANOVA analysis + user.parameter set
#
##

## requires the common numerical result script sourced

# conf is the level of confidence for credible intervals
# psi is the overexposure risk threshold
# frac_threshold is the acceptable exceedance value
# target_perc is the percentile of interest
# rappap_cover is the coverage of the rapport ratio
# wwct is the threshold for the within worker correlation coefficient


############ cOMPARATIVE TABLE FOR ACROSS ALL CATEGORTIES



fun.comp.table.D<-function(bayesian.analysis.D , c.oel, user.input,
                           comp.d.1="GM",
                           comp.d.2="GSD",
                           comp.d.3="RIE",
                           comp.d.4="Exceedance",
                           comp.d.5="Percentile",
                           comp.d.6="Arithmetic mean",
                           comp.d.7="Parameter") {

  #bayesian.analysis.D <- bayesian.analysis.cat
  #c.oel <- data.sample.formatted$c.oel
  #user.input <- user.input


  results <-data.frame(parameter=c(comp.d.1,comp.d.2,comp.d.3,
                                   comp.d.4,paste(comp.d.4,'_UCL',sep=""),
                                   paste(comp.d.5,user.input$target_perc,sep=""),
                                   paste(comp.d.5,user.input$target_perc,"_UCL",sep=""),
                                   comp.d.6,
                                   paste(comp.d.6,'_UCL',sep="")),stringsAsFactors=F)

  names(results)[1] <-comp.d.7

  #determination of ref.chain : highest gm

  vec.mu.med <-apply(bayesian.analysis.D$mu.chain,1,median)

  index.ref <-which(vec.mu.med==max(vec.mu.med))

  ref.chain <-bayesian.analysis.D$mu.chain[index.ref,]


  #loop de remplissage
  for (i in bayesian.analysis.D$group.id$index) {



    results <- cbind(results,fun.mini.stats(bayesian.analysis.D$mu.chain[i,],
                                            bayesian.analysis.D$sigma.chain[i,],
                                            c.oel,
                                            ref.chain,
                                            user.input$conf, user.input$target_perc))

    names(results)[i+1] <-bayesian.analysis.D$group.id$name[i]
  }


  return(results)

}


############ COMPARING 2 CATEGORIES - DISTRIBUTION

fun.2cat.dist <-function(bayesian.output.D , cat1 , cat2, user.input,

                         comp.2cat.1 = "GM",
                         comp.2cat.2 = "GSD",
                         comp.2cat.3 = "Parameter",
                         comp.2cat.4 = "Category") {

  #bayesian.output.D <- bayesian.analysis.cat
  #user.input <-user.input
  #cat1 <- category1
  #cat2 <- category2

  conf <- user.input$conf
  expratio <- user.input$exp.ratio.gm

  res <-data.frame(parameter=c(comp.2cat.1,comp.2cat.2),
                   V1=numeric(2),
                   V2=numeric(2),
                   ratio=numeric(2),
                   p.sup=numeric(2),
                   p.inf=numeric(2),
                   stringsAsFactors = FALSE)

  mu1.chain <-bayesian.output.D$mu.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat1],]
  mu2.chain <-bayesian.output.D$mu.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat2],]
  sigma1.chain <-bayesian.output.D$sigma.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat1],]
  sigma2.chain <-bayesian.output.D$sigma.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat2],]

  res$V1[1] <-paste(signif(median(exp(mu1.chain)),2),
                    " [",
                    signif(quantile(exp(mu1.chain),(100-conf)/200),2),
                    " - ",
                    signif(quantile(exp(mu1.chain),1-(100-conf)/200),2),
                    "]",sep="")
  res$V2[1] <-paste(signif(median(exp(mu2.chain)),2),
                    " [",
                    signif(quantile(exp(mu2.chain),(100-conf)/200),2),
                    " - ",
                    signif(quantile(exp(mu2.chain),1-(100-conf)/200),2),
                    "]",sep="")

  res$ratio[1] <-paste(signif(median(exp(mu2.chain-mu1.chain)),2),
                       " [",
                       signif(quantile(exp(mu2.chain-mu1.chain),(100-conf)/200),2),
                       " - ",
                       signif(quantile(exp(mu2.chain-mu1.chain),1-(100-conf)/200),2),
                       "]",sep="")

  res$p.sup[1] <-paste(signif(100*length(mu1.chain[(mu2.chain-mu1.chain)>log(expratio)])/length(mu1.chain),2)," %",sep="")

  res$p.inf[1] <-paste(signif(100*length(mu1.chain[(mu2.chain-mu1.chain)<log(expratio)])/length(mu1.chain),2)," %",sep="")


  res$V1[2] <-paste(signif(median(exp(sigma1.chain)),2),
                    " [",
                    signif(quantile(exp(sigma1.chain),(100-conf)/200),2),
                    " - ",
                    signif(quantile(exp(sigma1.chain),1-(100-conf)/200),2),
                    "]",sep="")
  res$V2[2] <-paste(signif(median(exp(sigma2.chain)),2),
                    " [",
                    signif(quantile(exp(sigma2.chain),(100-conf)/200),2),
                    " - ",
                    signif(quantile(exp(sigma2.chain),1-(100-conf)/200),2),
                    "]",sep="")

  res$ratio[2] <-paste(signif(median(exp(sigma2.chain-sigma1.chain)),2),
                       " [",
                       signif(quantile(exp(sigma2.chain-sigma1.chain),(100-conf)/200),2),
                       " - ",
                       signif(quantile(exp(sigma2.chain-sigma1.chain),1-(100-conf)/200),2),
                       "]",sep="")

  res$p.sup[2] <-paste(signif(100*length(sigma1.chain[(sigma2.chain-sigma1.chain)>log(expratio)])/length(sigma1.chain),2)," %",sep="")

  res$p.inf[2] <-paste(signif(100*length(sigma1.chain[(sigma2.chain-sigma1.chain)<log(expratio)])/length(sigma1.chain),2)," %",sep="")


  names(res) <-c(comp.2cat.3,paste0(comp.2cat.4,"1"),paste0(comp.2cat.4,"2"),"ratio2/1","Proba.sup","Proba.inf")


  return(res)

}

############ COMPARING 2 CATEGORIES - AM AND PERC



fun.2cat.metrics <-function(bayesian.output.D , cat1 , cat2 , user.input,

                            comp.2cat.1 = "Perc",
                            comp.2cat.2 = "AM",
                            comp.2cat.3 = "Parameter",
                            comp.2cat.4 = "Category") {


  #bayesian.output.D <- bayesian.analysis.cat
  #user.input <-user.input
  #cat1 <- category1
  #cat2 <- category2

  conf <- user.input$conf
  expratio <- user.input$exp.ratio.perc
  perc <-user.input$target_perc

  res <-data.frame(parameter=c(comp.2cat.1,comp.2cat.2),
                   V1=numeric(2),
                   V2=numeric(2),
                   ratio=numeric(2),
                   p.sup=numeric(2),
                   p.inf=numeric(2),
                   stringsAsFactors = FALSE)

  mu1.chain <-bayesian.output.D$mu.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat1],]
  mu2.chain <-bayesian.output.D$mu.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat2],]
  sigma1.chain <-bayesian.output.D$sigma.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat1],]
  sigma2.chain <-bayesian.output.D$sigma.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat2],]

  pPerc1.chain <-exp(mu1.chain+qnorm(perc/100)*sigma1.chain)
  pPerc2.chain <-exp(mu2.chain+qnorm(perc/100)*sigma2.chain)

  am1.chain <-exp(mu1.chain+0.5*sigma1.chain^2)
  am2.chain <-exp(mu2.chain+0.5*sigma2.chain^2)




  res$V1[1] <-paste(signif(median(pPerc1.chain),2),
                    " [",
                    signif(quantile(pPerc1.chain,(100-conf)/200),2),
                    " - ",
                    signif(quantile(pPerc1.chain,1-(100-conf)/200),2),
                    "]",sep="")
  res$V2[1] <-paste(signif(median(pPerc2.chain),2),
                    " [",
                    signif(quantile(pPerc2.chain,(100-conf)/200),2),
                    " - ",
                    signif(quantile(pPerc2.chain,1-(100-conf)/200),2),
                    "]",sep="")

  res$ratio[1] <-paste(signif(median(pPerc2.chain/pPerc1.chain),2),
                       " [",
                       signif(quantile(pPerc2.chain/pPerc1.chain,(100-conf)/200),2),
                       " - ",
                       signif(quantile(pPerc2.chain/pPerc1.chain,1-(100-conf)/200),2),
                       "]",sep="")

  res$p.sup[1] <-paste(signif(100*length(pPerc1.chain[pPerc2.chain/pPerc1.chain>expratio])/length(pPerc1.chain),2)," %",sep="")

  res$p.inf[1] <-paste(signif(100*length(pPerc1.chain[pPerc2.chain/pPerc1.chain<expratio])/length(pPerc1.chain),2)," %",sep="")


  res$V1[2] <-paste(signif(median(am1.chain),2),
                    " [",
                    signif(quantile(am1.chain,(100-conf)/200),2),
                    " - ",
                    signif(quantile(am1.chain,1-(100-conf)/200),2),
                    "]",sep="")
  res$V2[2] <-paste(signif(median(am2.chain),2),
                    " [",
                    signif(quantile(am2.chain,(100-conf)/200),2),
                    " - ",
                    signif(quantile(am2.chain,1-(100-conf)/200),2),
                    "]",sep="")

  res$ratio[2] <-paste(signif(median(am2.chain/am1.chain),2),
                       " [",
                       signif(quantile(am2.chain/am1.chain,(100-conf)/200),2),
                       " - ",
                       signif(quantile(am2.chain/am1.chain,1-(100-conf)/200),2),
                       "]",sep="")

  res$p.sup[2] <-paste(signif(100*length(am1.chain[am2.chain/am1.chain>expratio])/length(am1.chain),2)," %",sep="")

  res$p.inf[2] <-paste(signif(100*length(am1.chain[am2.chain/am1.chain<expratio])/length(am1.chain),2)," %",sep="")


  names(res) <-c(comp.2cat.3,paste0(comp.2cat.4,"1"),paste0(comp.2cat.4,"2"),"ratio2/1","Proba.sup","Proba.inf")


  return(res)

}

############ COMPARING 2 CATEGORIES - EXCEEDANCE


fun.2cat.f <-function(data.formatted , bayesian.output.D , cat1 , cat2 , user.input,

                      comp.2cat.1 = "Exceedance",

                      comp.2cat.3 = "Parameter",
                      comp.2cat.4 = "Category") {

  conf <- user.input$conf
  expdelta <- user.input$expdelta
  perc <-user.input$target_perc
  c.oel <-data.formatted$c.oel

  res <-data.frame(parameter=c(comp.2cat.1),
                   V1=numeric(1),
                   V2=numeric(1),
                   delta=numeric(1),
                   p.sup=numeric(1),
                   p.inf=numeric(1),
                   stringsAsFactors = FALSE)

  mu1.chain <-bayesian.output.D$mu.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat1],]
  mu2.chain <-bayesian.output.D$mu.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat2],]
  sigma1.chain <-bayesian.output.D$sigma.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat1],]
  sigma2.chain <-bayesian.output.D$sigma.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat2],]


  frac1.chain <-100*(1-pnorm((log(c.oel)-mu1.chain)/sigma1.chain))

  frac2.chain <-100*(1-pnorm((log(c.oel)-mu2.chain)/sigma2.chain))


  res$V1[1] <-paste(signif(median(frac1.chain),2),
                    " [",
                    signif(quantile(frac1.chain,(100-conf)/200),2),
                    " - ",
                    signif(quantile(frac1.chain,1-(100-conf)/200),2),
                    "]",sep="")
  res$V2[1] <-paste(signif(median(frac2.chain),2),
                    " [",
                    signif(quantile(frac2.chain,(100-conf)/200),2),
                    " - ",
                    signif(quantile(frac2.chain,1-(100-conf)/200),2),
                    "]",sep="")

  res$delta[1] <-paste(signif(median(frac2.chain-frac1.chain),2),
                       " [",
                       signif(quantile(frac2.chain-frac1.chain,(100-conf)/200),2),
                       " - ",
                       signif(quantile(frac2.chain-frac1.chain,1-(100-conf)/200),2),
                       "]",sep="")

  res$p.sup[1] <-paste(signif(100*length(frac1.chain[frac2.chain-frac1.chain>expdelta])/length(frac1.chain),2)," %",sep="")

  res$p.inf[1] <-paste(signif(100*length(frac1.chain[frac2.chain-frac1.chain<expdelta])/length(frac1.chain),2)," %",sep="")

  names(res) <-c(comp.2cat.3,paste0(comp.2cat.4,"1"),paste0(comp.2cat.4,"2"),"delta2-1","Proba.sup","Proba.inf")


  return(res)

}



############ COMPARING 2 CATEGORIES - RISK


fun.2cat.risk <-function(data.formatted , bayesian.output.D , cat1 , cat2 , user.input,

                         comp.2cat.1 = "Exceedance",
                         comp.2cat.2 = "Percentile",
                         comp.2cat.3 = "Arithmetic mean",
                         comp.2cat.4 = "Parameter",
                         comp.2cat.5 = "Overexpo") {

  conf <- user.input$conf
  thresh <- user.input$frac_threshold
  perc <-user.input$target_perc
  c.oel <-data.formatted$c.oel

  res <-data.frame(parameter=c(comp.2cat.1,paste0(comp.2cat.2,perc),comp.2cat.3),
                   P.overexpo1=numeric(3),
                   P.overexpo2=numeric(3),
                   delta.p.overexpo=numeric(3),
                   stringsAsFactors = FALSE)

  mu1.chain <-bayesian.output.D$mu.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat1],]
  mu2.chain <-bayesian.output.D$mu.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat2],]
  sigma1.chain <-bayesian.output.D$sigma.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat1],]
  sigma2.chain <-bayesian.output.D$sigma.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat2],]

  frac1.chain <-100*(1-pnorm((log(c.oel)-mu1.chain)/sigma1.chain))
  frac2.chain <-100*(1-pnorm((log(c.oel)-mu2.chain)/sigma2.chain))

  pPerc1.chain <-exp(mu1.chain+qnorm(perc/100)*sigma1.chain)
  pPerc2.chain <-exp(mu2.chain+qnorm(perc/100)*sigma2.chain)

  am1.chain <-exp(mu1.chain+0.5*sigma1.chain^2)
  am2.chain <-exp(mu2.chain+0.5*sigma2.chain^2)

  risk1.f <-100*length(frac1.chain[frac1.chain>thresh])/length(frac1.chain)
  risk2.f <-100*length(frac2.chain[frac2.chain>thresh])/length(frac2.chain)

  risk1.pPerc <-100*length(pPerc1.chain[pPerc1.chain>c.oel])/length(pPerc1.chain)
  risk2.pPerc <-100*length(pPerc2.chain[pPerc2.chain>c.oel])/length(pPerc2.chain)

  risk1.am <-100*length(am1.chain[am1.chain>c.oel])/length(am1.chain)
  risk2.am <-100*length(am2.chain[am2.chain>c.oel])/length(am2.chain)


  res$P.overexpo1[1] <-paste(signif(risk1.f,2)," %",sep="")
  res$P.overexpo1[2] <-paste(signif(risk1.pPerc,2)," %",sep="")
  res$P.overexpo1[3] <-paste(signif(risk1.am,2)," %",sep="")

  res$P.overexpo2[1] <-paste(signif(risk2.f,2)," %",sep="")
  res$P.overexpo2[2] <-paste(signif(risk2.pPerc,2)," %",sep="")
  res$P.overexpo2[3] <-paste(signif(risk2.am,2)," %",sep="")

  res$delta.p.overexpo[1] <-paste(signif(risk2.f-risk1.f,2)," %",sep="")
  res$delta.p.overexpo[2] <-paste(signif(risk2.pPerc-risk1.pPerc,2)," %",sep="")
  res$delta.p.overexpo[3] <-paste(signif(risk2.am-risk1.am,2)," %",sep="")

  names(res)[1] <- comp.2cat.4
  for (i in 2:length(names(res))) {
    names(res)[i] <- gsub("overexpo", comp.2cat.5, names(res)[i])
  }

  return(res)

}



######SUPPORT FUNCTIONs

fun.mini.stats <- function(mu.chain,sigma.chain,c.oel,ref.chain,conf, perc) {


  result <-character(9)

  #GM
  result[1] <-paste(signif(median(exp(mu.chain)),2),
                    " [",
                    signif(quantile(exp(mu.chain),(100-conf)/200),2),
                    "-",
                    signif(quantile(exp(mu.chain),1-(100-conf)/200),2),
                    "]",sep="")
  #GSD
  result[2] <-paste(signif(median(exp(sigma.chain)),2),
                    " [",
                    signif(quantile(exp(sigma.chain),(100-conf)/200),2),
                    "-",
                    signif(quantile(exp(sigma.chain),1-(100-conf)/200),2),
                    "]",sep="")

  #RIE

  if (max(mu.chain-ref.chain)==0) result[3] <-"100%"

  else {

    result[3] <-paste(signif(100*exp(median(mu.chain-ref.chain)),2),
                      "% [",
                      signif(quantile(100*exp(mu.chain-ref.chain),(100-conf)/200),2),
                      "-",
                      signif(quantile(100*exp(mu.chain-ref.chain),1-(100-conf)/200),2),
                      "]",sep="")

  }


  #exceedance

  param.chain <-100*(1-pnorm((log(c.oel)-mu.chain)/sigma.chain))

  result[4] <-paste(signif(median(param.chain),2),
                    "% [",
                    signif(quantile(param.chain,(100-conf)/200),2),
                    "-",
                    signif(quantile(param.chain,1-(100-conf)/200),2),
                    "]",sep="")

  result[5] <-paste(signif(quantile(param.chain,conf/100),2),
                    "%",sep="")


  #pPerc

  param.chain <-exp(mu.chain+qnorm(perc/100)*sigma.chain)

  result[6] <-paste(signif(median(param.chain),2),
                    " [",
                    signif(quantile(param.chain,(100-conf)/200),2),
                    "-",
                    signif(quantile(param.chain,1-(100-conf)/200),2),
                    "]",sep="")

  result[7] <-paste(signif(quantile(param.chain,conf/100),2),
                    "",sep="")


  #AM

  param.chain <-exp(mu.chain+0.5*sigma.chain^2)

  result[8] <-paste(signif(median(param.chain),2),
                    " [",
                    signif(quantile(param.chain,(100-conf)/200),2),
                    "-",
                    signif(quantile(param.chain,1-(100-conf)/200),2),
                    "]",sep="")

  result[9] <-paste(signif(quantile(param.chain,conf/100),2),
                    "",sep="")

  return(result)

}


