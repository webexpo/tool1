#########################################################################
#
#
# Descriptive statistics, using simple procedure for censored data
#
#
# V1.00   20 march 2018
#
#
####################################################################

##
#
# INPUT : ouput from simple.censored.treatment + corrected OEL
#
##

fun.desc.stat <-function(data.simply.imputed , c.oel) {


#res is the data element of the fun.NDexpo output

  res <-data.simply.imputed$imputed$data

  if (length(res[res$is.censored==0,1])==0) {

    results <-data.frame(parameter=c('n','PropCensored','min',
                                     'Q25','Q50','Q75','maximum',
                                     'propOverOEL','am','asd',
                                     'cv','gm','gsd'),
                         value=numeric(13),stringsAsFactors=F)

    results[1,2] <- as.character(dim(res)[1])
    results[2,2] <- as.character(0)
    results[3,2] <- as.character(signif(min(res$x),3))
    results[4,2] <- as.character(signif(quantile(res$x,probs=0.25),3))
    results[5,2] <- as.character(signif(median(res$x),3))
    results[6,2] <- as.character(signif(quantile(res$x,probs=0.75),3))
    results[7,2] <- as.character(signif(max(res$x),3))
    results[8,2] <- as.character(paste0(signif(100*length(res$x[res$x>c.oel])/length(res$x),2), "%"))
    results[9,2] <- as.character(signif(mean(res$x),3))
    results[10,2] <- as.character(signif(sd(res$x),3))
    results[11,2] <- as.character(paste0(signif(100*sd(res$x)/mean(res$x),3), "%"))
    results[12,2] <- as.character(signif(exp(mean(res$yfin)),3))
    results[13,2] <- as.character(signif(exp(sd(res$yfin)),3))
  }

if (length(res[res$is.censored==0,1])!=0)

  {
  results <-data.frame(parameter=c('n','PropCensored','min',
                                   'Q25','Q50','Q75','maximum',
                                   'propOverOEL','am','asd',
                                   'cv','gm','gsd'),
                       value=numeric(13),stringsAsFactors=F)

    results[1,2] <- as.character(dim(res)[1])
    results[2,2] <- paste0(as.character(signif(100*length(res[data.simply.imputed$notcensored==FALSE,1])/length(res[,1]),2)), "%")
    results[3,2] <- as.character(paste('<',signif(min(res$x[res$is.censored==0]),3)),sep='')
    results[4,2] <- as.character(signif(quantile(exp(res$yfin),probs=0.25),3))
    results[5,2] <- as.character(signif(median(exp(res$yfin)),3))
    results[6,2] <- as.character(signif(quantile(exp(res$yfin),probs=0.75),3))
    results[7,2] <- as.character(signif(max(res$x),3))
    results[8,2] <- as.character(paste0(signif(100*length(res$x[res$x>c.oel])/length(res$x),2), "%"))
    results[9,2] <- as.character(signif(mean(exp(res$yfin)),3))
    results[10,2] <- as.character(signif(sd(exp(res$yfin)),3))
    results[11,2] <- as.character(paste0(signif(100*sd(exp(res$yfin))/mean(exp(res$yfin)),3), "%"))
    results[12,2] <- as.character(signif(exp(mean(res$yfin)),3))
    results[13,2] <- as.character(signif(exp(sd(res$yfin)),3))


  }

  return(results)

  }


