######################################################
#
#
#
# graphical output functions - determinant analysis specific graphs
#
#
# V1.00   20 march 2018
# V1.01   19 APRIL 2018 #minor typo
# V1.02   2 may 2018 : adding delphine Bosson's graphs
# v1.03   5 june 2018 : tweaks to delphine's graph
# v1.04   18 june 2018 : tweaks to delphine's graph
# v1.05   22 june 2018 : tweaks to delphine's graph
#
######################################################

library(ggplot2)

##
#
#  INPUT : output from one of the bayesian output functions + user.parameter set
#
##

##############################   BOXPLOT BY category graph   ###########################

  boxplot.by.cat <-function(data.formatted ,
                            data.simply.imputed ,
                            bayesian.output.D,
                            pal_col = TRUE,
                            boxplot.cat.1="Categories",
                            boxplot.cat.2="Concentration",
                            boxplot.cat.3="OEL") {

    c.oel<-data.formatted$c.oel

    ########################simulated data

    data.g <- bayesian.output.D$group.id

    data.g$mu <-numeric(length(data.g[,1]))

    for (i in 1:length(data.g[,1])) data.g$mu[i]<-median(bayesian.output.D$mu.chain[i,])

    data.g$sw <- median(bayesian.output.D$sigma.chain[i,])

    dat <-data.frame(value=numeric(0), group=character(0),stringsAsFactors=F)

    for (i in 1:length(data.g[,1])) dat <- rbind(dat, data.frame(value=exp(rnorm(1000,data.g$mu[i],data.g$sw[i])),      group=rep(data.g$name[i],1000),stringsAsFactors=F))


    ######observed data


    data.fin <-data.frame(x.orig=data.formatted$data)

    data.fin$x <-data.simply.imputed$imputed$data$xfin

    data.fin$censored=!data.formatted$notcensored

    data.fin$id <-data.formatted$var


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
      labs(x = boxplot.cat.1, y = boxplot.cat.2) +
      theme(axis.title.x=element_text(size=16,vjust=-1)) +
      theme(axis.text.x=element_text(size=14)) +
      theme(axis.title.y=element_text(size=16,angle=90)) +
      theme(axis.text.y=element_text(size=14,angle=0)) +
      theme(aspect.ratio=0.6) +
      theme(legend.position = "none") +
      annotate("text", 2.5, c.oel*1.5, label = boxplot.cat.3, size=5 , color="red") +
      theme(axis.line = element_line(size = 3, colour = "grey80")) +
      theme(axis.ticks = element_line(size = 2))   +
      coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)

  return(p1)

  }


  ##############################   BOXPLOT BY category graph - 2 categories   ###########################

  boxplot.2.cat <-function(data.formatted ,
                           bayesian.output.D ,
                           cat1 ,
                           cat2,
                           boxplot.2cat.1="Category 1",
                           boxplot.2cat.2="Category 2",
                           boxplot.2cat.3="Concentration",
                           boxplot.2cat.4="OEL",
                           boxplot.2cat.5="Category") {

    #bayesian.output.D <- bayesian.analysis.cat
    #data.formatted <-data.sample.formatted
    #cat1 <- category1
    #cat2 <- category2


      mu1.chain <-bayesian.output.D$mu.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat1],]
      mu2.chain <-bayesian.output.D$mu.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat2],]
      sigma1.chain <-bayesian.output.D$sigma.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat1],]
      sigma2.chain <-bayesian.output.D$sigma.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat2],]



      #data prep

      ### empirical data for the graph

      x <- data.formatted

      cat.values <-data.formatted$var

      ##imputation for each category

      restrict <- x$var==cat1

      x1 <- simple.censored.treatment(
        observations.formatted=x$data[restrict] ,
        notcensored=x$notcensored[restrict] ,
        leftcensored=x$leftcensored[restrict] ,
        rightcensored=x$rightcensored[restrict] ,
        intcensored=x$intcensored[restrict])$imputed$data$xfin

      restrict <- x$var==cat2

      x2 <- simple.censored.treatment(
        observations.formatted=x$data[restrict] ,
        notcensored=x$notcensored[restrict] ,
        leftcensored=x$leftcensored[restrict] ,
        rightcensored=x$rightcensored[restrict] ,
        intcensored=x$intcensored[restrict])$imputed$data$xfin

      dat1 <-data.frame(x=x1,group=boxplot.2cat.1,stringsAsFactors = FALSE)

      dat2 <-data.frame(x=x2,group=boxplot.2cat.2,stringsAsFactors = FALSE)

      empi <-data.frame(value=c(dat1$x,dat2$x),group=factor(c(dat1$group,dat2$group),levels=c(boxplot.2cat.1, boxplot.2cat.2),labels=c(boxplot.2cat.1, boxplot.2cat.2)))


      #predicted data

      gm1 <- exp(median(mu1.chain))
      gsd1 <-exp(median(sigma1.chain))
      gm2 <- exp(median(mu2.chain))
      gsd2 <-exp(median(sigma2.chain))

      #####min max for graph

      min.val <-min(exp(log(gm1)-1.5*log(gsd1)),exp(log(gm2)-1.5*log(gsd2)))
      max.val <-max(exp(log(gm1)+1.5*log(gsd1)),exp(log(gm2)+1.5*log(gsd2)) )

      dat <-data.frame(value=c(exp(rnorm(1000,log(gm1),log(gsd1))),exp(rnorm(1000,log(gm2),log(gsd2)))),
                       group=rep(c( boxplot.2cat.1, boxplot.2cat.2),c(1000,1000)),
                       stringsAsFactors=F)
      dat$group <-factor(dat$group,levels=c(boxplot.2cat.1, boxplot.2cat.2),labels=c(boxplot.2cat.1, boxplot.2cat.2))


      f <- function(x) {
        r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
        names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
        r
      }

      #simulated data plotting

      p1<- ggplot(dat, aes(x=group, y=value, color=group, group=group),ylim=c(min.val,max.val))
      p1 <-p1+geom_point(position=position_jitter(width=0.3), alpha=0.2)
      p1<-p1+stat_summary(fun.data = f, geom="boxplot",size=1.4,alpha=0, outlier.size=0)
      p1<-p1+stat_summary(fun.data = f, geom="boxplot",size=1.4,alpha=0, outlier.size=0,color=alpha("black",0.3))
      p1 <-p1+scale_color_manual(values=c( "darkorchid4","steelblue4"))

      #empirical data plotting
      p1 <-p1+geom_point(data=empi,colour="gray18", shape=20, size = 5,aes(x=group, y=value))
      p1 <-p1+scale_y_log10()
      p1 <-p1+theme(aspect.ratio=0.6)
      p1 <-p1+ylab(boxplot.2cat.3) + xlab(boxplot.2cat.5)
      p1 <-p1+theme(axis.title.x=element_text(size=16,vjust=-1))
      p1 <-p1+theme(axis.text.x=element_text(size=16))
      p1 <-p1+theme(axis.title.y=element_text(size=16,angle=90))
      p1 <-p1+theme(axis.text.y=element_text(size=13,angle=90))
      p1 <-p1+theme(legend.position = "none")
      p1 <-p1+geom_hline(yintercept = data.formatted$c.oel,colour = "red", size = 1)
      p1<-p1+annotate("text", 0.5, data.formatted$c.oel*1.3, label = boxplot.2cat.4)


    return(p1)

  }



  #########################  riskband by band graph



  riskband.perc.byband <- function(bayesian.ouput.D , c.oel, target_perc, sorting=TRUE,
                                   riskplot.3="<1%\nOEL",
                                   riskplot.4="1-10%\nOEL",
                                   riskplot.5="10-50%\nOEL",
                                   riskplot.6="50-100%\nOEL",
                                   riskplot.7=">OEL",
                                   riskband.byband.1 = "Categories",
                                   riskband.byband.2 = "Probability",
                                   riskband.byband.3 = "Critical percentile category"){

    #bayesian.ouput.D <-  bayesian.analysis.cat

    #c.oel <-0.3

    #target_perc <-95

    #sorting <-TRUE

    #bornes <- c("<1%", "1-10%", "10-50%", "50-100%", ">OEL")

    # riskplot.3="<1%\nOEL"
    # riskplot.4="1-10%\nOEL"
    # riskplot.5="10-50%\nOEL"
    #riskplot.6="50-100%\nOEL"
    #riskplot.7=">OEL"
    # riskband.byband.1 = "Categories"
    # riskband.byband.2 = "Probability"
    # riskband.byband.3 = "Critical percentile category"

    bornes<-c(riskplot.3, riskplot.4, riskplot.5, riskplot.6, riskplot.7)

    #creation of the basic dataframe

    df <- data.frame(Cat = bayesian.ouput.D$group.id$name)

    #matrix of probabilities by category

    mat.prob <-matrix(nrow = length(bayesian.ouput.D$group.id$name), ncol = 5)

    for (i in 1:length(bayesian.ouput.D$group.id$name))

    {

      mu <-bayesian.ouput.D$mu.chain[i,]

      sigma<-bayesian.ouput.D$sigma.chain[i,]

      perc.chain <-exp(mu+qnorm(target_perc/100)*sigma)

      C1 <-100*length(perc.chain[perc.chain<0.01*c.oel])/length(perc.chain)
      C2 <-100*length(perc.chain[perc.chain>=0.01*c.oel & perc.chain<0.1*c.oel])/length(perc.chain)
      C3 <-100*length(perc.chain[perc.chain>=0.1*c.oel & perc.chain<0.5*c.oel])/length(perc.chain)
      C4 <-100*length(perc.chain[perc.chain>=0.5*c.oel & perc.chain<1*c.oel])/length(perc.chain)
      C5 <-100*length(perc.chain[perc.chain>=1*c.oel ])/length(perc.chain)

      # mat.prob[i,] <-c(C5,C4,C3,C2,C1)
      mat.prob[i,] <-c(C1,C2,C3,C4,C5)
    }

    df <-cbind(df,mat.prob)

    names(df) <- c("Cat","p.1","p.2","p.3","p.4","p.5")

    ###          Dephine Bosson's script
    ######################### SETTINGS #############################
    ### Cat refers to the ID variable of each category/worker
    ### p refers to the related probability
    ### time comes from the reshape and refers to the # measure of each category/worker
    ### bornes refers to the choosen born to appear in the legend

    ### Reshape a wide table into long table and potential data sort

    if(sorting == TRUE){

      df <- df[order(df$p.3, decreasing = TRUE), ]
      df$Cat <- factor(df$Cat, levels = as.character(df$Cat))

    }

    df <- reshape(df, varying = paste("p", 1 : 5, sep = "."), direction = "long") # 5 refers to the number of strips

    ### New order for decreasing appearance
    df <- df[order(df$Cat, df$time), ]
    df$time <- factor(df$time, levels = c(5:1)); levels(df$time) <- rev(bornes)

    ###Cumulative sum to put the text
    df$CumSum <- unlist(by(df$p, df$Cat, cumsum))

    ######################### PLOT #############################

    ### Drawing ggplot
    g3 <- ggplot(df, aes(Cat, p, fill = as.factor(time))) +
      labs(x = riskband.byband.1, y = riskband.byband.2) +
      geom_bar(stat = "identity") +
      scale_fill_manual(riskband.byband.3,
                        values = alpha(c("darkred", "gray15", "gray25", "gray50", "gray90"), .9)) +
      geom_text(aes(label = ifelse(p<3,"",round(p, 0)), y = CumSum), colour = rep(c("black","white","white", "white", "white"), nrow(bayesian.ouput.D$group.id)), size = 3, vjust = 1.5 , hjust=1.3) +
      theme_light() +
      theme(legend.position = "bottom", legend.direction = "horizontal")
    g3 <- g3 + theme(text = element_text(size = 15))+
      coord_flip(xlim = NULL, ylim = NULL, expand = TRUE) +
      guides(fill = guide_legend(reverse = TRUE))

    ### Return results
    return(g3)

  }





  #########################  riskband by band graph

  risk.gauge <- function(bayesian.ouput.D , c.oel, user.input,
                         ggplot.cat.1 = "",
                         ggplot.cat.2 = "Overexposure risk")

    {



   #bayesian.ouput.D <-  Y

   #c.oel <-X$c.oel

   #user.input <- Z

   #ggplot.cat.1 = ""
   #ggplot.cat.2 = "Overexposure risk (%)"



    df <- data.frame(Cat = bayesian.ouput.D$group.id$name, p=numeric(length(bayesian.ouput.D$group.id$name)))

    #adding overexposure risk

     for (i in 1:length(bayesian.ouput.D$group.id$name))

    {

      mu <-bayesian.ouput.D$mu.chain[i,]

      sigma<-bayesian.ouput.D$sigma.chain[i,]

      perc.chain <-exp(mu+qnorm(user.input$target_perc/100)*sigma)

     df$p[i] <-100*length(perc.chain[perc.chain>c.oel])/length(perc.chain)

     }

    df <-df[order(df$p, decreasing = FALSE),]


    ################################## SCRIPT DELPHIHNE BOSSON #####################################

    ### Need to redefine the factors' order to suit with ggplot
    df$Cat <- factor(df$Cat, levels = as.character(df$Cat))

    ### Rescaling for value 5 and 20 to appear as 30 and 60 in y axis
    df$p2 <- ifelse(df$p <= 5, df$p / 5 * 30,
                    ifelse(df$p <= 30, 24 + (df$p / 25 * 30),
                           50 + (df$p / 80 * 40)))

    ### Definition of a new scaling for labels and tick marks
    manual_scale <- c(0, 5, seq(20, 100, by = 10))
    manual_scale2 <- ifelse(manual_scale <= 5, manual_scale / 5 * 30,
                            ifelse(manual_scale <= 30, 24 + (manual_scale / 25 * 30),
                                   50 + (manual_scale / 80 * 40)))

    ######################### PLOT #############################

    ### Draw first ggplot
    p1 <- ggplot(df, aes(p2, Cat)) + # aes_string a garder en tete dans fonction
      expand_limits(x = c(0, 100)) +
      geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 1) +
      labs(x = paste(ggplot.cat.2, "(%)"), y = ggplot.cat.1) +
      scale_x_discrete(breaks = manual_scale2, limits = manual_scale, labels = manual_scale, position = "top") +
      coord_fixed(ratio = nrow(df)*1.8)

    ### Collect the maximum range, depending on number of group, to build background until x limit
    max.x.range <- ggplot_build(p1)$layout$panel_ranges[[1]]$x.range[2]

    ### Continue plot with Background + Point + Text
    p1 <- p1 +
      annotate("rect",
               xmin = -Inf, xmax = 30 ,
               ymin = -Inf, ymax = Inf,  fill = "green", alpha = .3) +
      annotate("rect",
               xmin = 30, xmax = 60,
               ymin = -Inf, ymax = Inf,  fill = "yellow", alpha = .3) +
      annotate("rect",
               xmin = 60, xmax = Inf,
               ymin = -Inf, ymax = Inf,  fill = "red", alpha = .3) +

      geom_segment(aes(x = 0, y = Cat, xend = 98, yend = Cat), size = 10, lineend = "round", alpha = .3) +
      geom_segment(aes(xend = 0, yend = Cat), size = 7, lineend = "round") +
      geom_point(shape = 21, colour = "black", fill = ifelse(df$p <= 5, "chartreuse3", ifelse(df$p <= 20, "gold1", "red3")), size = 5, stroke = 0.5) +
      geom_text(aes(label = round(p,2), x = p2 + 0.05), size = 3, position = position_dodge(0.9), hjust = - 0.6, fontface = "bold") +
      theme_light()
    p1 <- p1 + theme(text = element_text(size = 15))


    ### Return result
    return(p1)

  }

  plot.frac.grp <- function(bayesian.output.D,
                            c.oel,
                            frac_threshold,
                            # psi=30,
                            sorting = TRUE,
                            ggplot.GRP.1 = "Categories",
                            ggplot.GRP.2 = "Probabilities",
                            ggplot.GRP.3 = "Category of overexposure probability (%)",
                            ggplot.GRP.4 = "",
                            ggplot.GRP.5 = "Overexposure probability (%)"){

    data_source <- data.frame(GRP = bayesian.output.D$group.id$name)
    bande1 <- frac_threshold/10
    bornes <- c(paste0('<', bande1, '%'),paste0(bande1, '-', frac_threshold, '%'),paste0('>', frac_threshold, '%'))

    for (i in 1 : nrow(data_source)){

      mu <- bayesian.output.D$mu.chain[i, ]

      sigma <- bayesian.output.D$sigma.chain[i,]

      frac.chain <-100*(1-pnorm((log(c.oel)-mu)/sigma))

      data_source$C.3[i] <- 100*length(frac.chain[frac.chain>=frac_threshold ])/length(frac.chain)
      data_source$C.2[i] <- 100*length(frac.chain[frac.chain>=bande1 & frac.chain<frac_threshold])/length(frac.chain)
      data_source$C.1[i] <- 100*length(frac.chain[frac.chain<bande1])/length(frac.chain)

    }

    ######################### SETTINGS #############################
    ### GRP refers to the ID variable of each category/worker
    ### p refers to the related probability
    ### time comes from the reshape and refers to the # measure of each category/worker
    ### bornes refers to the choosen born to appear in the legend

    data <- data_source

    if(sorting == TRUE){

      data <- data[order(data$C.3, decreasing = TRUE), ]
      data$GRP <- factor(data$GRP, levels = as.character(data$GRP))

    }

    data <- reshape(data, varying = paste("C", 3 : 1, sep = "."), direction = "long") #3 refers to the number of strip (bandes)

    ### Need to redefine the factors' name with borns' value
    data$time <- factor(data$time); levels(data$time) <- bornes

    ### New order to calculate cumulative sum for each one
    data <- data[order(data$GRP), ]
    data$CumSum <- unlist(by(data$C, data$GRP, cumsum))


    ######################### PLOT CUMULE #############################

    ### Drawing ggplot
    g1 <- ggplot(data, aes(GRP, C, fill = as.factor(time))) +
      labs(x = ggplot.GRP.1, y = ggplot.GRP.2) +
      geom_bar(stat = "identity") +
      scale_fill_manual(ggplot.GRP.3,
                        values = alpha(c("gray70", "gray25", "darkred"), .9)) +
      geom_text(aes(label = ifelse(C != 0, round(C, 2), NA), y = ifelse(C != 0, CumSum, NA)), colour = rep(c("white","white", "black"), nrow(bayesian.output.D$group.id)), size = 3, vjust = 1.5) +
      theme_light() +
      theme(legend.position = "bottom", legend.direction = "horizontal")

    ### Return result
    #print(g1)

    ######################### PLOT JAUGE #############################

    ### New order for decreasing appearance
    data2 <- data_source[order(data_source$C.3, decreasing = FALSE), ]
    ### Need to redefine the factors' order to suit with ggplot
    data2$GRP <- factor(data2$GRP, levels = as.character(data2$GRP))

    ### Rescaling for value 5 and 20 to appear as 30 and 60 in y axis
    data2$C.3.2 <- ifelse(data2$C.3 <= 5, data2$C.3 / 5 * 30,
                          ifelse(data2$C.3 <= 30, 24 + (data2$C.3 / 25 * 30),
                                 50 + (data2$C.3 / 80 * 40)))

    ### Definition of a new scaling for labels and tick marks
    manual_scale <- c(0, 5, seq(20, 100, by = 10))
    manual_scale2 <- ifelse(manual_scale <= 5, manual_scale / 5 * 30,
                            ifelse(manual_scale <= 30, 24 + (manual_scale / 25 * 30),
                                   50 + (manual_scale / 80 * 40)))


    ### Draw first ggplot
    p2 <- ggplot(data2, aes(C.3.2, GRP)) + # aes_string a garder en tete dans fonction
      expand_limits(x = c(0, 105)) +
      geom_point(shape = 21, colour = "black", fill = "white", size = 1, stroke = 1) +
      labs(x = ggplot.GRP.5, y = ggplot.GRP.4) +
      scale_x_discrete(breaks = manual_scale2, limits = manual_scale, labels = manual_scale, position = "top") +
      coord_fixed(ratio = nrow(data2)*1.9)

    ### Collect the maximum range, depending on number of group, to build background until x limit
    max.x.range <- ggplot_build(p2)$layout$panel_ranges[[1]]$x.range[2]

    ### Continue plot with Background + Point + Text
    p2 <- p2 +
      annotate("rect",
               xmin = -Inf, xmax = 30 ,
               ymin = -Inf, ymax = Inf,  fill = "green", alpha = .3) +
      annotate("rect",
               xmin = 30, xmax = 60,
               ymin = -Inf, ymax = Inf,  fill = "yellow", alpha = .3) +
      annotate("rect",
               xmin = 60, xmax = Inf,
               ymin = -Inf, ymax = Inf,  fill = "red", alpha = .3) +

      geom_segment(aes(x = 0, y = GRP, xend = 100, yend = GRP), size = 9, lineend = "round", alpha = .3) +
      geom_segment(aes(xend = 0, yend = GRP), size = 5, lineend = "round") +
      geom_point(shape = 21, colour = "black", fill = ifelse(data2$C.3 <= 5, "chartreuse3", ifelse(data2$C.3 <= 20, "gold1", "red3")), size = 4, stroke = 0.5) +
      geom_text(aes(label = round(C.3, 2), x = C.3.2 + 0.05), size = 3, position = position_dodge(0.9), hjust = - 0.6, fontface = "bold") +
      theme_light()

    return(p2)
  }



  plot.perc.grp <- function(bayesian.output.D,
                            c.oel,
                            target_perc,
                            # psi=30,
                            sorting = TRUE,
                            ggplot.GRP.1 = "Categories",
                            ggplot.GRP.2 = "Probabilities",
                            ggplot.GRP.3 = "Category of overexposure probability (%)",
                            ggplot.GRP.4 = "",
                            ggplot.GRP.5 = "Overexposure probability (%)",
                            ggplot.GRP.6 = "<1%\nOEL",
                            ggplot.GRP.7 = "1-10%\nOEL",
                            ggplot.GRP.8 = "10-50%\nOEL",
                            ggplot.GRP.9 = "50-100%\nOEL",
                            ggplot.GRP.10 = ">OEL",
                            ggplot.GRP.11 = "Critical percentile category"){


    data_source <- data.frame(GRP = bayesian.output.D$group.id$name)
    bornes <- c(ggplot.GRP.6, ggplot.GRP.7, ggplot.GRP.8, ggplot.GRP.9, ggplot.GRP.10)

    for (i in 1 : nrow(data_source)){

      mu <- bayesian.output.D$mu.chain[i, ]

      sigma <- bayesian.output.D$sigma.chain[i,]

      perc.chain <-exp(mu+qnorm(target_perc/100)*sigma)

      data_source$C.5[i] <-100*length(perc.chain[perc.chain>=1*c.oel ])/length(perc.chain)
      data_source$C.4[i] <-100*length(perc.chain[perc.chain>=0.5*c.oel & perc.chain<1*c.oel])/length(perc.chain)
      data_source$C.3[i] <-100*length(perc.chain[perc.chain>=0.1*c.oel & perc.chain<0.5*c.oel])/length(perc.chain)
      data_source$C.2[i] <-100*length(perc.chain[perc.chain>=0.01*c.oel & perc.chain<0.1*c.oel])/length(perc.chain)
      data_source$C.1[i] <-100*length(perc.chain[perc.chain<0.01*c.oel])/length(perc.chain)

    }

    ######################### SETTINGS #############################
    ### GRP refers to the ID variable of each category/worker
    ### p refers to the related probability
    ### time comes from the reshape and refers to the # measure of each category/worker
    ### bornes refers to the choosen born to appear in the legend

    data <- data_source

    if(sorting == TRUE){

      data <- data[order(data$C.5, decreasing = FALSE), ]
      data$GRP <- factor(data$GRP, levels = as.character(data$GRP))

    }

    data <- reshape(data, varying = paste("C", 5 : 1, sep = "."), direction = "long") #5 refers to the number of strip (bandes)

    ### Need to redefine the factors' name with borns' value
    data$time <- factor(data$time); levels(data$time) <- bornes

    ### New order to calculate cumulative sum for each one
    data <- data[order(data$GRP), ]
    data$CumSum <- unlist(by(data$C, data$GRP, cumsum))


    ######################### PLOT CUMULE #############################

    ### Drawing ggplot
    g1 <- ggplot(data, aes(GRP, C, fill = as.factor(time))) +
      labs(x = ggplot.GRP.1, y = ggplot.GRP.2) +
      geom_bar(stat = "identity") + coord_flip() +
      scale_fill_manual(ggplot.GRP.3,
                        values = alpha(c("gray90", "gray50", "gray25", "gray15", "darkred" ), .9)) +
      geom_text(aes(label = ifelse(C != 0, round(C, 1), NA), y = ifelse(C >= 0.05, CumSum, NA)), colour = rep(c("white","white", "white", "black", "black"), nrow(bayesian.output.D$group.id)), size = 3, hjust = 1.3) +
      scale_y_discrete(limits = c(0,25,50,75,100), position = "top") +
      theme(axis.title.x=element_text(vjust=-0.5,size=16))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.x=element_text(size=12))+
      theme(axis.text.y=element_text(size=12,angle=0)) +
      theme_light() + theme(aspect.ratio = 0.3) +
      theme(legend.position = "bottom", legend.direction = "horizontal")

    ### Return result
    #print(g1)


    ######################### PLOT JAUGE #############################

    ### New order for decreasing appearance
    data2 <- data_source[order(data_source$C.5, decreasing = FALSE), ]
    ### Need to redefine the factors' order to suit with ggplot
    data2$GRP <- factor(data2$GRP, levels = as.character(data2$GRP))

    ### Rescaling for value 5 and 20 to appear as 30 and 60 in y axis
    data2$C.5.2 <- ifelse(data2$C.5 <= 5, data2$C.5 / 5 * 30,
                          ifelse(data2$C.5 <= 30, 24 + (data2$C.5 / 25 * 30),
                                 50 + (data2$C.5 / 80 * 40)))

    ### Definition of a new scaling for labels and tick marks
    manual_scale <- c(0, 5, seq(20, 100, by = 10))
    manual_scale2 <- ifelse(manual_scale <= 5, manual_scale / 5 * 30,
                            ifelse(manual_scale <= 30, 24 + (manual_scale / 25 * 30),
                                   50 + (manual_scale / 80 * 40)))


    ### Draw first ggplot
    p1 <- ggplot(data2, aes(C.5.2, GRP)) + # aes_string a garder en tete dans fonction
      expand_limits(x = c(0, 108)) +
      geom_point(shape = 21, colour = "black", fill = "white", size = 1, stroke = 1) +
      labs(x = ggplot.GRP.5, y = ggplot.GRP.4) +
      scale_x_discrete(breaks = manual_scale2, limits = manual_scale, labels = manual_scale, position = "top") +
      coord_fixed(ratio = nrow(data2)*1.9)

    ### Collect the maximum range, depending on number of group, to build background until x limit
    max.x.range <- ggplot_build(p1)$layout$panel_ranges[[1]]$x.range[2]

    ### Continue plot with Background + Point + Text
    p1 <- p1 +
      annotate("rect",
               xmin = -Inf, xmax = 30 ,
               ymin = -Inf, ymax = Inf,  fill = "green", alpha = .3) +
      annotate("rect",
               xmin = 30, xmax = 60,
               ymin = -Inf, ymax = Inf,  fill = "yellow", alpha = .3) +
      annotate("rect",
               xmin = 60, xmax = Inf,
               ymin = -Inf, ymax = Inf,  fill = "red", alpha = .3) +

      geom_segment(aes(x = 0, y = GRP, xend = 100, yend = GRP), size = 9, lineend = "round", alpha = .3) +
      geom_segment(aes(xend = 0, yend = GRP), size = 5, lineend = "round") +
      geom_point(shape = 21, colour = "black", fill = ifelse(data2$C.5 <= 5, "chartreuse3", ifelse(data2$C.5 <= 20, "gold1", "red3")), size = 4, stroke = 0.5) +
      geom_text(aes(label = round(C.5, 2), x = C.5.2 + 0.05), size = 3, position = position_dodge(0.9), hjust = - 0.6, fontface = "bold") +
      theme_light() + theme(aspect.ratio = 0.2) +
      theme(axis.title.x=element_text(vjust=-0.5,size=11))+
      theme(axis.title.y=element_text(size=11,angle=90))+
      theme(axis.text.x=element_text(size=10))+
      theme(axis.text.y=element_text(size=10,angle=0))

    return(p1)


  }




  plot.am.grp <- function(bayesian.output.D,
                          c.oel,
                          # psi=30,
                          sorting = TRUE,
                          ggplot.GRP.1 = "Categories",
                          ggplot.GRP.2 = "Probabilities",
                          ggplot.GRP.3 = "Category of overexposure probability (%)",
                          ggplot.GRP.4 = "",
                          ggplot.GRP.5 = "Overexposure probability (%)",
                          ggplot.GRP.6 = "<1%\nOEL",
                          ggplot.GRP.7 = "1-10%\nOEL",
                          ggplot.GRP.8 = "10-50%\nOEL",
                          ggplot.GRP.9 = "50-100%\nOEL",
                          ggplot.GRP.10 = ">OEL",
                          ggplot.GRP.11 = "Critical percentile category"){


    data_source <- data.frame(GRP = bayesian.output.D$group.id$name)
    bornes <- c(ggplot.GRP.6, ggplot.GRP.7, ggplot.GRP.8, ggplot.GRP.9, ggplot.GRP.10)

    for (i in 1 : nrow(data_source)){

      mu <- bayesian.output.D$mu.chain[i, ]

      sigma <- bayesian.output.D$sigma.chain[i,]

      am.chain <- exp(mu+0.5*sigma^2)

      data_source$C.5[i] <- 100*length(am.chain[am.chain>=1*c.oel ])/length(am.chain)
      data_source$C.4[i] <- 100*length(am.chain[am.chain>=0.5*c.oel & am.chain<1*c.oel])/length(am.chain)
      data_source$C.3[i] <- 100*length(am.chain[am.chain>=0.1*c.oel & am.chain<0.5*c.oel])/length(am.chain)
      data_source$C.2[i] <- 100*length(am.chain[am.chain>=0.01*c.oel & am.chain<0.1*c.oel])/length(am.chain)
      data_source$C.1[i] <- 100*length(am.chain[am.chain<0.01*c.oel])/length(am.chain)

    }

    ######################### SETTINGS #############################
    ### GRP refers to the ID variable of each category/worker
    ### p refers to the related probability
    ### time comes from the reshape and refers to the # measure of each category/worker
    ### bornes refers to the choosen born to appear in the legend

    data <- data_source

    if(sorting == TRUE){

      data <- data[order(data$C.5, decreasing = FALSE), ]
      data$GRP <- factor(data$GRP, levels = as.character(data$GRP))

    }

    data <- reshape(data, varying = paste("C", 5 : 1, sep = "."), direction = "long") #5 refers to the number of strip (bandes)

    ### Need to redefine the factors' name with borns' value
    data$time <- factor(data$time); levels(data$time) <- bornes

    ### New order to calculate cumulative sum for each one
    data <- data[order(data$GRP), ]
    data$CumSum <- unlist(by(data$C, data$GRP, cumsum))


    ######################### PLOT CUMULE #############################

    ### Drawing ggplot
    g1 <- ggplot(data, aes(GRP, C, fill = as.factor(time))) +
      labs(x = ggplot.GRP.1, y = ggplot.GRP.2) +
      geom_bar(stat = "identity") + coord_flip() +
      scale_fill_manual(ggplot.GRP.3,
                        values = alpha(c("gray90", "gray50", "gray25", "gray15", "darkred" ), .9)) +
      geom_text(aes(label = ifelse(C != 0, round(C, 1), NA), y = ifelse(C >= 0.05, CumSum, NA)), colour = rep(c("white","white", "white", "black", "black"), nrow(bayesian.output.D$group.id)), size = 3, hjust = 1.3) +
      scale_y_discrete(limits = c(0,25,50,75,100), position = "top") +
      theme(axis.title.x=element_text(vjust=-0.5,size=16))+
      theme(axis.title.y=element_text(size=16,angle=90))+
      theme(axis.text.x=element_text(size=12))+
      theme(axis.text.y=element_text(size=12,angle=0)) +
      theme_light() + theme(aspect.ratio = 0.3) +
      theme(legend.position = "bottom", legend.direction = "horizontal")

    ### Return result
    #print(g1)


    ######################### PLOT JAUGE #############################

    ### New order for decreasing appearance
    data2 <- data_source[order(data_source$C.5, decreasing = FALSE), ]
    ### Need to redefine the factors' order to suit with ggplot
    data2$GRP <- factor(data2$GRP, levels = as.character(data2$GRP))

    ### Rescaling for value 5 and 20 to appear as 30 and 60 in y axis
    data2$C.5.2 <- ifelse(data2$C.5 <= 5, data2$C.5 / 5 * 30,
                          ifelse(data2$C.5 <= 30, 24 + (data2$C.5 / 25 * 30),
                                 50 + (data2$C.5 / 80 * 40)))

    ### Definition of a new scaling for labels and tick marks
    manual_scale <- c(0, 5, seq(20, 100, by = 10))
    manual_scale2 <- ifelse(manual_scale <= 5, manual_scale / 5 * 30,
                            ifelse(manual_scale <= 30, 24 + (manual_scale / 25 * 30),
                                   50 + (manual_scale / 80 * 40)))


    ### Draw first ggplot
    p1 <- ggplot(data2, aes(C.5.2, GRP)) + # aes_string a garder en tete dans fonction
      expand_limits(x = c(0, 108)) +
      geom_point(shape = 21, colour = "black", fill = "white", size = 1, stroke = 1) +
      labs(x = ggplot.GRP.5, y = ggplot.GRP.4) +
      scale_x_discrete(breaks = manual_scale2, limits = manual_scale, labels = manual_scale, position = "top") +
      coord_fixed(ratio = nrow(data2)*1.9)

    ### Collect the maximum range, depending on number of group, to build background until x limit
    max.x.range <- ggplot_build(p1)$layout$panel_ranges[[1]]$x.range[2]

    ### Continue plot with Background + Point + Text
    p1 <- p1 +
      annotate("rect",
               xmin = -Inf, xmax = 30 ,
               ymin = -Inf, ymax = Inf,  fill = "green", alpha = .3) +
      annotate("rect",
               xmin = 30, xmax = 60,
               ymin = -Inf, ymax = Inf,  fill = "yellow", alpha = .3) +
      annotate("rect",
               xmin = 60, xmax = Inf,
               ymin = -Inf, ymax = Inf,  fill = "red", alpha = .3) +

      geom_segment(aes(x = 0, y = GRP, xend = 100, yend = GRP), size = 9, lineend = "round", alpha = .3) +
      geom_segment(aes(xend = 0, yend = GRP), size = 5, lineend = "round") +
      geom_point(shape = 21, colour = "black", fill = ifelse(data2$C.5 <= 5, "chartreuse3", ifelse(data2$C.5 <= 20, "gold1", "red3")), size = 4, stroke = 0.5) +
      geom_text(aes(label = round(C.5, 2), x = C.5.2 + 0.05), size = 3, position = position_dodge(0.9), hjust = - 0.6, fontface = "bold") +
      theme_light() + theme(aspect.ratio = 0.2) +
      theme(axis.title.x=element_text(vjust=-0.5,size=11))+
      theme(axis.title.y=element_text(size=11,angle=90))+
      theme(axis.text.x=element_text(size=10))+
      theme(axis.text.y=element_text(size=10,angle=0))

    return(p1)


  }
