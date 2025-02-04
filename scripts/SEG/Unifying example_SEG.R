################################################################
#
#
#  Example of running the complete SEG analysis tool
#
# V1.02   june 4 2018 minor tweaks
# V1.01   22 march 2018 adding the descriptive boxplot
# V1.00   20 march 2018
#
#
################################################################


######### sourcing the scripts

setwd("D:/Dropbox/SHARE Daniel Margulius/expostats2018/scripts")



##SEG SPECIFIC FUNCTIONS

source("SEG/Data formatting functions_SEG.R")

source("SEG/Generating a random sample functions_SEG.R") #this function is not necessary for calculations, just used to create example datasets

##COMMON

source("Common/Simple censored imputation functions.R")

source("Common/Descriptive numerical output functions.R")

source("Common/Descriptive graphs functions.R")

source("Common/Bayesian engine functions.R")

source("Common/script density comparison.R") #this function is not necessary for calculations, just used to compare various bayesian engines

source("Common/Numerical output functions.R")

source("Common/Main graph functions.R")

#################### Creating a sample ###########################


data.sample <- fun.gener.censored(n=300)$string


###alternatively : the default expostats sample with some uncleanliness

data.sample<- '   28.9\n19.4\n<5.5\n\n>149.9\n26.42\n56.1\n[10-20]   '

data.sample<- '   28.9\n19.4\n5.5\n\n>149.9\n26.42\n56.1\n15   '

################ initial data formating

data.sample.formatted <-data.formatting.SEG(data.in=data.sample,oel=2, oel.mult = 1)

############## simple censored imputation

data.sample.imputed <-simple.censored.treatment(observations.formatted=data.sample.formatted$data,
                                                notcensored=data.sample.formatted$notcensored,
                                                leftcensored=data.sample.formatted$leftcensored,
                                                rightcensored=data.sample.formatted$rightcensored,
                                                intcensored=data.sample.formatted$intcensored)

#############  descriptive statistics

View(fun.desc.stat(data.simply.imputed=data.sample.imputed , c.oel = data.sample.formatted$c.oel ) )


#############  descriptive graphs

fun.qqplot(data.simply.imputed=data.sample.imputed , notcensored = data.sample.formatted$notcensored)

fun.boxplot( data.simply.imputed = data.sample.imputed,
                notcensored = data.sample.formatted$notcensored,
             c.oel =data.sample.formatted$c.oel,
                        boxplot.1="Measurement type",
                        boxplot.2="Concentration",
                        boxplot.3="OEL",
                        boxplot.4="Censored",
                        boxplot.5="Detected",
                        boxplot.6="Data")
################## bayesian output - jags

#JAGS function
res1 <-fun.bayes.jags(observations=data.sample.formatted$data ,
                      notcensored=data.sample.formatted$notcensored,
                      leftcensored=data.sample.formatted$leftcensored ,
                      rightcensored=data.sample.formatted$rightcensored ,
                      intcensored=data.sample.formatted$intcensored ,
                      seed=data.sample.formatted$seed ,
                      c.oel=data.sample.formatted$c.oel , n.iter=25000 , updateProgress= NULL)
# STAN function
res2 <-fun.bayes.stan(observations=data.sample.formatted$data ,
                      notcensored=data.sample.formatted$notcensored,
                      leftcensored=data.sample.formatted$leftcensored ,
                      rightcensored=data.sample.formatted$rightcensored ,
                      intcensored=data.sample.formatted$intcensored ,
                      seed=data.sample.formatted$seed ,
                      c.oel=data.sample.formatted$c.oel , n.iter=25000 , updateProgress= NULL,
                      mypath <-"C:/jerome/Dropbox/bureau/RStudio/expostats/shinyapps/expostats functions 2018/Common functions")

# WEBEXPO function
res3 <-fun.bayes.webexpo(observations=data.sample.formatted$data ,
                         notcensored=data.sample.formatted$notcensored,
                         leftcensored=data.sample.formatted$leftcensored ,
                         rightcensored=data.sample.formatted$rightcensored ,
                         intcensored=data.sample.formatted$intcensored ,
                         seed=data.sample.formatted$seed ,
                         c.oel=data.sample.formatted$c.oel , n.iter=25000 , updateProgress= NULL,
                         mypath <-"C:/jerome/Dropbox/bureau/RStudio/expostats/shinyapps/expostats functions 2018/Common functions")


############# comparing the bbayesian results

fun.compare.dens(jags.chain=res1$mu.chain ,stan.chain=res2$mu.chain,webexpo.chain=res3$mu.chain)


fun.compare.dens(jags.chain=res1$sigma.chain,stan.chain=res2$sigma.chain,webexpo.chain=res3$sigma.chain)


################## numerical output - jags


##### EXEMPLE OF INPUT SET

user.input <-list( conf=90 , psi = 30 , frac_threshold = 5 , target_perc = 95 )

num.res <-all.numeric(mu.chain=res1$mu.chain ,
                      sigma.chain=res1$sigma.chain ,
                      conf=user.input$conf ,
                      c.oel=data.sample.formatted$c.oel ,
                      frac_threshold=user.input$frac_threshold ,
                      target_perc=user.input$target_perc)



############### graphical ouput

#calendar plot

calendar.plot(frac.est=num.res$frac$est, pal_col = FALSE)

#sequential plot

sequential.plot.frac(gm=num.res$gm$est ,
                     gsd=num.res$gsd$est ,
                     frac=num.res$frac$est ,
                     c.oel=data.sample.formatted$c.oel ,
                     data.simply.imputed=data.sample.imputed)

sequential.plot.perc(gm=num.res$gm$est ,
                     gsd=num.res$gsd$est ,
                     perc=num.res$perc$est ,
                     target_perc=user.input$target_perc,
                     c.oel=data.sample.formatted$c.oel ,
                     data.simply.imputed=data.sample.imputed)

sequential.plot.am(gm=num.res$gm$est ,
                   gsd=num.res$gsd$est ,
                   am=num.res$am$est ,
                   c.oel=data.sample.formatted$c.oel ,
                   data.simply.imputed=data.sample.imputed)


#distribution plot

distribution.plot.frac(gm=num.res$gm$est ,
                       gsd=num.res$gsd$est ,
                       frac=num.res$frac$est ,
                       c.oel=data.sample.formatted$c.oel)

distribution.plot.perc(gm=num.res$gm$est ,
                       gsd=num.res$gsd$est ,
                       perc=num.res$perc$est ,
                       target_perc=user.input$target_perc,
                       c.oel=data.sample.formatted$c.oel)

distribution.plot.am(gm=num.res$gm$est ,
                     gsd=num.res$gsd$est ,
                     am=num.res$am$est ,
                     c.oel=data.sample.formatted$c.oel)

#risk band plot

riskband.plot.frac(mu.chain=res1$mu.chain,
                   sigma.chain=res1$sigma.chain,
                   c.oel=data.sample.formatted$c.oel,
                   frac_threshold = user.input$frac_threshold)

riskband.plot.perc(mu.chain=res1$mu.chain,
                   sigma.chain=res1$sigma.chain,
                   c.oel=data.sample.formatted$c.oel,
                   target_perc=user.input$target_perc)

riskband.plot.am(mu.chain=res1$mu.chain,
                 sigma.chain=res1$sigma.chain,
                 c.oel=data.sample.formatted$c.oel)
