################################################################
#
#
#  Example of running the complete between-worker analysis tool
#
################################################################

#
#
# V1.00   20 march 2018
#
#

######### sourcing the scripts

setwd("C:/jerome/Dropbox/bureau/RStudio/expostats/shinyapps/2018 expostat apps/Tool2En/scripts")


##### between worker specific functions

source("Betweenworker specific functions/Data formatting functions_B.R")

source("Betweenworker specific functions/Generating a random sample functions_B.R")

source("Betweenworker specific functions/Descriptive graphs functions_B.R")

source("Betweenworker specific functions/Bayesian engine functions_B.R")

source("Betweenworker specific functions/Main graph functions_B.R")

#### common functions


source("Common functions/Simple censored imputation functions.R")

source("Common functions/Descriptive numerical output functions.R")

source("Common functions/Descriptive graphs functions.R")

source("Common functions/script density comparison.R")

source("Common functions/Numerical output functions.R")

source("Common functions/Main graph functions.R")

#################### Creating a sample ###########################


data.sample <- fun.gener.censored.betweenworker(n.worker = 5,
                                                           n.days = rep(10,5), #vecteur de longuer le nombre de travailleurs
                                                           no.censoring = FALSE,
                                                           perc.lowerthan = 20,
                                                           perc.greaterthan = 10,
                                                           perc.between = 10,
                                                           gm = 0.3,
                                                           gsd = 2.5,
                                                           rho = 0.5,
                                                           error  = "",
                                                           left_factor = 1.5,
                                                           right_factor = 1/1.5,
                                                           int_factor = 1.5)$string


###alternatively : the default expostats sample with some uncleanliness

data.sample<- '40.1\tworker-10\n28.5\tworker-2\n48.5\tworker-9\n87\tworker-1\n6.73\tworker-5\n105\tworker-7\n64.6\tworker-1\n87.5\tworker-6\n6.38\tworker-2\n68.6\tworker-6\n41.4\tworker-10\n92.2\tworker-8\n19.1\tworker-2\n67.9\tworker-8\n345\tworker-1\n63.7\tworker-4\n17.6\tworker-3\n89.1\tworker-7\n59.8\tworker-9\n87.4\tworker-1\n89.2\tworker-4\n82.3\tworker-8\n12.6\tworker-5\n198\tworker-7\n25.1\tworker-3'


################ initial data formating

data.sample.formatted <-data.formatting.B(data.in=data.sample,oel=300, oel.mult = 1)

############## simple censored imputation

data.sample.imputed <-simple.censored.treatment(
              observations.formatted=data.sample.formatted$data$x ,
              notcensored=data.sample.formatted$notcensored ,
              leftcensored=data.sample.formatted$leftcensored ,
              rightcensored=data.sample.formatted$rightcensored ,
              intcensored=data.sample.formatted$intcensored)

#############  descriptive statistics

View(fun.desc.stat(data.sample.imputed,data.sample.formatted$c.oel))


#############  descriptive graphs

### useable for worker specific QQplots

X <- data.sample.formatted

X.worker <- select.worker.formatted(data.formatted= X ,
                                    worker.id ="worker-2")

data.imputed.worker <-simple.censored.treatment(observations.formatted=X.worker$data$x,
                          notcensored=X.worker$notcensored,
                          leftcensored=X.worker$leftcensored,
                          rightcensored=X.worker$rightcensored,
                          intcensored=X.worker$intcensored)

Y <- data.imputed.worker

p <- fun.qqplot(data.simply.imputed= Y  ,
                notcensored = X.worker$notcensored)


### for the whole group, one colour per worker

fun.qqplot.group(data.sample.imputed ,
                            notcensored = data.sample.formatted$notcensored ,
                            workers = data.sample.formatted$data$worker
                            )




################## bayesian output - jags
ptm <- proc.time()
res1 <-fun.bayes.jags.B(data.formatted=data.sample.formatted,updateProgress= NULL,n.iter=50000)
proc.time() - ptm

ptm <- proc.time()
res2 <-fun.bayes.stan.B(data.formatted=data.sample.formatted,updateProgress= NULL,
                      mypath <-"C:/jerome/Dropbox/bureau/RStudio/expostats/shinyapps/unified functions/Between-worker")
proc.time() - ptm

ptm <- proc.time()
res3 <-fun.bayes.webexpo.B(data.formatted=data.sample.formatted,updateProgress= NULL,
                      mypath <-"C:/jerome/Dropbox/bureau/RStudio/expostats/shinyapps/unified functions/Between-worker")
proc.time() - ptm


##comparing densities


fun.compare.dens(jags.chain=res1$mu ,stan.chain=res2$mu,webexpo.chain=res3$mu)


fun.compare.dens(jags.chain=res1$sw,stan.chain=res2$sw,webexpo.chain=res3$sw)

fun.compare.dens(jags.chain=res1$sb,stan.chain=res2$sb,webexpo.chain=res3$sb)

fun.compare.dens(jags.chain=res1$mu.workers[res1$worker.index$numberinoutput[res1$worker.index$worker=="worker-5"],],
                                       stan.chain=res2$mu.workers[res2$worker.index$numberinoutput[res2$worker.index$worker=="worker-5"],],
                                      webexpo.chain=res2$mu.workers[res2$worker.index$numberinoutput[res2$worker.index$worker=="worker-5"],])



################## numerical output - jags


##### EXEMPLE OF INPUT SET

user.input.B <-list( conf=90 , psi = 30 , frac_threshold = 5 , target_perc = 95 , rappap_cover = 80 , wwct = 0.2)

##numerical results from the ANOVA analysis
num.res.B <-all.numeric.B(bayesian.output.B=res1,user.input.B=user.input.B)

##numerical results for the whole group, not taking worker into account, can also be used for worker specific results
num.res.group <-all.numeric(mu.chain=res1$mu ,
                            sigma.chain=res1$stot ,
                            conf=user.input.B$conf ,
                            c.oel=data.sample.formatted$c.oel ,
                            frac_threshold=user.input.B$frac_threshold ,
                            target_perc=user.input.B$target_perc)


############### graphical ouput - USEABLe for the whole group or restricted to a single worker

#calendar plot

calendar.plot(frac.est = num.res.group$frac$est)

#sequential plot

sequential.plot.frac(gm=num.res.group$gm$est ,
                     gsd=num.res.group$gsd$est ,
                     frac=num.res.group$frac$est ,
                     c.oel=data.sample.formatted$c.oel ,
                     data.simply.imputed=data.sample.imputed)

sequential.plot.perc(gm=num.res.group$gm$est ,
                     gsd=num.res.group$gsd$est ,
                     perc=num.res.group$perc$est ,
                     target_perc=user.input.B$target_perc,
                     c.oel=data.sample.formatted$c.oel ,
                     data.simply.imputed=data.sample.imputed)

sequential.plot.am(gm=num.res.group$gm$est ,
                   gsd=num.res.group$gsd$est ,
                   am=num.res.group$am$est ,
                   c.oel=data.sample.formatted$c.oel ,
                   data.simply.imputed=data.sample.imputed)


#distribution plot

distribution.plot.frac(gm=num.res.group$gm$est ,
                       gsd=num.res.group$gsd$est ,
                       frac=num.res.group$frac$est ,
                       c.oel=data.sample.formatted$c.oel)

distribution.plot.perc(gm=num.res.group$gm$est ,
                       gsd=num.res.group$gsd$est ,
                       perc=num.res.group$perc$est ,
                       target_perc=user.input.B$target_perc,
                       c.oel=data.sample.formatted$c.oel)

distribution.plot.am(gm=num.res.group$gm$est ,
                     gsd=num.res.group$gsd$est ,
                     am=num.res.group$am$est ,
                     c.oel=data.sample.formatted$c.oel)

#risk band plot

riskband.plot.frac(mu.chain=res1$mu,
                   sigma.chain=res1$stot,
                   c.oel=data.sample.formatted$c.oel,
                   frac_threshold = user.input.B$frac_threshold)

riskband.plot.perc(mu.chain=res1$mu,
                   sigma.chain=res1$stot,
                   c.oel=data.sample.formatted$c.oel,
                   target_perc=user.input.B$target_perc)

riskband.plot.am(mu.chain=res1$mu,
                 sigma.chain=res1$stot,
                 c.oel=data.sample.formatted$c.oel)


#riskmeter

dessinerRisqueMetre(actualProb=num.res.group$frac.risk, minProbUnacceptable=user.input.B$psi)


############### graphical ouput - FOR THE BETWEEN WORKER / ANOVA ANALYSIS


##### boxplots by worker

boxplot.by.worker(data.formatted = data.sample.formatted ,
                  data.simply.imputed = data.sample.imputed ,
                  bayesian.output.B = res1 ,
                  worker.list=data.sample.formatted$data$worker)

X <- data.sample.formatted

Y <- data.sample.imputed

Z <- res1

  Freq.table <- data.frame(table(X$data$worker))

  names(Freq.table) <-c("nom","n")

  ListOfWorkers <-as.character(Freq.table$nom)


p1 <-boxplot.by.worker(data.formatted = X ,
                       data.simply.imputed = Y ,
                       bayesian.output.B = Z ,
                       worker.list = ListOfWorkers)

suppressWarnings(print(p1))


######individualrisk band plots

individual.riskband.plot.frac(mu.chain=res1$mu,
                   sw.chain=res1$sw,
                   sb.chain=res1$sb,
                   c.oel=data.sample.formatted$c.oel,
                   frac_threshold = user.input.B$frac_threshold,
                   psi = user.input.B$psi)

individual.riskband.plot.perc(mu.chain=res1$mu,
                              sw.chain=res1$sw,
                              sb.chain=res1$sb,
                              c.oel=data.sample.formatted$c.oel,
                              target_perc = user.input.B$target_perc,
                              psi = user.input.B$psi)

individual.riskband.plot.am(mu.chain=res1$mu,
                              sw.chain=res1$sw,
                              sb.chain=res1$sb,
                              c.oel=data.sample.formatted$c.oel,
                              psi = user.input.B$psi)



################# individual sequential PLOT

X <- res1

Y <- data.sample.formatted

Z <- user.input.B

mu.chain.worker <- X$mu.workers[X$worker.index$numberinoutput[X$worker.index$worker=="worker-1"],]

sigma.chain.worker <-X$sw

num.res.worker <- all.numeric(mu.chain=mu.chain.worker ,
                      sigma.chain=sigma.chain.worker ,
                      conf=Z$conf ,
                      c.oel=Y$c.oel ,
                      frac_threshold=Z$frac_threshold ,
                      target_perc=Z$target_perc)


##imputation for selected worker

Y.worker <- select.worker.formatted(data.formatted= Y ,
                                    worker.id ="worker-1")

data.imputed.worker <-simple.censored.treatment(observations.formatted=Y.worker$data$x,
                                                notcensored=Y.worker$notcensored,
                                                leftcensored=Y.worker$leftcensored,
                                                rightcensored=Y.worker$rightcensored,
                                                intcensored=Y.worker$intcensored)


Z <- data.imputed.worker

X <- num.res.worker

graph2 <- sequential.plot.frac(gm=X$gm$est ,
                               gsd=X$gsd$est ,
                               frac=X$frac$est ,
                               c.oel=Y.worker$c.oel ,
                               data.simply.imputed=Z)

suppressWarnings(print(graph2))
