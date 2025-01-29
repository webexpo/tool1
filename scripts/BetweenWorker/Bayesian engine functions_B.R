######################################################
#
#
#
# Bayesian functions - between worker analysis tool
#
# V1.00   20 march 2018
# V1.01   4 june 2018 correction of issue with regexpr
#
#
# 3 versions of the fonction exist : JAG / STAN / Webexpo
#
#  The STAN function require using a saved R object created in advance
#
######################################################


##
#
#  INPUT : output from data.formatting.B
#
##

library(rjags)
#library(rstan)
library("randtoolbox", character.only = TRUE)


#
#
#  JAGS function
#
##


          fun.bayes.jags.B <-function(data.formatted,updateProgress,n.iter,uninformed.prior=FALSE) {


          #n.iter multiple of 1000

          c.oel <-data.formatted$c.oel

          x <- data.formatted$data$x

          unique.seed <-data.formatted$seed

          oldw <- getOption("warn")
          options(warn = -1)

          ###creating the inout for the censoring functions in JAGS

          ##must create a matrix with n rows and 2 columnns
          ##column 1 is for the left censoring points (<)
          ##column 2 is for the right censoring points (>)

          ## values of the dinterval function
          # 0  : < to the left censoring point
          # 1  : between the 2 censoring points
          # 2 : > to the right censoring points

          ##coding the dinterval values

          #for left censored points :
          #dinterval is 0
          #left is the censoring value
          #right is 50 (value we are sure any observation is smaller than)

          #for right censored points :
          #dinterval is 2
          #left is -50 (value we are sure any observation is greater than)
          #right is the censoring value

          #for interval censored points :
          #dinterval is 1
          #left is left censoring point
          #right is right censoring value

          #for observed points :
          #dinterval is 1
          #left is is -50 (value we are sure any observation is greater than)
          #right is 50 (value we are sure any observation is smaller than)



          #censorLimitMat is the matrix of censoring points

          #x/Y should be set to NA when censored



          ##Nas automatically generated
          y <- log(as.numeric(x)/c.oel)  ##Nas automatically generated

          ##creating the dinterval vector : CensorType

          CensorType <- rep(1,length(x))

          #left censored
          CensorType[grepl('<' , x, fixed = TRUE)] <-0

          #right censored
          CensorType[grepl('>' , x, fixed = TRUE)] <-2

          ## creating the matrix of limits
          censorLimitMat <- matrix(nrow = length(y) , ncol = 2)
          censorLimitMat[,1] <-rep(-50,length(y))
          censorLimitMat[,2] <-rep(50,length(y))


          #left censored
          censorLimitMat[data.formatted$leftcensored,1] <-log(as.numeric(substring(x[data.formatted$leftcensored],2))/c.oel)
          #right censored
          censorLimitMat[data.formatted$rightcensored,2] <-log(as.numeric(substring(x[data.formatted$rightcensored],2))/c.oel)
          #interval censored
          censorLimitMat[data.formatted$intcensored,1] <-log(as.numeric(substring(x[data.formatted$intcensored],2,regexpr("-",x[data.formatted$intcensored],fixed=TRUE)-1))/c.oel)
          censorLimitMat[data.formatted$intcensored,2] <-log(as.numeric(substring(x[data.formatted$intcensored],regexpr("-",x[data.formatted$intcensored],fixed=TRUE)+1,nchar(x[data.formatted$intcensored])-1))/c.oel)


          #
          #
          ######################################## JAGS MODEL
          #
          #


          if (uninformed.prior==FALSE) {

          ###### JAGS MODELs USING DINTERVAL

          model.2 <-paste("

 model {


                          #random effect / hierarchy

                          for (i in 1:n.group){
                          worker.effect[i] ~ dnorm(0, tau.worker) # Random intercepts
                          }

                          tau.worker <-1/(sigma.worker*sigma.worker)

                          sigma.worker <-exp(log.sigma.worker)

                          log.sigma.worker ~dnorm(-0.8786,1.634)


                          #priors on fixed effects

                          tau <-1/(sigma*sigma)

                          sigma <-exp(log.sigma)

                          log.sigma ~dnorm(-0.4106,1.9002)

                          mu.group ~dunif(-20,20)

                          ###likelihood

                          for (i in 1:n.obs) {

                          CensorType[i] ~ dinterval( y[i] , censorLimitMat[i,] )

                          y[i] ~dnorm(mu[i],tau)

                          mu[i] <- mu.group+worker.effect[groups[i]]

                          }

          }

                          ",sep='')



          #############################################preparing data for JAGS

          #some inputs

          n.obs<-length(y)

          n.group <-  length(unique(data.formatted$data$worker))
          groups <-as.integer(factor(data.formatted$data$worker))

          yinit<-y
          yinit[]<-NA
          ##initial values
          yinit[data.formatted$leftcensored] <- censorLimitMat[data.formatted$leftcensored,1]-log(2) #init value is LOQ/2
          yinit[data.formatted$rightcensored] <- censorLimitMat[data.formatted$rightcensored,2]-log(2) #init value is LOQ/2
          yinit[data.formatted$intcensored] <- (censorLimitMat[data.formatted$intcensored,1]+censorLimitMat[data.formatted$intcensored,2])/2 #mean of the range


          #initial values

          inits <- list(log.sigma.worker=log(log(2)),
                        log.sigma=log(log(2)),
                        mu.group=log(0.3),y=yinit, .RNG.name="base::Wichmann-Hill", .RNG.seed=unique.seed)

          }


          if (uninformed.prior==TRUE) {

            ###### JAGS MODELs USING DINTERVAL

            model.2 <-paste("

                            model {


                            #random effect / hierarchy

                            for (i in 1:n.group){
                            worker.effect[i] ~ dnorm(0, tau.worker) # Random intercepts
                            }

                            tau.worker <-1/(sigma.worker*sigma.worker)

                            sigma.worker ~ dunif(0,10)


                            #priors on fixed effects

                            tau <-1/(sigma*sigma)

                            sigma ~ dunif(0,10)

                            mu.group ~dunif(-20,20)

                            ###likelihood

                            for (i in 1:n.obs) {

                            CensorType[i] ~ dinterval( y[i] , censorLimitMat[i,] )

                            y[i] ~dnorm(mu[i],tau)

                            mu[i] <- mu.group+worker.effect[groups[i]]

                            }

                            }

                            ",sep='')



            #############################################preparing data for JAGS

            #some inputs

            n.obs<-length(y)

            n.group <-  length(unique(data.formatted$data$worker))
            groups <-as.integer(factor(data.formatted$data$worker))

            yinit<-y
            yinit[]<-NA
            ##initial values
            yinit[data.formatted$leftcensored] <- censorLimitMat[data.formatted$leftcensored,1]-log(2) #init value is LOQ/2
            yinit[data.formatted$rightcensored] <- censorLimitMat[data.formatted$rightcensored,2]-log(2) #init value is LOQ/2
            yinit[data.formatted$intcensored] <- (censorLimitMat[data.formatted$intcensored,1]+censorLimitMat[data.formatted$intcensored,2])/2 #mean of the range


            #initial values

            inits <- list(sigma.worker=log(2),
                          sigma=log(2),
                          mu.group=log(0.3),y=yinit, .RNG.name="base::Wichmann-Hill", .RNG.seed=unique.seed)

          }


          ###################  calling jags

          #parameter definition
          j.mod <- jags.model(file=textConnection(model.2),
                              data = list(y = y,
                                          n.obs = n.obs,
                                          n.group=n.group,
                                          groups=groups,
                                          CensorType=CensorType,
                                          censorLimitMat=censorLimitMat),
                              inits =inits,
                              n.chains = 1,
                              n.adapt = 100)

          #burnin
          update(j.mod,n.iter=5000)




          ###### loop for each 1000 iterations

          mu <-numeric(0)
          sw <-numeric(0)
          sb <-numeric(0)

          mu.workers <-matrix(nrow=n.group,ncol=n.iter)


          for (j in 1:(n.iter/1000)) {

            #updates
            j.out <-coda.samples(model=j.mod,
                                 variable.names=c('mu.group','sigma','sigma.worker','worker.effect'),
                                 n.iter=1000,
                                 thin=1,
                                 progress.bar="none")



            #########Numerical resulst of interest

            mu <-c(mu,j.out[[1]][,1]+log(c.oel))
            sw <-c(sw,j.out[[1]][,2])
            sb <-c(sb,j.out[[1]][,3])

            for (i in 1:n.group) mu.workers[i,(j*1000-999):(j*1000)] <-  j.out[[1]][,i+3]+j.out[[1]][,1]+log(c.oel)

            worker.index <-data.frame(worker=names(table(factor(data.formatted$data$worker))),stringsAsFactors=F)

            worker.index$numberinoutput <-1:length(worker.index[,1])



            ###progressbar
            if (is.function(updateProgress)) {
              text <- paste0(j*1000,"/",n.iter)
              updateProgress(detail = text)
            }


          }

          results <-list(mu=mu,
                         sw=sw,
                         sb=sb,
                         stot=sqrt(sb^2+sw^2),
                         mu.workers=mu.workers,
                         worker.index=worker.index)



          return(results)

          options(warn = oldw)

          }


#
#
#  STAN function
#
##






fun.bayes.stan.B <-function(data.formatted , mypath , updateProgress= NULL , n.iter ) {

              #data.formatted<-data.sample.formatted
              #mypath <-"C:/jerome/Dropbox/bureau/RStudio/expostats/shinyapps/unified functions/Between-worker"

              #n.iter multiple of 10 000

              c.oel <-data.formatted$c.oel

              x <- data.formatted$data$x

              unique.seed <-data.formatted$seed

              stanmodel.B <-readRDS(paste(mypath,"/stanmodel.B.RDS",sep=""))


              oldw <- getOption("warn")
              options(warn = -1)


            ##data

              data.formatted$worker.index <-as.integer(factor(data.formatted$data$worker))

              N_workers <-length(unique(data.formatted$data$worker))

            #observed values

              observed_values <-log(as.numeric(x[data.formatted$notcensored])/c.oel)

              N_observed <-length(observed_values)

              worker_observed <-data.formatted$worker.index[data.formatted$notcensored]



            #left censored values

            leftcensored_values <-log(as.numeric(substring(x[data.formatted$leftcensored],2))/c.oel)

            N_leftcensored <-length(leftcensored_values)

            worker_leftcensored <-data.formatted$worker.index[data.formatted$leftcensored]

            #right censored values

            rightcensored_values <-log(as.numeric(substring(x[data.formatted$rightcensored],2))/c.oel)

            N_rightcensored <-length(rightcensored_values)

            worker_rightcensored <-data.formatted$worker.index[data.formatted$rightcensored]


            #interval censored values

            intcensored_left_values <-log(as.numeric(substring(x[data.formatted$intcensored],2,regexpr("-",x[data.formatted$intcensored],fixed=TRUE)-1))/c.oel)

            intcensored_right_values <-log(as.numeric(substring(x[data.formatted$intcensored],regexpr("-",x[data.formatted$intcensored],fixed=TRUE)+1,nchar(x[data.formatted$intcensored])-1))/c.oel)

            N_intcensored <-length(intcensored_left_values)

            worker_intcensored <-data.formatted$worker.index[data.formatted$intcensored]


            ###data list

            dataList = list( "observed_values" = array(observed_values,dim=length(observed_values)) ,
                             "N_observed" = N_observed,
                             "worker_observed" = array(worker_observed,dim=length(worker_observed)) ,
                             "leftcensored_values" = array(leftcensored_values,dim=length(leftcensored_values)),
                             "N_leftcensored" = N_leftcensored,
                             "worker_leftcensored" = array(worker_leftcensored,dim=length(worker_leftcensored)) ,
                             "rightcensored_values" = array(rightcensored_values,dim=length(rightcensored_values)),
                             "N_rightcensored" = N_rightcensored,
                             "worker_rightcensored" = array(worker_rightcensored,dim=length(worker_rightcensored)) ,
                             "intcensored_left_values" = array(intcensored_left_values,dim=length(intcensored_left_values)),
                             "intcensored_right_values" = array(intcensored_right_values,dim=length(intcensored_right_values)),
                             "N_intcensored" = N_intcensored,
                             "worker_intcensored" = array(worker_intcensored,dim=length(worker_intcensored)) )

            ######loop for progress bar

            mu <-numeric(0)
            sw <-numeric(0)
            sb <-numeric(0)

            mu.workers <-matrix(nrow=N_workers,ncol=n.iter)

            for (j in 1:(n.iter/10000)) {

              #updates
              stanFit <- sampling( object=stanmodel.B , data=dataList ,
                                   chains=1 , iter=15000 , warmup=5000 , thin=1, show_messages=FALSE )

              samples <-extract(stanFit,c("mu_group","sigma_between","sigma_within","worker_effect"),permuted = FALSE, inc_warmup = FALSE)


              #filling samples


              mu <-c(mu,samples[,1,1])
              sw <-c(sw,samples[,1,3])
              sb <-c(sb,samples[,1,2])

              for (i in 1:N_workers) {mu.workers[i,((j-1)*10000+1):(j*10000)] <-  samples[,1,i+3] + samples[,1,"mu_group"] }



              ###progressbar
              if (is.function(updateProgress)) {
                text <- paste0(i*10000,"/",n.iter)
                updateProgress(detail = text)
              }


            }

            worker.index <-data.frame(worker=names(table(factor(data.formatted$data$worker))),stringsAsFactors=F)

            worker.index$numberinoutput <-1:length(worker.index[,1])



            return(list(mu=mu+log(c.oel),
                        sw=sw,
                        sb=sb,
                        stot=sqrt(sb^2+sw^2),
                        mu.workers=mu.workers+log(c.oel),
                        worker.index=worker.index))

            options(warn = oldw)

            }

#
#
#
##################################### MCGILL FUNCTIONS
#
#
#


fun.bayes.webexpo.B <-function(data.formatted , mypath , updateProgress= NULL , n.iter) {


setwd(mypath)

source("data-summary.R")

source("fcts.R")

source("model-Between-worker.R")


c.oel <-data.formatted$c.oel

x <- data.formatted$data$x

unique.seed <-data.formatted$seed


oldw <- getOption("warn")
options(warn = -1)


##data

#observed values

observed_values <-as.numeric(x[data.formatted$notcensored])/c.oel

observed_workers <-data.formatted$data$worker[data.formatted$notcensored]

#left censored values

leftcensored_values <-as.numeric(substring(x[data.formatted$leftcensored],2))/c.oel

leftcensored_workers <-data.formatted$data$worker[data.formatted$leftcensored]

#right censored values

rightcensored_values <-as.numeric(substring(x[data.formatted$rightcensored],2))/c.oel

rightcensored_workers <-data.formatted$data$worker[data.formatted$rightcensored]

#interval censored values

intcensored_left_values <-as.numeric(substring(x[data.formatted$intcensored],2,regexpr("-",x[data.formatted$intcensored],fixed=TRUE)-1))/c.oel

intcensored_right_values <-as.numeric(substring(x[data.formatted$intcensored],regexpr("-",x[data.formatted$intcensored],fixed=TRUE)+1,nchar(x[data.formatted$intcensored])-1))/c.oel

intcensored_workers <-data.formatted$data$worker[data.formatted$intcensored]

## bayesian calculations



set.generator("MersenneTwister", initialization = "init2002", resolution = 32, seed = substring(unique.seed,5))

res <-Between.worker(y=observed_values, worker=observed_workers,
                     lt=leftcensored_values, worker.lt=leftcensored_workers,
                     gt=rightcensored_values, worker.gt=rightcensored_workers,
                     interval.lower=intcensored_left_values, interval.upper=intcensored_right_values, worker.interval=intcensored_workers,
                     n.chains=1, n.iter=n.iter, n.burnin=5000, n.thin=1, monitor.burnin=F,
                     log.sigma.between.mu=-0.8786, log.sigma.between.prec=1.634,
                     log.sigma.within.mu=-0.4106, log.sigma.within.prec=1.9002,
                     mu.overall.lower=-20, mu.overall.upper=20,
                     outcome.is.logNormally.distributed=T,
                     use.uniform.prior.on.sds=F, sigma.between.range=c(0,100), sigma.within.range=c(0,100),
                     me.sd.range=numeric(0), cv.range=numeric(0),
                     init.mu.overall=log(0.3),
                     init.sigma.within=log(2),
                     save.RData=F, RData.dir='c:/users/jerome')


sw.chain <-res$sample$sigma.within
sb.chain <-res$sample$sigma.between
mu.chain <-res$sample$mu.overall+log(c.oel)

worker.index <-data.frame(worker=names(table(factor(data.formatted$data$worker))),stringsAsFactors=F)

worker.index$numberinoutput <-1:length(worker.index[,1])

mu.workers <-matrix(res$sample$mu.worker+log(c.oel),nrow=length(worker.index[,1]),ncol=n.iter)

results <-list(mu=mu.chain,
               sw=sw.chain,
               sb=sb.chain,
               stot=sqrt(sb^2+sw^2),
               mu.workers=mu.workers,
               worker.index=worker.index)



return(results)

options(warn = oldw)

}
