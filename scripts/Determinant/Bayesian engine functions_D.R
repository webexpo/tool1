######################################################
#
#
#
# Bayesian functions - Determinants analyses
#
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
#  INPUT : output from data.formatting.SEG
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


          fun.bayes.jags.D <-function( data.formatted , n.iter , updateProgress= NULL , uninformed.prior=FALSE) {

            #data.formatted <-data.sample.formatted
            #n.iter<-5000
            #updateProgress<- NULL

          #n.iter has to be a multiple of 1000

          x <- data.formatted$data

          unique.seed <-data.formatted$seed

          categories <-as.character(data.formatted$var)

          c.oel <-data.formatted$c.oel

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


                          ##priors

                          for (i in 1:n.group) {

                          mu[i] ~dunif(-20,20)

                          tau[i] <-1/(sigma[i]*sigma[i])

                          sigma[i] <-exp(log.sigma[i])

                          log.sigma[i] ~dnorm(-0.1744,2.5523)

                          }

                          ###likelihood

                          for (i in 1:n.obs) {

                          CensorType[i] ~ dinterval( y[i] , censorLimitMat[i,] )

                          y[i] ~dnorm(mu[group[i]],tau[group[i]])

                          }

                          }

                          ",sep='')



          #############################################preparing data for JAGS

          #some inputs

          n.group <-length(unique(categories))

          group <-as.integer(factor(categories))

          n.obs<-length(y)

          yinit<-y
          yinit[]<-NA
          ##initial values
          yinit[data.formatted$leftcensored] <- censorLimitMat[data.formatted$leftcensored,1]-log(2) #init value is LOQ/2
          yinit[data.formatted$rightcensored] <- censorLimitMat[data.formatted$rightcensored,2]-log(2) #init value is LOQ/2
          yinit[data.formatted$intcensored] <- (censorLimitMat[data.formatted$intcensored,1]+censorLimitMat[data.formatted$intcensored,2])/2 #mean of the range


          #initial values

          inits <- list(mu=rep(log(0.3),n.group),log.sigma=rep(log(log(2.5)),n.group),y=yinit, .RNG.name="base::Wichmann-Hill", .RNG.seed=unique.seed)

          }



          if (uninformed.prior==TRUE) {


            ###### JAGS MODELs USING DINTERVAL

            model.2 <-paste("

                            model {


                            ##priors

                            for (i in 1:n.group) {

                            mu[i] ~dunif(-20,20)

                            tau[i] <-1/(sigma[i]*sigma[i])

                            sigma[i] ~ dunif(0,10)

                            }

                            ###likelihood

                            for (i in 1:n.obs) {

                            CensorType[i] ~ dinterval( y[i] , censorLimitMat[i,] )

                            y[i] ~dnorm(mu[group[i]],tau[group[i]])

                            }

                            }

                            ",sep='')



            #############################################preparing data for JAGS

            #some inputs

            n.group <-length(unique(categories))

            group <-as.integer(factor(categories))

            n.obs<-length(y)

            yinit<-y
            yinit[]<-NA
            ##initial values
            yinit[data.formatted$leftcensored] <- censorLimitMat[data.formatted$leftcensored,1]-log(2) #init value is LOQ/2
            yinit[data.formatted$rightcensored] <- censorLimitMat[data.formatted$rightcensored,2]-log(2) #init value is LOQ/2
            yinit[data.formatted$intcensored] <- (censorLimitMat[data.formatted$intcensored,1]+censorLimitMat[data.formatted$intcensored,2])/2 #mean of the range


            #initial values

            inits <- list(mu=rep(log(0.3),n.group),log.sigma=rep(log(2.5),n.group),y=yinit, .RNG.name="base::Wichmann-Hill", .RNG.seed=unique.seed)

          }

          ###################  calling jags

          #parameter definition
          j.mod <- jags.model(file=textConnection(model.2),
                              data = list(y = y,
                                          n.obs = n.obs,
                                          CensorType=CensorType,
                                          censorLimitMat=censorLimitMat,
                                          n.group=n.group,
                                          group=group),
                              inits =inits,
                              n.chains = 1,
                              n.adapt = 100,quiet=TRUE)

          #burnin
          update(j.mod,n.iter=500,progress.bar="none")




          ###### loop for each 1000 iterations

          mu.chain <-matrix(nrow=n.group,ncol=n.iter)

          sigma.chain <-matrix(nrow=n.group,ncol=n.iter)


          for (j in 1:(n.iter/1000)) {

            #updates
            j.out <-coda.samples(model=j.mod,
                                 variable.names=c('mu','sigma'),
                                 n.iter=1000,
                                 thin=1,progress.bar="none")



            #########Numerical resulst of interest

            for (i in 1:n.group) {

              mu.chain[i,(j*1000-999):(j*1000)] <- j.out[[1]][,i]
              sigma.chain[i,(j*1000-999):(j*1000)] <-j.out[[1]][,i+n.group]

            }


            ###progressbar
            if (is.function(updateProgress)) {
              text <- paste0(j*1000,"/",n.iter)
              updateProgress(detail = text)
            }


          }

          index <-data.frame(index=1:n.group,name=names(table((factor(categories)))),stringsAsFactors = FALSE)

          results <-list(mu.chain=mu.chain+log(c.oel),sigma.chain=sigma.chain,group.id=index)

          return(results)

          options(warn = oldw)

          }


#
#
#  STAN function
#
##






fun.bayes.stan.D <-function(data.formatted , n.iter , updateProgress= NULL , mypath ) {



            ##STAN is capricious with the model object, thay may have to be recreated on each new computer


               #n.iter has to be a multiple of 5000

              x <- data.formatted$data

              unique.seed <-data.formatted$seed

              stanmodel.D <-readRDS(paste(mypath,"/stanmodel.D.RDS",sep=""))

              c.oel<-data.formatted$c.oel

              categories <-data.formatted$var

              oldw <- getOption("warn")
              options(warn = -1)


              ##data

              group.index <-as.integer(factor(categories))

              N_group <-length(unique(categories))


            #observed values

            observed_values <-log(as.numeric(x[data.formatted$notcensored])/c.oel)

            N_observed <-length(observed_values)

            group_observed <-group.index[data.formatted$notcensored]


            #left censored values

            leftcensored_values <-log(as.numeric(substring(x[data.formatted$leftcensored],2))/c.oel)

            N_leftcensored <-length(leftcensored_values)

            group_leftcensored <-group.index[data.formatted$leftcensored]


            #right censored values

            rightcensored_values <-log(as.numeric(substring(x[data.formatted$rightcensored],2))/c.oel)

            N_rightcensored <-length(rightcensored_values)

            group_rightcensored <-group.index[data.formatted$rightcensored]


            #interval censored values

            intcensored_left_values <-log(as.numeric(substring(x[data.formatted$intcensored],2,regexpr("-",x[data.formatted$intcensored],fixed=TRUE)-1))/c.oel)

            intcensored_right_values <-log(as.numeric(substring(x[data.formatted$intcensored],regexpr("-",x[data.formatted$intcensored],fixed=TRUE)+1,nchar(x[data.formatted$intcensored])-1))/c.oel)

            N_intcensored <-length(intcensored_left_values)

            group_intcensored <-group.index[data.formatted$intcensored]


            ###data list

            dataList = list( "observed_values" = array(observed_values,dim=length(observed_values)) ,
                             "N_observed" = N_observed,
                             "group_observed" = array(group_observed,dim=length(group_observed)) ,
                             "leftcensored_values" = array(leftcensored_values,dim=length(leftcensored_values)),
                             "N_leftcensored" = N_leftcensored,
                             "group_leftcensored" = array(group_leftcensored,dim=length(group_leftcensored)) ,
                             "rightcensored_values" = array(rightcensored_values,dim=length(rightcensored_values)),
                             "N_rightcensored" = N_rightcensored,
                             "group_rightcensored" = array(group_rightcensored,dim=length(group_rightcensored)) ,
                             "intcensored_left_values" = array(intcensored_left_values,dim=length(intcensored_left_values)),
                             "intcensored_right_values" = array(intcensored_right_values,dim=length(intcensored_right_values)),
                             "N_intcensored" = N_intcensored,
                             "group_intcensored" = array(group_intcensored,dim=length(group_intcensored)) )


            ##samples

            mu.chain <-matrix(nrow=N_group,ncol=n.iter)

            sigma.chain <-matrix(nrow=N_group,ncol=n.iter)


            for (j in 1:(n.iter/5000)) {

                #updates
              stanFit = sampling( object=stanmodel.D , data=dataList ,
                                  chains=1 , iter=7000 , warmup=2000 , thin=1,
                                  show_messages=FALSE ,seed = unique.seed+j, verbose=FALSE) ####uncertainty about the seed behavior for STAN

              samples <-extract(stanFit,c("group_mean","group_sigma"),permuted = FALSE, inc_warmup = FALSE)

              #########Numerical resulst of interest

              for (i in 1:N_group) {

                mu.chain[i,((j-1)*5000+1):(j*5000)] <-samples[,1,i]
                sigma.chain[i,((j-1)*5000+1):(j*50000)] <-samples[,1,i+N_group]

              }


              ###progressbar
              if (is.function(updateProgress)) {
                text <- paste0(i*5000,"/",n.iter)
                updateProgress(detail = text)
              }


            }


            group.index <-data.frame(name=names(table(factor(categories))),stringsAsFactors=F)

            group.index$index <-1:length(group.index[,1])


            return(list(mu.chain=mu.chain+log(c.oel),
                        sigma.chain=sigma.chain,group.id=group.index))

            options(warn = oldw)

            }

#
#
#
##################################### MCGILL FUNCTIONS
#
#
#


fun.bayes.webexpo.D <-function(data.formatted , n.iter , updateProgress= NULL , mypath) {

  #data.formatted<-data.sample.formatted
  #mypath <-"D:/Dropbox/bureau/RStudio/expostats/shinyapps/expostats functions 2018"

 # n.iter<-10



setwd(mypath)

source("Common functions/data-summary.R")

source("Common functions/fcts.R")

source("Common functions/model-SEG-informedvar.R")

source("Determinant specific functions/model-SEG-informedvar-MultiLevel.R")


  x <- data.formatted$data

  unique.seed <-data.formatted$seed

  c.oel<-data.formatted$c.oel

  categories <-data.formatted$var

oldw <- getOption("warn")
options(warn = -1)

  group.index <-as.numeric(factor(categories))
  group.index.level <-unique(categories)
  group.index.level.num <-as.numeric(factor(group.index.level))
  N_group <-length(group.index.level)
##data

#observed values

observed_values <-as.numeric(x[data.formatted$notcensored])/c.oel

observed_group <-group.index[data.formatted$notcensored]

#left censored values

leftcensored_values <-as.numeric(substring(x[data.formatted$leftcensored],2))/c.oel

leftcensored_group <-group.index[data.formatted$leftcensored]

#right censored values

rightcensored_values <-as.numeric(substring(x[data.formatted$rightcensored],2))/c.oel

rightcensored_group <-group.index[data.formatted$rightcensored]

#interval censored values

intcensored_left_values <-as.numeric(substring(x[data.formatted$intcensored],2,regexpr("-",x[data.formatted$intcensored],fixed=TRUE)-1))/c.oel

intcensored_right_values <-as.numeric(substring(x[data.formatted$intcensored],regexpr("-",x[data.formatted$intcensored],fixed=TRUE)+1,nchar(x[data.formatted$intcensored])-1))/c.oel

intcensored_group <-group.index[data.formatted$intcensored]



## bayesian calculations



set.generator("MersenneTwister", initialization = "init2002", resolution = 32, seed = substring(unique.seed,5))

res <-SEG.informedvar.MultiLevel(y=observed_values,
                                 lt=leftcensored_values,
                                 gt=rightcensored_values,
                                 interval.lower=intcensored_left_values,
                                 interval.upper=intcensored_right_values,
                                 level.index=observed_group,
                                 level.index.lt=leftcensored_group,
                                 level.index.gt=rightcensored_group,
                                 level.index.interval=intcensored_group,
                                 n.chains=1, n.iter=n.iter, n.burnin=5, n.thin=1, monitor.burnin=F,
                                 mu.lower=rep(-20, N_group),
                                 mu.upper=rep(20, N_group),
                                 log.sigma.mu=rep(-0.1744, N_group),
                                 log.sigma.prec=rep(2.5523, N_group),
                                             init.mu   =matrix(log(0.3), ncol=1, nrow=N_group),
                                             init.sigma=matrix(log(2.5), ncol=1, nrow=N_group),
                                             outcome.is.logNormally.distributed=T,
                                             me.sd.range=numeric(0), cv.range=numeric(0),
                                             save.RData=F, RData.dir='c:/users/jerome')



mu.chain <-matrix(nrow=N_group,ncol=n.iter)

sigma.chain <-matrix(nrow=N_group,ncol=n.iter)

group.id <-data.frame(name=names(table(factor(categories))),stringsAsFactors=F)

group.id$index <-1:length(group.id[,1])



for (i in 1:N_group) {

  mu.chain[i,] <-(unlist(res$sample$mu)+log(c.oel))[((i-1)*n.iter+1):(i*n.iter)]
  sigma.chain[i,] <-(unlist(res$sample$sd))[((i-1)*n.iter+1):(i*n.iter)]

}


results <-list(mu.chain=mu.chain,sigma.chain=sigma.chain,group.id=group.id)



return(results)

options(warn = oldw)

}

##################### formatting output for a single cell

bayesian.output.B.single <- function(bayesian.output.D , cat) {

  mu.chain <-bayesian.output.D$mu.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat],]

  sigma.chain <-bayesian.output.D$sigma.chain[bayesian.output.D$group.id$index[bayesian.output.D$group.id$name==cat],]

  return(list(mu.chain=mu.chain,
              sigma.chain=sigma.chain,id=cat))

}

