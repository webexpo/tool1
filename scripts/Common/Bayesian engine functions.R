######################################################
#
#
#
# Bayesian functions
#
# V1.00   20 march 2018
# V1.01   7 mai 2018   ### adding uninformative option in the JAGS functions
# V1.02   4 june 2018 correction of issue with regexpr
#
#
# 3 versions of the fonction exist : JAG / STAN / Webexpo
#
#  The STAN function require using a saved R object created in advance. A bit capricious, recommand recrating the object on each computer used.
#
######################################################


##
#
#  INPUT :
# vector of  observations
# vector indicating not censored data
# vector indicating left censored data
# vector indicating right censored data
# vector indicating interval censored censored data
# seed value
# corrected OEL
# n.iterations
# for STAN path to model object, for Webexpo path to McGill functions
##

#library(rjags)
#library(rstan)
#library(randtoolbox)

#
#
#  JAGS function
#
##


fun.bayes.jags <-function( observations , notcensored, leftcensored , rightcensored ,
                            intcensored , seed , c.oel , n.iter , uninformed.prior=FALSE , updateProgress= NULL) {

          #n.iter has to be a multiple of 1000

          x <- observations

          unique.seed <-seed

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
          censorLimitMat[leftcensored,1] <-log(as.numeric(substring(x[leftcensored],2))/c.oel)
          #right censored
          censorLimitMat[rightcensored,2] <-log(as.numeric(substring(x[rightcensored],2))/c.oel)
          #interval censored
          censorLimitMat[intcensored,1] <-log(as.numeric(substring(x[intcensored],2,regexpr("-",x[intcensored],fixed=TRUE)-1))/c.oel)
          censorLimitMat[intcensored,2] <-log(as.numeric(substring(x[intcensored],regexpr("-",x[intcensored],fixed=TRUE)+1,nchar(x[intcensored])-1))/c.oel)

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

                          mu ~dunif(-20,20)

                          tau <-1/(sigma*sigma)

                          sigma <-exp(log.sigma)

                          log.sigma ~dnorm(-0.1744,2.5523)

                          ###likelihood

                          for (i in 1:n.obs) {

                          CensorType[i] ~ dinterval( y[i] , censorLimitMat[i,] )

                          y[i] ~dnorm(mu,tau)

                          }

                          }

                          ",sep='')



          #############################################preparing data for JAGS

          #some inputs

          n.obs<-length(y)

          yinit<-y
          yinit[]<-NA
          ##initial values
          yinit[leftcensored] <- censorLimitMat[leftcensored,1]-log(2) #init value is LOQ/2
          yinit[rightcensored] <- censorLimitMat[rightcensored,2]-log(2) #init value is LOQ/2
          yinit[intcensored] <- (censorLimitMat[intcensored,1]+censorLimitMat[intcensored,2])/2 #mean of the range


          #initial values

          inits <- list(mu=log(0.3),log.sigma=log(log(2.5)),y=yinit, .RNG.name="base::Wichmann-Hill", .RNG.seed=unique.seed)

          }


          if (uninformed.prior==TRUE) {

            ###### JAGS MODELs USING DINTERVAL

            model.2 <-paste("

                            model {


                            ##priors

                            mu ~dunif(-20,20)

                            tau <-1/(sigma*sigma)

                            sigma ~ dunif(0,10)

                            ###likelihood

                            for (i in 1:n.obs) {

                            CensorType[i] ~ dinterval( y[i] , censorLimitMat[i,] )

                            y[i] ~dnorm(mu,tau)

                            }

                            }

                            ",sep='')



          #############################################preparing data for JAGS

          #some inputs

          n.obs<-length(y)

          yinit<-y
          yinit[]<-NA
          ##initial values
          yinit[leftcensored] <- censorLimitMat[leftcensored,1]-log(2) #init value is LOQ/2
          yinit[rightcensored] <- censorLimitMat[rightcensored,2]-log(2) #init value is LOQ/2
          yinit[intcensored] <- (censorLimitMat[intcensored,1]+censorLimitMat[intcensored,2])/2 #mean of the range


          #initial values

          inits <- list(mu=log(0.3),sigma=log(2.5),y=yinit, .RNG.name="base::Wichmann-Hill", .RNG.seed=unique.seed)

          }



          ###################  calling jags

          #parameter definition
          j.mod <- jags.model(file=textConnection(model.2),
                              data = list(y = y,
                                          n.obs = n.obs,
                                          CensorType=CensorType,
                                          censorLimitMat=censorLimitMat),
                              inits =inits,
                              n.chains = 1,
                              n.adapt = 100,quiet=TRUE)

          #burnin
          update(j.mod,n.iter=500,progress.bar="none")




          ###### loop for each 1000 iterations

          mu.chain <- numeric(0)
          sigma.chain <-  numeric(0)

          for (i in 1:(n.iter/1000)) {

            #updates
            j.out <-coda.samples(model=j.mod,
                                 variable.names=c('mu','sigma'),
                                 n.iter=1000,
                                 thin=1,progress.bar="none")



            #########Numerical resulst of interest

            mu.chain <- c(mu.chain,j.out[[1]][,1])
            sigma.chain <-c(sigma.chain,j.out[[1]][,2])


            ###progressbar
            if (is.function(updateProgress)) {
              text <- paste0(i*1000,"/",n.iter)
              updateProgress(detail = text)
            }


          }

          results <-list(mu.chain=mu.chain+log(c.oel),sigma.chain=sigma.chain)



          return(results)

          options(warn = oldw)

          }


#
#
#  STAN function
#
##



fun.bayes.stan <-function(observations , notcensored, leftcensored , rightcensored , intcensored , seed , c.oel , n.iter , updateProgress= NULL , mypath ) {


              ##STAN is capricious with the model object, thay may have to be recreated on each new computer


               #n.iter has to be a multiple of 5000

              x <- observations

              unique.seed <-seed

              stanmodel.seg <-readRDS(paste(mypath,"/stanmodel.seg.RDS",sep=""))


              oldw <- getOption("warn")
              options(warn = -1)


            ##data

            #observed values

            observed_values <-log(as.numeric(x[notcensored])/c.oel)

            N_observed <-length(observed_values)


            #left censored values

            leftcensored_values <-log(as.numeric(substring(x[leftcensored],2))/c.oel)

            N_leftcensored <-length(leftcensored_values)

            #right censored values

            rightcensored_values <-log(as.numeric(substring(x[rightcensored],2))/c.oel)

            N_rightcensored <-length(rightcensored_values)

            #interval censored values

            intcensored_left_values <-log(as.numeric(substring(x[intcensored],2,regexpr("-",x[intcensored],fixed=TRUE)-1))/c.oel)

            intcensored_right_values <-log(as.numeric(substring(x[intcensored],regexpr("-",x[intcensored],fixed=TRUE)+1,nchar(x[intcensored])-1))/c.oel)

            N_intcensored <-length(intcensored_left_values)

            ###data list

            dataList = list( "observed_values" = array(observed_values,dim=length(observed_values)) ,
                             "N_observed" = N_observed,
                             "leftcensored_values" = array(leftcensored_values,dim=length(leftcensored_values)),
                             "N_leftcensored" = N_leftcensored,
                             "rightcensored_values" = array(rightcensored_values,dim=length(rightcensored_values)),
                             "N_rightcensored" = N_rightcensored,
                             "intcensored_left_values" = array(intcensored_left_values,dim=length(intcensored_left_values)),
                             "intcensored_right_values" = array(intcensored_right_values,dim=length(intcensored_right_values)),
                             "N_intcensored" = N_intcensored)


            ##samples

            mu.chain <- numeric(0)
            sigma.chain <-  numeric(0)

            for (i in 1:(n.iter/5000)) {



              #updates
              stanFit = sampling( object=stanmodel.seg , data=dataList ,
                                  chains=1 , iter=5500 , warmup=500 , thin=1,
                                  show_messages=FALSE ,seed = unique.seed+i, verbose=FALSE) ####uncertainty about the seed behavior for STAN

              samples <-extract(stanFit,c("mu","sigma"),permuted = FALSE, inc_warmup = FALSE)

              #########Numerical resulst of interest

              mu.chain <- c(mu.chain,samples[,1,1])
              sigma.chain <-c(sigma.chain,samples[,1,2])


              ###progressbar
              if (is.function(updateProgress)) {
                text <- paste0(i*5000,"/",n.iter)
                updateProgress(detail = text)
              }


            }




            return(list(mu.chain=mu.chain+log(c.oel),
                        sigma.chain=sigma.chain))

            options(warn = oldw)

            }

#
#
#
##################################### MCGILL FUNCTIONS
#
#
#


fun.bayes.webexpo <-function(observations , notcensored, leftcensored , rightcensored , intcensored , seed , c.oel , n.iter , updateProgress= NULL , mypath) {


        setwd(mypath)

        source("data-summary.R")

        source("fcts.R")

        source("model-SEG-informedvar.R")

        x <- observations

        unique.seed <-seed


        oldw <- getOption("warn")
        options(warn = -1)


        ##data

        #observed values

        observed_values <-as.numeric(x[notcensored])/c.oel

        #left censored values

        leftcensored_values <-as.numeric(substring(x[leftcensored],2))/c.oel

        #right censored values

        rightcensored_values <-as.numeric(substring(x[rightcensored],2))/c.oel

        #interval censored values

        intcensored_left_values <-as.numeric(substring(x[intcensored],2,regexpr("-",x[intcensored],fixed=TRUE)-1))/c.oel

        intcensored_right_values <-as.numeric(substring(x[intcensored],regexpr("-",x[intcensored],fixed=TRUE)+1,nchar(x[intcensored])-1))/c.oel


        ## bayesian calculations



        set.generator("MersenneTwister", initialization = "init2002", resolution = 32, seed = substring(unique.seed,5))

        res <-SEG.informedvar(y=observed_values,
                              lt=leftcensored_values,
                              gt=rightcensored_values,
                              interval.lower=intcensored_left_values,
                              interval.upper=intcensored_right_values,
                              n.chains=1, n.iter=n.iter ,
                              n.burnin=500, n.thin=1, monitor.burnin=F,
                              mu.lower=-20, mu.upper=20,
                              log.sigma.mu= -0.1744, log.sigma.prec=2.5523,
                              init.mu=log(0.3),
                              init.sigma=log(2.5),
                              outcome.is.logNormally.distributed=TRUE,
                              past.data=NULL,
                              me.sd.range=numeric(0),
                              cv.range=numeric(0))

        results <-list(mu.chain=res$sample$mu+log(c.oel),sigma.chain=res$sample$sd)



        return(results)

        options(warn = oldw)

}
