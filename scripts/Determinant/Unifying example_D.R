################################################################
#
#
#  Example of running the complete determinants analysis tool
#
################################################################

#
#
# V1.00   20 march 2018
# V1.01    8 mai 2018
#

######### sourcing the scripts

setwd("C:/jerome/Dropbox/SHARE Daniel Margulius/expostats2018/scripts")

library(readxl)


##### determinant specific functions

source("Determinant/Data formatting functions_D.R")

source("Determinant/Bayesian engine functions_D.R")

source("Determinant/Descriptive graphs functions_D.R")

source("Determinant/Numerical output functions_D.R")

source("Determinant/Main graph functions_D.R")

#### common functions

source("Common/Bayesian engine functions.R")

source("Common/Simple censored imputation functions.R")

source("Common/Descriptive numerical output functions.R")

source("Common/Descriptive graphs functions.R")

source("Common/script density comparison.R")

source("Common/Numerical output functions.R")

source("Common/Main graph functions.R")

#################### Creating a sample ###########################


data.sample <-read_excel("Determinant/data.example.xlsx",sheet=1)


ListOfVars <-names(data.sample[,-1])

VarOfInterest <-ListOfVars[1]

CategoryOfInterest <-as.character(unique(data.sample[,VarOfInterest]))[1]

user.input <-list( conf=90 , psi = 30 , frac_threshold = 5 , target_perc = 95 ,
                   exp.ratio.gm = 2 , expdelta= 10 )


############## GLOBAL ANALYSIS

### descriptive

    ## formatting the whole dataset
    data.sample.formatted <-data.formatting.D( data.in=data.sample,
                                               oel=0.6,oel.mult=1,
                                               VarOfInterest=VarOfInterest)

    ## imputation
    data.sample.imputed <-simple.censored.treatment(
         observations.formatted=data.sample.formatted$data ,
          notcensored=data.sample.formatted$notcensored ,
          leftcensored=data.sample.formatted$leftcensored ,
         rightcensored=data.sample.formatted$rightcensored ,
         intcensored=data.sample.formatted$intcensored)


    #graph

    X <-data.sample.formatted
    Y <- data.sample.imputed

    p <- fun.qqplot.group.D(data.simply.imputed = Y,
                            notcensored = X$notcensored ,
                            cats = X$var)

    #descriptive table

    X <-data.sample.formatted
    Y <- data.sample.imputed

    View(fun.desc.stat(data.simply.imputed = Y, c.oel = X$c.oel))



### bayesian analysis


    ##calculations

    X <-data.sample.formatted

    bayesian.analysis.global <- fun.bayes.jags(observations=X$data ,
                                               notcensored=X$notcensored,
                                               leftcensored=X$leftcensored ,
                                               rightcensored=X$rightcensored ,
                                               intcensored=X$intcensored ,
                                               seed=X$seed ,
                                               c.oel=X$c.oel , n.iter=25000 , updateProgress= NULL)

    ##numerical analysis

    X <-bayesian.analysis.global
    Y <-data.sample.formatted
    Z <- user.input

    num.res.global <-all.numeric(mu.chain=X$mu.chain ,
                          sigma.chain=X$sigma.chain ,
                          conf=Z$conf ,
                          c.oel=Y$c.oel ,
                          frac_threshold=Z$frac_threshold ,
                          target_perc=Z$target_perc)

    ## graphs : see TOOL1 unifying example

    #sequential plot

    X <-num.res.global
    Y <-data.sample.formatted
    Z <- user.input
    W <- data.sample.imputed

    sequential.plot.frac(gm=X$gm$est ,
                         gsd=X$gsd$est ,
                         frac=X$frac$est ,
                         c.oel=Y$c.oel ,
                         data.simply.imputed=W)

    sequential.plot.perc(gm=X$gm$est ,
                         gsd=X$gsd$est ,
                         perc=X$perc$est ,
                         target_perc=Z$target_perc,
                         c.oel=Y$c.oel ,
                         data.simply.imputed=W)

    sequential.plot.am(gm=X$gm$est ,
                       gsd=X$gsd$est ,
                       am=X$am$est ,
                       c.oel=Y$c.oel ,
                       data.simply.imputed=W)


    #risk band plot

    X <-bayesian.analysis.global
    Y <-data.sample.formatted
    Z <- user.input


    riskband.plot.frac(mu.chain=X$mu.chain,
                       sigma.chain=X$sigma.chain,
                       c.oel=Y$c.oel,
                       frac_threshold = Z$frac_threshold)

    riskband.plot.perc(mu.chain=X$mu.chain,
                       sigma.chain=X$sigma.chain,
                       c.oel=Y$c.oel,
                       target_perc=Z$target_perc)

    riskband.plot.am(mu.chain=X$mu.chain,
                     sigma.chain=X$sigma.chain,
                     c.oel=Y$c.oel)


############## ANALYSES ON THE MULTI CATEGORY MODEL


#####   bayesian analysis

    X <-data.sample.formatted

    bayesian.analysis.cat <-fun.bayes.jags.D( data.formatted = X ,
                             n.iter=30000 ,
                             updateProgress= NULL)




###### comparative analysis - all categories

    #### boxplot graph

    X <-data.sample.formatted

    Y <- bayesian.analysis.cat

    Z <- data.sample.imputed

    p <- boxplot.by.cat(data.formatted = X ,
                        data.simply.imputed = Z ,
                        bayesian.output.D = Y)



    #### comparative table

    X <-data.sample.formatted

    Y <- bayesian.analysis.cat

    Z <- user.input

    View(fun.comp.table.D(bayesian.analysis.D = Y,
                          c.oel = X$c.oel,
                          user.input = Z))


    ### comparative graphs

    X <- data.sample.formatted

    Y <- bayesian.analysis.cat

    Z <- user.input

    riskband.perc.byband(bayesian.ouput.D = Y, c.oel=X$c.oel, target_perc=Z$target_perc)

    risk.gauge(bayesian.ouput.D = Y , c.oel=X$c.oel, user.input=Z)

######### comparative analysis - 2 categories

category1 <- unlist(unique(data.sample[,VarOfInterest]))[1]
category2 <- unlist(unique(data.sample[,VarOfInterest]))[4]


    ###comparative graph

    X <-data.sample.formatted

    Y <- bayesian.analysis.cat

    Z <- user.input

    p <- boxplot.2.cat(data.formatted = X ,
                       bayesian.output.D = Y,
                       cat1 =  category1,
                       cat2 = category2)

   ### comparative metrics

    X <-data.sample.formatted

    Y <- bayesian.analysis.cat

    Z <- user.input

    View(fun.2cat.dist(bayesian.output.D = Y ,  cat1 =  category1,
                       cat2 = category2, user.input = user.input))

    View(fun.2cat.metrics(bayesian.output.D = Y ,  cat1 =  category1,
                       cat2 = category2, user.input = user.input))
    View(fun.2cat.f (dataformatted = X, bayesian.output.D = Y , cat1 =  category1,
                     cat2 = category2, user.input = user.input))

    View(fun.2cat.risk(data.formatted = X, bayesian.output.D = Y , cat1 =  category1,
                       cat2 = category2, user.input = user.input))




    ###### analysis for a single category

    ##data-restriction for a single category of interest :

    X <-data.formatted

    single.cat.formatted <-select.cat.formatted(data.formatted = X , cat = CategoryOfInterest)


    ############## simple censored imputation

    X <- single.cat.formatted

    single.cat.imputed <-simple.censored.treatment(observations.formatted=X$data,
                                                    notcensored=X$notcensored,
                                                    leftcensored=X$leftcensored,
                                                    rightcensored=X$rightcensored,
                                                    intcensored=X$intcensored)

    #############  descriptive statistics

    X <- single.cat.formatted

    Y <- single.cat.imputed

    View(fun.desc.stat(data.simply.imputed = Y , c.oel = X$c.oel ) )


    #############  descriptive graphs

    X <- single.cat.formatted

    Y <- single.cat.imputed

    fun.qqplot(data.simply.imputed = Y , notcensored = X$notcensored)


    ################## bayesian output - jags

    X <- bayesian.analysis.cat

    bayes.single <-bayesian.output.B.single(bayesian.output.D = X, cat = CategoryOfInterest)


    ################## numerical output - jags


    ##### EXEMPLE OF INPUT SET

    X <- bayes.single

    Y <- single.cat.formatted

    Z <- single.cat.imputed

    W <- user.input

    num.res <-all.numeric(mu.chain=X$mu.chain ,
                          sigma.chain=X$sigma.chain ,
                          conf=W$conf ,
                          c.oel=Y$c.oel ,
                          frac_threshold=W$frac_threshold ,
                          target_perc=W$target_perc)



    ############### graphical ouput

    #calendar plot

    calendar.plot(frac.est=num.res$frac$est)

    #sequential plot

    sequential.plot.frac(gm=num.res$gm$est ,
                         gsd=num.res$gsd$est ,
                         frac=num.res$frac$est ,
                         c.oel=Y$c.oel ,
                         data.simply.imputed=Z)

    sequential.plot.perc(gm=num.res$gm$est ,
                         gsd=num.res$gsd$est ,
                         perc=num.res$perc$est ,
                         target_perc=W$target_perc,
                         c.oel=Y$c.oel ,
                         data.simply.imputed=Z)

    sequential.plot.am(gm=num.res$gm$est ,
                       gsd=num.res$gsd$est ,
                       am=num.res$am$est ,
                       c.oel=Y$c.oel ,
                       data.simply.imputed=Z)




    #risk band plot

    riskband.plot.frac(mu.chain=X$mu.chain,
                       sigma.chain=X$sigma.chain,
                       c.oel=Y$c.oel,
                       frac_threshold = W$frac_threshold)

    riskband.plot.perc(mu.chain=X$mu.chain,
                       sigma.chain=X$sigma.chain,
                       c.oel=Y$c.oel,
                       target_perc=W$target_perc)

    riskband.plot.am(mu.chain=X$mu.chain,
                     sigma.chain=X$sigma.chain,
                     c.oel=Y$c.oel)


####### COMPARISON OF DENSITIES ACROSS BAYES ENGINES

ptm <- proc.time()
res1 <-fun.bayes.jags.D( data.formatted=data.sample.formatted ,
                         n.iter=30000 ,
                         updateProgress= NULL)
proc.time() - ptm


ptm <- proc.time()
res2 <-fun.bayes.stan.D( data.formatted=data.sample.formatted ,
                         n.iter=30000 ,
                         updateProgress= NULL,
                         mypath = "D:/Dropbox/bureau/RStudio/expostats/shinyapps/expostats functions 2018/Determinant specific functions")
proc.time() - ptm


ptm <- proc.time()
res3 <- fun.bayes.webexpo.D(data.formatted=data.sample.formatted ,
                            n.iter=30000 ,
                            updateProgress= NULL,
                            mypath = "D:/Dropbox/bureau/RStudio/expostats/shinyapps/expostats functions 2018")
proc.time() - ptm



################ comparison of densities

res1$group.id
res2$group.id
res3$group.id


fun.compare.dens(jags.chain=res1$mu[1,] ,stan.chain=res2$mu[1,],webexpo.chain=res3$mu[1,])
fun.compare.dens(jags.chain=res1$mu[3,] ,stan.chain=res2$mu[3,],webexpo.chain=res3$mu[3,])

fun.compare.dens(jags.chain=res1$sigma[1,] ,stan.chain=res2$sigma[1,],webexpo.chain=res3$sigma[1,])
fun.compare.dens(jags.chain=res1$sigma[3,] ,stan.chain=res2$sigma[3,],webexpo.chain=res3$sigma[3,])

fun.compare.dens(jags.chain=res1$mu[res1$group.id$index[res1$group.id$name==CategoryOfInterest],],
                  stan.chain=res2$mu[res2$group.id$index[res1$group.id$name==CategoryOfInterest],],
                  webexpo.chain=res3$mu[res3$group.id$index[res1$group.id$name==CategoryOfInterest],])

