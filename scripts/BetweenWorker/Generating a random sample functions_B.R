#################################
#
#
#
# Generating random sample of censored lognormal data _ Between worker
#
#
# V1.00   20 march 2018
#
###############################

#####EXEMPLE

#test <-fun.gener.censored.betweenworker(n.worker = 20,
#                                            n.days = rep(20,20), #as many elements as workers
#                                            no.censoring = TRUE,
#                                            perc.lowerthan = 20,
#                                            perc.greaterthan = 10,
#                                            perc.between = 20,
#                                            gm = 0.3,
#                                            gsd = 2.5,
#                                            rho = 0.5,
#                                            error  = "cv(0.1,0.1)",
#                                            left_factor = 1.5,
#                                            right_factor = 1/1.5,
#                                            int_factor = 1.5)

#test$dataframe   ######data as a dataframe

#test$string ###########data as a string (expostats format)


#########################FONCTION

fun.gener.censored.betweenworker <-function(n.worker = 20,
                                            n.days = rep(20,20), #as many elements as workers
                                            no.censoring = TRUE,
                                            perc.lowerthan = 20,
                                            perc.greaterthan = 10,
                                            perc.between = 20,
                                            gm = 0.3,
                                            gsd = 2.5,
                                            rho = 0.5,
                                            error  = "",
                                            left_factor = 1.5,
                                            right_factor = 1/1.5,
                                            int_factor = 1.5) {

  #######generating the raw sample

  raw.sample <- gener.sample.V2(mu.y=log(gm),gsd=log(gsd),rho=rho,n.t=n.worker,n.w=n.days)


  ###### Censorship values
  #
  # left  : x becomes <x*left_factor=1.5
  # right : x becomes x>x*right_factor=1/1.5
  # interval : x becomes <x*int_factor and x>x/int_factor

  ####number of censored = perc*n rounded up

  #####left censored chosen on whole data ordered
  #####right censored chosen on whole data ordered
  #####intervall censored chosen on whole data randomly

  raw.sample$final.x <-as.character(signif(raw.sample$x,3))

  if (!no.censoring) {


    #####ordering data

    raw.sample <-raw.sample[order(raw.sample$x,decreasing = FALSE),]

    #####left censoring

    n.Lcensored <-ceiling(perc.lowerthan*sum(n.days)/100)

    if(n.Lcensored!=0) {

      raw.sample$final.x[1:n.Lcensored] <- paste("<",signif(raw.sample$x[1:n.Lcensored]*left_factor,3),sep="")

    }

    #####right censoring

    n.Rcensored <-ceiling(perc.greaterthan*sum(n.days)/100)

    if(n.Rcensored!=0) {



      raw.sample$final.x[(sum(n.days)-n.Rcensored+1):sum(n.days)] <- paste(">",signif(raw.sample$x[(sum(n.days)-n.Rcensored+1):sum(n.days)]*right_factor,3),sep="")

    }

    #####intervall censoring

    n.Icensored <-ceiling(perc.between*sum(n.days)/100)

    if(n.Icensored!=0) {


      index <-sample((n.Lcensored+1):(sum(n.days)-n.Rcensored), size=n.Icensored,replace = FALSE)

      raw.sample$final.x[index] <- paste("[",signif(raw.sample$x[index]/int_factor,3),
                                         "-",signif(raw.sample$x[index]*int_factor,3),"]",sep="")

    }

    ########reordering by worker + random

    raw.sample <-raw.sample[order(raw.sample$worker),]

    new.index <-sample(1:n.days[1],size=n.days[1],replace=FALSE)

    for (i in 2:n.worker) new.index <-c(new.index,
                                        sample(sum(n.days[1:(i-1)]):sum(n.days[1:(i)]),size=n.days[i],replace=FALSE))

    raw.sample <-raw.sample[new.index,]

  }

  #######results

  results <-list()

  ###as a data frame

  results$dataframe <-raw.sample

  ##formatted as for F Lemay

  raw.sample$error <-error

  raw.sample$string <-paste(raw.sample$final.x, raw.sample$worker, sep="\t")

  results$string <-paste(raw.sample$string,collapse="\n")

  if (error!="") results$string <-paste(error,results$string,sep="|")


  return(results)


}



#############
#
#
#         SUPPORT FUNCTIONS
#
##
#######################


##### function for repetition index to identify workers

index.fun.V2 <-function(n.t,n.w=rep(3,n.t)) {

  #n.t worker number
  #n.w number of measurement per worker

  res <-integer(0)

  for (i in 1:n.t) {

    res <-c(res,rep(i,n.w[i]))

  }
  return(res)

}


#################generating a sample for a heterogeneous group


gener.sample.V2 <-function(mu.y,gsd,rho,n.t,n.w=rep(3,n.t),worst=1)   ###unbalanced datasets, n.w is a vbector of length n.t

{

  #mu.y : mean of logged data
  #gsd : geometric SD
  #rho within-worker correlation
  #n.t number of workers,
  #n.w nombre of wordays
  #worst case: 1-no worst case (random worker in the group),
  #2-50% worst-case , worker in the most exposed half of the group
  #3 75%worst-case , worker in the most exposed 25% of the group




  worker.effect <-numeric(n.t)

  #identifier for workers

  worker.name <-paste(rep('worker',n.t),as.character(1:n.t),sep='-')

  #estimating variance components from rho

  sigB <-sqrt(rho*(log(gsd))^2)
  sigW <-sqrt((1-rho)*(log(gsd))^2)

  #worker effect
  a<-numeric(1)

  ###if sigB!=0

  if (sigB!=0) {

    #sampling a random worker, with rejection mechanism if worst case
    for (i in 1:n.t) {	if (worst==1) {a<-rnorm(1,0,sigB)}
      else if (worst==2) {		a<-rnorm(1,0,sigB)
      while (a<qnorm(0.5,0,sigB)) {a<-rnorm(1,0,sigB)}
      }
      else if (worst==3) {		a<-rnorm(1,0,sigB)
      while (a<qnorm(0.75,0,sigB)) {a<-rnorm(1,0,sigB)}
      }
      worker.effect[i] <-a
    }
  }
  ###if sigB==0
  else {worker.effect <- c(rep(0,n.t)) }

  #each worker effect is repeated n.w times
  worker.effect <-worker.effect[index.fun.V2(n.t,n.w)]

  #adding the "within sample" random variation
  sim.sample <-rnorm(sum(n.w),mu.y,sigW)+worker.effect

  #final data.frame

  result <-data.frame(x=exp(sim.sample),worker=worker.name[index.fun.V2(n.t,n.w)],stringsAsFactors=F)
  return(result)
}
