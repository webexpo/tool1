



#########################FONCTION GENERATING A CENSORED RANDOM SAMPLE FOR THE SEG ANALYSIS TOOL

# V1.00   20 march 2018

fun.gener.censored <-function(n = 10,
                              no.censoring = FALSE,
                              perc.lowerthan = 20,
                              perc.greaterthan = 10,
                              perc.between = 20,
                              gm = 0.3,
                              gsd = 2.5,
                              left_factor = 1.5,
                              right_factor = 1/1.5,
                              int_factor = 1.5) {

  #######generating the raw sample

  raw.sample <- exp(rnorm(n,log(gm),log(gsd)))


  ###### Censorship values
  #
  # left  : x becomes <x*left_factor=1.5
  # right : x becomes x>x*right_factor=1/1.5
  # interval : x becomes <x*int_factor and x>x/int_factor

  ####number of censored = perc*n rounded up

  #####left censored chosen on whole data ordered
  #####right censored chosen on whole data ordered
  #####intervall censored chosen on whole data randomly


  samplev1 <-as.character(signif(raw.sample,3))

  if (!no.censoring) {


    #####ordering data

    raw.sample <-raw.sample[order(raw.sample,decreasing = FALSE)]


    ######insuring a minimum of 3 uncensored


    if (floor((100-perc.lowerthan-perc.greaterthan-perc.between)*n/100)<3) {


      perc.greaterthan <-0
      perc.between <-0
      perc.lowerthan <- 100*(n-3)/n
    }



    #####left censoring

    n.Lcensored <-round(perc.lowerthan*sum(n)/100)

    if(n.Lcensored!=0) {

      samplev1[1:n.Lcensored] <- paste("<",signif(raw.sample[1:n.Lcensored]*left_factor,3),sep="")

    }

    #####right censoring

    n.Rcensored <-round(perc.greaterthan*sum(n)/100)

    if(n.Rcensored!=0) {



      samplev1[(sum(n)-n.Rcensored+1):sum(n)] <- paste(">",signif(raw.sample[(sum(n)-n.Rcensored+1):sum(n)]*right_factor,3),sep="")

    }

    #####intervall censoring

    n.Icensored <-round(perc.between*sum(n)/100)

    if(n.Icensored!=0) {


      index <-sample((n.Lcensored+1):(sum(n)-n.Rcensored), size=n.Icensored,replace = FALSE)

      samplev1[index] <- paste("[",signif(raw.sample[index]/int_factor,3),
                               "-",signif(raw.sample[index]*int_factor,3),"]",sep="")

    }

    ########reordering by worker + random

    new.index <-sample(1:n,size=n,replace=FALSE)

    samplev1 <-samplev1[new.index]

  }

  #######results

  results <-list()

  ###as a vector

  results$data.frame <-data.frame(x=samplev1,stringsAsFactors = FALSE)

  ##formatted as for expostats data entry

  res.string <-samplev1

  results$string <-paste(res.string,collapse="\n")

  return(results)


}

