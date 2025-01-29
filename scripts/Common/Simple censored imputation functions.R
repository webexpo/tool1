######################################################
#
#
#
# Simple imputation for descriptive statistics and some graphs
#
#
# V1.00   20 march 2018
# V1.01   4 june 2018 correction of issue with regexpr
#
######################################################

##
#
#  INPUT : Vector of formatted observation, vectors of censoring.information
#
##


###
#
# the principle is to simply input right censored and interval censored data (which should be rare), and then to apply NDEXPO, as there does not seem to be
# a practical equivalent to NDexpo for these types of data
#
#
###

#
# RIGTH censored sample will be imputed as censoring.point*9/4
#

#
# intervall censored sample will be imputed as mid-range
#

simple.censored.treatment <-function(observations.formatted,notcensored,leftcensored,rightcensored,intcensored) {

 # data.formatted <-data.sample.formatted

imputed.data <-observations.formatted


  #treating right censored data

  restrict <-rightcensored

  imputed.data[restrict] <-as.numeric(substring(imputed.data[restrict],2))*9/4

  #treating interval censored data

  restrict <-intcensored

  imputed.data[restrict] <-(as.numeric(substring(imputed.data[restrict],2,regexpr("-",imputed.data[restrict],fixed=TRUE)-1))+

                               as.numeric(substring(imputed.data[restrict],regexpr("-",imputed.data[restrict],fixed=TRUE)+1,nchar(imputed.data[restrict])-1)))/2


  #treating left censored data (NDexpo)

  final.imputed.data <-fun.NdExpo(imputed.data)


  #output

  result <-list(imputed=final.imputed.data,
                leftcensored=leftcensored,
                rightcensored=rightcensored,
                intcensored=intcensored,
                notcensored=notcensored)

  }


#
#
#
# Support functions
#
#
#


fun.NdExpo <-function(x){

  ##input : expostats style vector of exposure values (include values of type x and <x)
  ##output	:  dataframe with
          # X values imputed with ROS
          # ND indicator : 0=ND
          #plotting positions ordered as the initial observation vector





  ####
  #
  #       WARNING : IF <3 detected : LOQ/2
  #
  ##


 #is.censored is 0 for censored, 1 for observed

  is.censored <- as.integer(!grepl( '<',x))     #0 means censored

  #eliminating the  '<' sign

  x[regexpr('<',x)==1] <-as.numeric(substring(x[regexpr('<',x)==1],2))

  x <-as.numeric(x)

  ###ordering X and keeping the order

  x.order <-rank(x, ties.method='first')

  x.ordered <-numeric(length(x))
  x.ordered[x.order] <-x

  is.censored[x.order] <- is.censored

  ######################IF NO ND

  if (length(x[is.censored==0])==0) {

    y <- log(as.numeric(x.ordered))

    pp <-((1:length(x))-0.375)/(length(x)+0.25)



    res <-list(data=data.frame(xfin=x.ordered[x.order],yfin=y[x.order],is.censored=is.censored[x.order],pp=pp[x.order],order.index=x.order,x=x.ordered[x.order]),
               alpha=0,beta=0)



  }

  else if(length(x[is.censored!=0])<3) {

    x.ordered[is.censored==0]<-x.ordered[is.censored==0]/2


    y <- log(as.numeric(x.ordered))

    pp <-((1:length(x))-0.375)/(length(x)+0.25)



    res <-list(data=data.frame(xfin=x.ordered[x.order],yfin=y[x.order],is.censored=is.censored[x.order],pp=pp[x.order],order.index=x.order,x=x.ordered[x.order]),
               alpha=0,beta=0)


  }

  else {

    ####correcting adjacency The NDExpo approach

    ##first ordering so that no case of <5 5 : to become <5 5

    for (i in (length(x)):2) {

      if (is.censored[i]==0 & is.censored[i-1]==1 & x.ordered[i]==x.ordered[i-1]) {

        is.censored[i] <-1
        is.censored[i-1] <-0

        temp <-x.order[i-1]
        x.order[i-1] <- x.order[i]
        x.order[i] <- temp

      }

    }


    #####correcting adjacency

    for (i in 1:(length(x)-1) ) {

      if (is.censored[i]==0 & is.censored[i+1]==0 & x.ordered[i]!=x.ordered[i+1]) {

        x.ordered[i+1] <-  x.ordered[i]

      }
    }




    #log transformation

    y <- log(as.numeric(x.ordered))


    #nb detection limits

    NL <-length(unique(y[is.censored==0]))



    #######the case where NL is 1

    if (NL==1) {

      NbA <- length(y[is.censored==1 & y>=min(y[is.censored==0])])
      NbB <- length(y[is.censored==1 & y<min(y[is.censored==0])])
      NbC <-length(y[is.censored==0])

      index.A <-1:NbA
      index.B <-1:NbB
      index.C <-1:NbC

      Pdep <-NbA/(NbB+NbC+NbA)

      if (NbA!=0) PdA <-(1-Pdep)+Pdep*index.A/(NbA+1) else PdA <-numeric(0)

      if (NbB!=0)PdB <-(1-Pdep)*index.B/(NbB+1) else PdB <-numeric(0)

      if (NbC!=0)PdC <-(1-Pdep)*index.C/(NbC+1) else PdC <-numeric(0)

      pp <-c(PdB,PdC,PdA)

    }


    else {

      #vector of limits

      L <-unique(y[is.censored==0][order(y[is.censored==0])])

      L <-c(-100000,L,10000)

      #vector of detected values between each LD    here

      NbA <-numeric(NL+1)

      for (i in 1:(NL+1)) NbA[i] <-length(y[is.censored==1 & y>=L[i] & y<L[i+1]])

      #vector of all values smaller than Li

      NbB.ND <-numeric(NL+1)    #inequality  is lenient

      NbB.D  <-numeric(NL+1)    #inequality  is strict


      for (i in 1:(NL+1)) NbB.ND[i] <-length(y[is.censored==0 & y<=L[i]])
      for (i in 1:(NL+1)) NbB.D[i] <-length(y[is.censored==1 & y<L[i]])

      NbB <-NbB.ND+NbB.D

      #vector of ND values at Lj

      NbC <-numeric(NL)

      for (i in 1:(NL)) NbC[i] <-NbB[i+1]-NbB[i]-NbA[i]

      NbC <-c(NbC,0)

      ##probability of exceedance for limit i

      Pdep <-  numeric(NL+2)
      Pdep[1] <-1
      Pdep[NL+2] <-0

      for (i in (NL+1):2) Pdep[i] <-Pdep[i+1] + (1-Pdep[i+1])*NbA[i]/(NbA[i]+NbB[i])


      ######vector of plotting positions

      pp <-numeric(length(y))


      ###forming the detected groups

      start <-1

      for (j in 1:(NL+1)) {



        if (NbA[j]!=0) {

          Pdet <-numeric(NbA[j])

          for (i in 1:NbA[j]) Pdet[i] <-(1-Pdep[j])+i*(Pdep[j]-Pdep[j+1])/(NbA[j]+1)

          index <-start:(start+NbA[j]-1)

          pp[index] <-Pdet

        }

        start <-start+NbA[j]+NbC[j]


      }


      ### forming the not detected groups

      start <-1+NbA[1]

      for (j in 1:(NL)) {


        if (NbC[j]!=0) {

          Pdet <-numeric(NbC[j])

          for (i in 1:NbC[j]) Pdet[i] <-i*(1-Pdep[j+1])/(NbC[j]+1)

          index <-start:(start+NbC[j]-1)

          pp[index] <-Pdet

        }
        start <-start+NbA[j+1]+NbC[j]


      }

    }
    #########predicting the NDs

    normal.scores <-qnorm(pp)

    regression <-lm(y[is.censored==1]~normal.scores[is.censored==1])

    y[is.censored==0] <- regression$coefficients[1]+regression$coefficients[2]*normal.scores[is.censored==0]

    #########final result preparation


    res <-list(data=data.frame(xfin=exp(y[x.order]),yfin=y[x.order],is.censored=is.censored[x.order],pp=pp[x.order],order.index=x.order,x=x.ordered[x.order]),
               alpha=regression$coefficients[1],beta=regression$coefficients[2])

  }

  return(res)
}
