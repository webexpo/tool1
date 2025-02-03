######################################################
#
#
#
# Numerical output functions - BETWEEN WORKER
#
#
# V1.00   20 march 2018
# V1.01   3 may 2018   adjusting num results
#
#
#
######################################################


##
#
#  INPUT : output from one of the bayesian output functions for the ANOVA analysis + user.parameter set
#
##

## requires the common numerical result script sourced

# conf is the level of confidence for credible intervals
# psi is the overexposure risk threshold
# frac_threshold is the acceptable exceedance value
# target_perc is the percentile of interest
# rappap_cover is the coverage of the rapport ratio
# wwct is the threshold for the within worker correlation coefficient



#### specific to the between worker analysis tool

#within worker correlation
rho <- function( sw.chain , sb.chain , conf) {

  chain <-sb.chain^2/(sb.chain^2+sw.chain^2)

  est <-median(chain)

  lcl <-quantile(chain,(100-conf)/200)

  ucl <-quantile(chain,1-(100-conf)/200)

  return(list(est=est,lcl=lcl,ucl=ucl))

}

#proba that correlation >threshold
rho.greaterthan <- function( sw.chain , sb.chain , conf , wwct) {  ## arithmetic mean

  chain <-sb.chain^2/(sb.chain^2+sw.chain^2)

  risk <-100*length (chain[ chain>wwct ])/length(chain)

  return(risk)

}

##rappaport ratio
Rappap.ratio <- function( sb.chain , conf , rappap_cover ) {  ## arithmetic mean

  chain <-exp(2*qnorm(1 - (100 - rappap_cover)/200)*sb.chain)

  est <-median(chain)

  lcl <-quantile(chain,(100-conf)/200)

  ucl <-quantile(chain,1-(100-conf)/200)

  return(list(est=est,lcl=lcl,ucl=ucl))
}


##rappaport ratio
Rappap.ratio.greaterthanX <- function( sb.chain , rappap_cover , hetero.criterion ) {  ## arithmetic mean

  chain <-exp(2*qnorm(1 - (100 - rappap_cover)/200)*sb.chain)

  risk <-100*length (chain[ chain>hetero.criterion ])/length(chain)

  return(risk)

}

#probability that a random worker has his own exceedance greater than the target_threshold
ind.frac.risk <- function(mu.chain, sw.chain , sb.chain , frac_threshold , c.oel , conf) {

  qn <- qnorm(1-(frac_threshold/100))

  chain <- 100*(1-pnorm((log((c.oel))-mu.chain-qn*sw.chain)/sb.chain))

  est <-median(chain)

  lcl <-quantile(chain,(100-conf)/200)

  ucl <-quantile(chain,1-(100-conf)/200)

  return(list(est=est,lcl=lcl,ucl=ucl))

}

ind.frac.risk.proboverX <- function(mu.chain, sw.chain , sb.chain , frac_threshold , c.oel , X) {

  qn <- qnorm(1-(frac_threshold/100))

  chain <- 100*(1-pnorm((log((c.oel))-mu.chain-qn*sw.chain)/sb.chain))

  risk <-100*length (chain[ chain>X ])/length(chain)

  return(risk)

}


#probability that a random worker has his own critical percentile greater than the c.oel
ind.perc.risk <- function(mu.chain, sw.chain , sb.chain , target_perc , c.oel , conf ) {

  qn <- qnorm(target_perc/100)

  chain <- 100*(1-pnorm((log((c.oel))-mu.chain-qn*sw.chain)/sb.chain))

  est <-median(chain)

  lcl <-quantile(chain,(100-conf)/200)

  ucl <-quantile(chain,1-(100-conf)/200)

  return(list(est=est,lcl=lcl,ucl=ucl))

}

ind.perc.risk.proboverX <- function(mu.chain, sw.chain , sb.chain ,  target_perc , c.oel , X) {

  qn <- qnorm(target_perc/100)

  chain <- 100*(1-pnorm((log((c.oel))-mu.chain-qn*sw.chain)/sb.chain))

  risk <-100*length (chain[ chain>X ])/length(chain)

  return(risk)

}





#probability that a random worker has his own AM greater than the c.oel
ind.am.risk <- function(mu.chain, sw.chain , sb.chain , c.oel , conf) {

  chain <- 100*(1-pnorm((log((c.oel))-mu.chain-0.5*sw.chain^2)/sb.chain))

  est <-median(chain)

  lcl <-quantile(chain,(100-conf)/200)

  ucl <-quantile(chain,1-(100-conf)/200)

  return(list(est=est,lcl=lcl,ucl=ucl))

}

ind.am.risk.proboverX <- function(mu.chain, sw.chain , sb.chain , c.oel , X) {

  chain <- 100*(1-pnorm((log((c.oel))-mu.chain-0.5*sw.chain^2)/sb.chain))

  risk <-100*length (chain[ chain>X ])/length(chain)

  return(risk)

}


####### creating a list containing everything for the GROUP or a single worker


####### creating a list containing everything specific to the ANOVA analysis


all.numeric.B <-function(bayesian.output.B,user.input.B, c.oel , frac.ind.threshold , perc.ind.threshold , am.ind.threshold) {

  return(list(

    gsdb = gsd(sigma.chain = bayesian.output.B$sb ,
               conf = user.input.B$conf),

    gsdw = gsd(sigma.chain = bayesian.output.B$sw ,
               conf = user.input.B$conf),

    rho=rho( sw.chain=bayesian.output.B$sw ,
             sb.chain=bayesian.output.B$sb ,
             conf=user.input.B$conf),
    rho.greaterthan=rho.greaterthan( sw.chain=bayesian.output.B$sw ,
                                     sb.chain=bayesian.output.B$sb ,
                                     conf=user.input.B$conf ,
                                     wwct=user.input.B$wwct),

    Rappap.ratio=Rappap.ratio( sb.chain=bayesian.output.B$sb  ,
                               conf=user.input.B$conf ,
                               rappap_cover=user.input.B$rappap_cover ),

    Rappap.ratio.greaterthan2=Rappap.ratio.greaterthanX( sb.chain=bayesian.output.B$sb  ,
                                                         rappap_cover=user.input.B$rappap_cover ,
                                                         hetero.criterion=2),

    Rappap.ratio.greaterthan10=Rappap.ratio.greaterthanX( sb.chain=bayesian.output.B$sb  ,
                                                          rappap_cover=user.input.B$rappap_cover ,
                                                          hetero.criterion=10),

    ind.frac.risk=ind.frac.risk(mu.chain=bayesian.output.B$mu,
                                sw.chain=bayesian.output.B$sw ,
                                sb.chain=bayesian.output.B$sb ,
                                frac_threshold=user.input.B$frac_threshold ,
                                c.oel=c.oel ,
                                conf=user.input.B$conf),

    ind.frac.risk.proboverX=ind.frac.risk.proboverX(mu.chain=bayesian.output.B$mu,
                                                     sw.chain=bayesian.output.B$sw ,
                                                     sb.chain=bayesian.output.B$sb ,
                                                     frac_threshold=user.input.B$frac_threshold ,
                                                     c.oel=c.oel ,
                                                     X=frac.ind.threshold),

    ind.perc.risk=ind.perc.risk(mu.chain=bayesian.output.B$mu,
                                sw.chain=bayesian.output.B$sw ,
                                sb.chain=bayesian.output.B$sb ,
                                target_perc=user.input.B$target_perc ,
                                c.oel=c.oel , conf=user.input.B$conf),

    ind.perc.risk.proboverX=ind.perc.risk.proboverX(mu.chain=bayesian.output.B$mu,
                                                     sw.chain=bayesian.output.B$sw ,
                                                     sb.chain=bayesian.output.B$sb ,
                                                     target_perc=user.input.B$target_perc  ,
                                                     c.oel=c.oel , X=perc.ind.threshold),

    ind.am.risk=ind.am.risk(mu.chain=bayesian.output.B$mu,
                            sw.chain=bayesian.output.B$sw ,
                            sb.chain=bayesian.output.B$sb ,
                            c.oel=c.oel ,
                            conf=user.input.B$conf),

    ind.am.risk.proboverX=ind.am.risk.proboverX(mu.chain=bayesian.output.B$mu,
                                                 sw.chain=bayesian.output.B$sw ,
                                                 sb.chain=bayesian.output.B$sb ,
                                                 c.oel=c.oel ,
                                                 X=am.ind.threshold),

    c.oel=c.oel

  ))


}



