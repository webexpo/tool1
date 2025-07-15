######################################################
#
#
#
# Numerical output functions
#
#
# V1.00   20 march 2018
#
#
#
#
######################################################


##
#
#  INPUT : output from one of the bayesian output functions + user.parameter set
#
##


# MCMC chain for MU
# MCMC chain for SIGMA
# corrected OEL
# conf is the level of confidence for credible intervals
# psi is the overexposure risk threshold
# frac_threshold is the acceptable exceedance value
# target_perc is the percentile of interest


#### parameter calculations - distribution

#geometric mean
gm <- function(mu.chain,conf) {

  chain <-exp(mu.chain)

  est <-median(chain)

  lcl <-quantile(chain,(100-conf)/200)

  ucl <-quantile(chain,1-(100-conf)/200)

  return(list(est=est,lcl=lcl,ucl=ucl))

}

#geometric standard deviation
gsd <- function(sigma.chain,conf) {

  chain <-exp(sigma.chain)

  est <-median(chain)

  lcl <-quantile(chain,(100-conf)/200)

  ucl <-quantile(chain,1-(100-conf)/200)

  return(list(est=est,lcl=lcl,ucl=ucl))

}


frac <- function(mu.chain, sigma.chain, conf, c.oel) {  #exceedance fraction

  chain <-100*(1-pnorm((log((c.oel))-mu.chain)/sigma.chain))

  est <-median(chain)

  lcl <-quantile(chain,(100-conf)/200)

  ucl <-quantile(chain,1-(100-conf)/200)

  return(list(est=est,lcl=lcl,ucl=ucl))

}

perc <- function(mu.chain, sigma.chain, target_perc, conf) { #percentile of interest

  chain <-exp(mu.chain + qnorm(target_perc/100)*sigma.chain)

  est <-median(chain)

  lcl <-quantile(chain,(100-conf)/200)

  ucl <-quantile(chain,1-(100-conf)/200)

  return(list(est=est,lcl=lcl,ucl=ucl))

}


am <- function(mu.chain, sigma.chain, conf) {  ## arithmetic mean

  chain <-exp(mu.chain + 0.5*sigma.chain^2)

  est <-median(chain)

  lcl <-quantile(chain,(100-conf)/200)

  ucl <-quantile(chain,1-(100-conf)/200)

  return(list(est=est,lcl=lcl,ucl=ucl))

}


#### parameter calculations - risk


frac.risk <- function(mu.chain, sigma.chain, frac_threshold, c.oel) {  #exceedance fraction

  chain <-100*(1-pnorm((log((c.oel))-mu.chain)/sigma.chain))

  risk <-100*length (chain[ chain>frac_threshold ])/length(chain)

  return(risk)

}

perc.risk <- function(mu.chain, sigma.chain, target_perc, c.oel) { #percentile of interest

  chain <-exp(mu.chain + qnorm(target_perc/100)*sigma.chain)

  risk <-100*length (chain[ chain>c.oel ])/length(chain)

  return(risk)

}


am.risk <- function(mu.chain, sigma.chain, c.oel) {  ## arithmetic mean

  chain <-exp(mu.chain + 0.5*sigma.chain^2)

  risk <-100*length (chain[ chain>c.oel ])/length(chain)

  return(risk)

}



####### creating a list containing everything

all.numeric <- function(
    mu.chain,
    sigma.chain,
    conf,
    c.oel,
    frac_threshold,
    target_perc)
{
    return(
        list(
            gm         = gm(mu.chain,conf),
            gsd        = gsd(sigma.chain,conf),
            frac       = frac(mu.chain, sigma.chain, conf, c.oel),
            frac.ucl95 = frac(mu.chain, sigma.chain, 90, c.oel)$ucl,
            frac.ucl70 = frac(mu.chain, sigma.chain, 40, c.oel)$ucl,
            perc       = perc(mu.chain, sigma.chain, target_perc, conf),
            perc.ucl95 = perc(mu.chain, sigma.chain, target_perc, 90)$ucl,
            perc.ucl70 = perc(mu.chain, sigma.chain, target_perc, 40)$ucl,
            am         = am(mu.chain, sigma.chain, conf),
            am.ucl95   = am(mu.chain, sigma.chain, 90)$ucl,
            am.ucl70   = am(mu.chain, sigma.chain, 40)$ucl,
            frac.risk  = frac.risk(mu.chain, sigma.chain, frac_threshold, c.oel),
            perc.risk  = perc.risk(mu.chain, sigma.chain, target_perc, c.oel),
            am.risk    = am.risk(mu.chain, sigma.chain, c.oel),
            c.oel      = c.oel
        )
    )
}

