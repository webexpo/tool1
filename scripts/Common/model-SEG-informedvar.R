# Version 0.15 (July 2017)

  ##slight modification by JLavoué for progess bars (look for **PROGRESS)

  # Change Log *****************************************************************
  #
  #   When updated, look for comments with new_* and modif_*
  #   to rapidly identify new/modified code.
  #
  # Version 0.14
  # ------------
  #   Added argument save.RData
  #     -> useful at development stage only, for monitoring parameters values when dens.gen.icdf is crashing
  #
  # Version 0.13
  # ------------
  #  Small correction -> see new_0.13
  #
  # Version 0.12
  # ------------
  #  Added the possibility to use past data
  #   > all code changes can be found by a search for "post.data", modif_0.12 & new_0.12
  #   > If past.data is used, then log.sigma.mu and log.sigma.prec are ignored
  #
  #       IMPORTANT: If it is suspected that past data were measured with error,
  #                  we do not recommend that they are used, especially if the suspected measurement error
  #                  would have been proportional to true values.
  #                  Indeed, the model assumes that the past data was measured without measurement error
  #                  (to include measurement error in the past data modelization,
  #                   we would need the ACTUAL values measured, not only a summary (through mean & sd)).
  #
  #   > If the outcome is log-normal, the past data values (mean, sd) must be those of the log-measurements.
  #
  #
  # Version 0.11
  # ------------
  #  - math.limits was changed to math.lower.limit
  #  - using out.logout.moments to compute 'moments'
  #  - also using argument logNormal.distrn in call to data.summary
  #
  #
  # Version 0.10 [replaces model.Kromhout]:
  # ------------
  #   - added arguments me.sd.range & cv.range
  #   - now using empty.matrices
  #   - now using sigma.truncatedData.gen.lnorm.object
  #   - now using sigma.gen.object
  #   - Slightly modified how the outcome list 'out' is prepared
  #     - objects mu.sample & sd.sample were regrouped in list 'sample'
  #         while burnin.mu & burnin.sd were regrouped in list 'burnin'
  #         > both are wrapped through function out.sample (defined in fcts.R) at the end of function


SEG.informedvar <- function(y=numeric(0), lt=numeric(0), gt=numeric(0),
  interval.lower=numeric(0), interval.upper=numeric(0),
  n.chains=1, n.iter=15000, n.burnin=500, n.thin=1, monitor.burnin=F,
  mu.lower=-20, mu.upper=20,
  log.sigma.mu=-0.1744, log.sigma.prec=2.5523,
  init.mu=rep(log(0.3), n.chains),
  init.sigma=rep(log(2.5), n.chains),
  outcome.is.logNormally.distributed=T,
  me.sd.range=numeric(0), cv.range=numeric(0),
  past.data=list(mean=numeric(0), sd=numeric(0), n=numeric(0)),
  save.RData=F, RData.dir='c:/users/jerome',
  updateProgress = NULL)                           ######## for progress bars in shiny or elsewhere **PROGRESS
{
  # Notes:
  # - me.sd.range is the range of Measurement Error SD (optional) [new_0.10]
  # - cv.range    is the range of Measurement Error Coefficient of Variation (optional)
  #   IMPORTANT: only one of me.sd.range or cv.range can be entered

  # new_0.14: added arguments save.RData and RData.dir
  RData <- list(save=save.RData, dir=RData.dir) # new_0.14


  past.data.summary <- function(past.data.list)
  {
    mean.len <- length(past.data.list$mean)

    if (mean.len > 1 | mean.len != length(past.data.list$sd) | mean.len != length(past.data.list$n))
    {
      stop("Elements mean, sd and n in past.data must be of same length (0 or 1).")
    }

    past.data.list$used <- mean.len == 1


    shape <- (past.data.list$n-1)/2                  # alpha
    rate <- (past.data.list$sd^2)*(past.data$n-1)/2  # beta

    level <- 0.95
    p <- 0.5 + c(-1,1)*level/2
    sigma2.lim <- 1/qgamma(p, shape=shape, rate=rate)

    # Find moments of log(sigma2) ~ N(mu, s2)
    # such that 95% interval endpoints match

    log.sigma2.lim <- log(sigma2.lim)

    sd <- abs(diff(log.sigma2.lim))/2/qnorm((1+level)/2)

    # When sigma2 ~ Inv-Gamma(shape, rate),
    # E(log(sigma2)) = -digamma(shape) + log(rate)
    # [see Wikipedia page for Inverted Gamma distrn]

    mu <- log(rate) - digamma(shape)

    # log(sigma2) ~ N(mu, sd^2)
    # => log(sigma) = 1/2 * log(sigma2)
    #               ~ N(mu/2, sd^2/4)

    past.data.list$log.sigma.mu <- mu/2
    past.data.list$log.sigma.sd <- sd/2

    past.data.list$ns2 <- (past.data.list$n - 1)*(past.data.list$sd^2)

    past.data.list
  } # end of past.data.summary


  sigma.truncatedData.gen.lnorm.object <- function(N, lnorm.mu, lnorm.sigma)
  {
    A <- list(N=N, lm=lnorm.mu, ls2=lnorm.sigma^2, M=0)
    f <- function(s, A){exp(-(A$N+1)*log(s) - A$b/s^2 - A$N*pnorm(A$mu/s,log.p=T) - ((log(s)-A$lm)^2)/(2*A$ls2) - A$M)}
    log.f <- function(s, A){-(A$N+1)*log(s) - A$b/s^2 - A$N*pnorm(A$mu/s,log.p=T) - ((log(s)-A$lm)^2)/(2*A$ls2)}
    log.f.prime  <- function(s, A){z=A$mu/s; l=lphi(z); -(A$N+1)/s + 2*A$b/s^3 - (log(s)-A$lm)/A$ls2/s + A$N*z*l$r/s}
    log.f.second <- function(s, A){z=A$mu/s; l=lphi(z); (A$N+1)/s^2 - 6*A$b/s^4 + (log(s)-A$lm-1)/A$ls2/s^2 + A$N*z*((z^2-2)*l$r+z*l$r2)/s^2}


    # new_0.11
    log.f.inv.remote <- function(target, A)
    {
      c3 <- - (A$N + 1)
      c2 <- A$N + 1 - target
      c1 <- 0
      c0 <- -A$b

      t <- logPhi.quadratic.approx.coeff(degree=3)
      t <- -A$N * t * c(1, A$mu, A$mu^2, A$mu^3)

      theta <- c(c0, c1, c2, c3) + t
      roots <- real.cubic.roots(theta, l=0)


      C <- - A$lm^2/2/A$ls2 + A$N*log(2)
      B <- A$lm/A$ls2 - (A$N + 1)
      A <- -1/2/A$ls2
      tmp <- quadratic.solution(c(C, B, A), target=target)

      roots <- c(roots, exp(tmp))

      roots
    } # end of log.f.inv.remote


    o <- list(A=A, range=c(0, Inf), f=f, log.f=log.f, log.f.prime=log.f.prime, log.f.second=log.f.second,
              log.f.inv.remote=log.f.inv.remote,
              potentially.bimodal=F, math.lower.limit=0)

    o #
  } # end of sigma.truncatedData.gen.lnorm.object


  me <- any.me(me.sd.range, cv.range) # see if measurement error is desired [new_0.10]

  # Remove y values that are NAs and count number of censored values (y = NA and lt and/or gt not NA)
  data <- data.summary(y=y, lt=lt, gt=gt, interval.lower=interval.lower, interval.upper=interval.upper,
                       logNormal.distrn=outcome.is.logNormally.distributed, me.through.cv=me$through.cv)


  past.data <- past.data.summary(past.data)

  if (past.data$used)
  {
    log.sigma.mu <- past.data$log.sigma.mu
    log.sigma.sd <- past.data$log.sigma.sd
    log.sigma.prec <- NA # definition not necessary, as we will use log.sigma.sd hereafter
  }
  else
  {
    log.sigma.sd <- 1/sqrt(log.sigma.prec)
  }

  mu.lim <- c(mu.lower, mu.upper)


  # new_0.10 [block]
  gen.y <- list()
  true.values <- list()


  # Prepare dens.gen.icdf objects

  if (me$any) o.tv <- truevalue.gen.object(me, outcome.is.logNormally.distributed)


  # modif_0.12

  combined.N <- data$N + ifelse(past.data$used, past.data$n, 0)

  if (me$through.cv & !outcome.is.logNormally.distributed)
  {
    o.mu <- mu.truncatedData.gen.object(combined.N) # modif_0.12
    o.sigma <- sigma.truncatedData.gen.lnorm.object(combined.N, log.sigma.mu, log.sigma.sd) # modif_0.12
  }
  else
  {
    o.sigma <- sigma.gen.object(combined.N, log.sigma.mu, log.sigma.sd) # modif_0.12
  }


  if (me$any & !me$known) o.me <- me.gen.object(me, outcome.is.logNormally.distributed, data$N)


  # new_0.14
  if (RData$save) sigma.save.objects <- paste(RData$dir, '_sigma.RData', sep='/')
  else sigma.save.objects <- character(0)


  # Prepare output objects with appropriate dimensions

  sample <- empty.matrices(n.iter, n.chains, me)

  burnin <- list()
  if (monitor.burnin) burnin <- empty.matrices(n.burnin, n.chains, me)



  M.iter <- n.burnin + n.iter * n.thin

  for (ch in seq(n.chains))
  {
    saved.iter <- 0

    mu <- init.mu[ch] # Initial values for mu and sigma
    sigma <- init.sigma[ch]


    # Initialize measured values for subjects with censored values [new_0.10]
    if (data$any.censored$any) gen.y <- y.gen.inits(data, mu, sigma, me$through.cv, outcome.is.logNormally.distributed)

    if (me$any) me$parm <- me$init # initialize Measurement Error parameter value


    for (iter in 1:M.iter)
    {
      # Sample true values (in presence of measurement error) [new_0.10]
      if (me$any) true.values <- truevalues.gen(gen.y, data, mu, sigma, me, outcome.is.logNormally.distributed, o=o.tv, RData=RData) # modif_0.14

      # Sample y latent values for subjects with censored values
      if (data$any.censored$any) gen.y <- y.gen(true.values, data, sigma, me, outcome.is.logNormally.distributed, mu=mu)

      # Compute data points sum and square sum
      moments <- out.logout.moments(me$any, outcome.is.logNormally.distributed, data, gen.y, true.values)


      # Sample from f(sigma | mu)
      # modif_0.10

      sigma.beta <- (moments$sum2 - 2*mu*moments$sum + data$N*mu^2)/2
      if (past.data$used) sigma.beta <- sigma.beta + past.data$n/2*((past.data$mean-mu)^2) + past.data$ns2/2


      if (me$through.cv & !outcome.is.logNormally.distributed)
      {
        A <- c(o.sigma$A, list(b=sigma.beta, mu=mu))
        start <- sigma
        inestimable.lower.limit <- F
      }
      else
      {
        A <- c(o.sigma$A, list(b=sigma.beta))
        start <- o.sigma$start(A)
        inestimable.lower.limit <- T
      }


      sigma <- dens.gen.icdf(o.sigma, A, range=c(0, Inf), start=start, inestimable.lower.limit=inestimable.lower.limit, save.objects=sigma.save.objects)


      # Sample from f(mu | sigma)
      # modif_0.10

      y.bar <- moments$sum / data$N # new_0.12 (was called mu.cond.mean in earlier versions)
      mu.cond.mean <- ifelse(past.data$used, (moments$sum + past.data$n*past.data$mean) / combined.N, y.bar) # new_0.12


      # modif_0.12

      if (me$through.cv & !outcome.is.logNormally.distributed)
      {
        mu <- mu.truncatedData.gen(o.mu, mu.lim, mu.cond.mean, sigma)
      }
      else
      {
        mu.cond.sd <- sigma / sqrt(combined.N) # modif_0.12

        p.lim <- pnorm((mu.lim - mu.cond.mean)/mu.cond.sd)
        p <- runif(1, min=p.lim[1], max=p.lim[2])
        mu <- qnorm(p, mean=mu.cond.mean, sd=mu.cond.sd)
      }


      # Sample Measurement Error from its posterior density

      if (me$any & !me$known) me$parm <- me.gen(o.me, me, data, gen.y, true.values)


      # Save values only when iter# modulo thinning = 0

      if (iter <= n.burnin)
      {
        if (monitor.burnin)
        {
          burnin$mu[ch, iter] <- mu
          burnin.$sd[ch, iter] <- sigma

          if (me$any & !me$known) burnin$me.parm[ch, iter] <- me$parm
        }
      }
      else if ((iter-n.burnin)%%n.thin == 0)
      {
        saved.iter <- saved.iter + 1
        sample$mu[ch, saved.iter] <- mu
        sample$sd[ch, saved.iter] <- sigma

        if (me$any & !me$known) sample$me.parm[ch, saved.iter] <- me$parm
      }


      ###progressbar         **PROGRESS

      if (is.function(updateProgress) & (iter/1000)==trunc(iter/1000)) {
        text <- paste0(iter,"/",M.iter)
        updateProgress(detail = text)
      }


    }
  }


  # Prepare output

  out <- out.sample(sample, burnin, n.chains, monitor.burnin, me, outcome.is.logNormally.distributed)
  out$past.data <- past.data

  out$mcmc  <- list(n.chains=n.chains, n.burnin=n.burnin, n.iter=n.iter, n.thin=n.thin, monitor.burnin=monitor.burnin)
  out$inits <- list(mu=init.mu, sigma=init.sigma)
  out$data  <- list(y=y, lt=lt, gt=gt, interval.lower=interval.lower, interval.upper=interval.upper)
  out$parms <- list(mu.lower=mu.lower, mu.upper=mu.upper, log.sigma.mu=log.sigma.mu, log.sigma.prec=log.sigma.prec,
                    outcome.is.logNormally.distributed=outcome.is.logNormally.distributed)

  me[c("known", "parm")] <- NULL # Drop these items from 'me' list
  out$me <- me

  out
} # end of SEG.informedvar
