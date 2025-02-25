# Version 0.15 (July 2017)


  # Change Log *****************************************************************
  #
  #   When updated, look for comments with new_* and modif_*
  #   to rapidly identify new/modified code.
  #
  #
  # Version 0.15
  # ------------
  #   Added argument save.RData
  #     -> useful at development stage only, for monitoring parameters values when dens.gen.icdf is crashing
  #
  # Version 0.14
  # ------------
  #   - A starting point was added to a call to dens.gen.icdf when generating
  #     a value for sigma.within (see: new_0.14)
  #
  # Version 0.12 & 0.13
  # -------------------
  #   - A few errors were corrected.
  #     Thanks to Fran�ois Lemay for highlighting these mistakes.
  #
  # Version 0.11
  # ------------
  #  - New function(s):
  #     - Default.inits.local
  #
  #  - math.limits was changed to math.lower.limit
  #
  #  - Modified function(s):
  #      mu.truncatedData.gen.local
  #        added $log.f to list A
  #      mu.truncatedData.gen.local.object
  #
  #      truevalues.gen.local
  #        > added log.y values to calls to truevalue.gen
  #
  #  - added sw2=sigma.within^2 to A list in mu.truncatedData.gen.local
  #
  #  - Now using:
  #     out.logout (defined locally)
  #
  #  - also using argument logNormal.distrn in call to data.summary
  #
  #
  # Version 0.10 [replaces model.McNally]:
  # ------------
  #  - ids was defined: a vector [factor] with patient id numbers
  #  - used function sqrt.invertedGamma.gen in a few occurences
  #  - dropped definitions of tau.within.range and tau.between.range
  #  - dropped which.censored
  #  - me.sd.range & cv.range were added
  #     only one of me.sd.range or cv.range must be specified [of length 2, if any]
  #  - added init.mu.overall and init.sigma.within to the list of fct arguments
  #  - using functions:
  #      empty.matrices.local
  #      me.gen
  #      me.gen.object
  #      mu.truncatedData.gen.local
  #      mu.truncatedData.gen.local.object
  #      mu.worker.truncatedData.gen
  #      mu.worker.truncatedData.gen.object
  #      sigma.gen.object
  #      sigma.within.truncatedData.gen
  #      sigma.within.truncatedData.gen.object
  #      truevalue.gen
  #      truevalue.gen.object
  #      truevalues.gen.local
  #      y.gen.local
  #   - NOT using sigma.gen.icdf anymore
  #   > Look for new_* and modif_ in comments for more modifications


Between.worker <- function(y=numeric(0), worker=numeric(0),
  lt=numeric(0), worker.lt=numeric(0),
  gt=numeric(0), worker.gt=numeric(0),
  interval.lower=numeric(0), interval.upper=numeric(0), worker.interval=numeric(0),
  n.chains=1, n.iter=15000, n.burnin=500, n.thin=1, monitor.burnin=F,
  log.sigma.between.mu=-0.8786, log.sigma.between.prec=1.634,
  log.sigma.within.mu=-0.4106, log.sigma.within.prec=1.9002,
  mu.overall.lower=-100, mu.overall.upper=100,
  outcome.is.logNormally.distributed=T,
  use.uniform.prior.on.sds=F, sigma.between.range=c(0,100), sigma.within.range=c(0,100),
  me.sd.range=numeric(0), cv.range=numeric(0),
  init.mu.overall=rep(default.inits$mu.overall, n.chains),
  init.sigma.within=rep(default.inits$sigma.within, n.chains),
  save.RData=F, RData.dir='c:/users/jerome')
{
  # Notes:
  # - sigma.between.range & sigma.within.range           are relevant only when  use.uniform.prior.on.sds = TRUE
  # -   log.sigma.between.mu,   log.sigma.between.prec,
  #     log.sigma.within.mu and log.sigma.within.prec    are relevant only when  use.uniform.prior.on.sds = FALSE


  # -----------------------------------------------------------------------------------------------------------------------
  # Useful functions


  # modif_0.13

  Default.inits.local <- function(data, logNormal.distrn, log.sigma.within.mu, use.uniform.prior.on.sds, include.censored.data=F)
  {
    # new_0.13 added arguments use.uniform.prior.on.sds & include.censored.data to this function

    sigma.within <- numeric(0)

    normalized.y <- numeric(0)
    ids <- numeric(0) # data frame with worker id numbers


    if (data$size$y > 0)
    {
      if (logNormal.distrn) normalized.y <- data$log$y
      else normalized.y <- data$y

      ids <- data$id$y
    }


    if (include.censored.data)
    {
      if (data$size$gt > 0)
      {
        if (logNormal.distrn) normalized.y <- c(normalized.y, data$log$gt)
        else normalized.y <- c(normalized.y, data$gt)

        ids <- c(ids, data$id$gt)
      }

      if (data$size$lt > 0)
      {
        if (logNormal.distrn) normalized.y <- c(normalized.y, data$log$lt)
        else normalized.y <- c(normalized.y, data$lt)

        ids <- c(ids, data$id$lt)
      }

      if (data$size$i > 0)
      {
        if (logNormal.distrn) normalized.y <- c(normalized.y, data$log$i)
        else normalized.y <- c(normalized.y, data$i)

        ids <- c(ids, data$id$i)
      }
    }


    if (length(ids) > 0)
    {
      ids <- as.factor(ids)
      df <- data.frame(y=normalized.y, id=ids)

      # Compute mean for each worker
      # and take the mean of worker means as an initial value for mu.overall

      worker.means <- aggregate(y~id, df, mean)$y
      mu.overall <- mean(worker.means)

      # Compute the sd of values measured for each patient
      # and take their mean as an initial value for sigma.within

      worker.sds <- aggregate(y~id, df, sd)$y
      worker.sds <- worker.sds[!is.na(worker.sds)]

      # modif_0.13

      if (length(worker.sds) == 0)
      {
        if (length(worker.means) > 1) sigma.within <- sd(worker.means)
      }
      else
      {
        sigma.within <- mean(worker.sds)
      }
    }
    else
    {
      mu.overall <- 0
      normalized.y <- numeric(0)
    }


    # new_0.13

    if (length(sigma.within) == 0 && !include.censored.data)
    {
      # call this fct again, but this time including censored data
      tmp <- Default.inits.local(data, logNormal.distrn, log.sigma.within.mu, use.uniform.prior.on.sds, include.censored.data=T)
      sigma.within <- tmp$sigma.within
    }

    # new_0.13

    if (length(sigma.within) == 0)
    {
      if (use.uniform.prior.on.sds)
      {
        sigma.within <- mean(sigma.within.range)
      }
      else
      {
        sigma.within <- exp(log.sigma.within.mu)
      }
    }


    list(mu.overall=mu.overall, sigma.within=sigma.within, normalized.y=normalized.y)
  } # end of Default.inits.local


  empty.matrices.local <- function(n.iter, n.workers, n.chains, me)
  {
    template   <- matrix(NA, nrow=n.chains,           ncol=n.iter)
    template.w <- matrix(NA, nrow=n.workers*n.chains, ncol=n.iter)

    out <- list(mu.overall=template, mu.worker=template.w, sigma.within=template, sigma.between=template)
    if (me$any & !me$known) out$me.parm <- template

    out
  } # end of empty.matrices.local


  mu.truncatedData.gen.local <- function(o, mu.mean, sigma.within, worker.means, range, current.value, RData=list(save=F))
  {
    # mu.mean: scalar
    # sigma.within: scalar
    # worker.means: vector of length '# of workers'
    # range: vector of length 2
    # current.value: scalar

    # new_0.15
    if (RData$save) save.objects <- paste(RData$dir, '_muTruncatedData.RData', sep='/')
    else save.objects <- character(0)

    # Combine o$A and new elements into new A
    A <- c(o$A, list(log.f=o$log.f, mu.mean=mu.mean, sw=sigma.within, sw2=sigma.within^2, muk=worker.means))

    start <- c(o$start(A), current.value)
    mu <- dens.gen.icdf(o, A, range=range, start=start, save.objects=save.objects)

    mu
  } # end of mu.truncatedData.gen.local


  mu.truncatedData.gen.local.object <- function(N, worker.counts)
  {
    # N: scalar
    # worker.counts: vector of length '# of workers'

    A <- list(N=N, nk=worker.counts, n.workers=length(worker.counts), M=0)
    f <- function(mu, A){exp(-A$N*((mu-A$mu.mean)^2)/(2*A$sw2) - as.vector(matrix(A$nk, nrow=1) %*% pnorm((matrix(mu, nrow=A$n.workers, ncol=length(mu), byrow=T) + A$muk)/A$sw, log.p=T)) - A$M)}
    log.f <- function(mu, A){-A$N*((mu-A$mu.mean)^2)/(2*A$sw2) - sum(A$nk*pnorm((mu+A$muk)/A$sw, log.p=T))}
    log.f.prime  <- function(mu, A){z=(mu+A$muk)/A$sw; l=lphi(z); -A$N*(mu-A$mu.mean)/A$sw2  - sum(A$nk*l$r)/A$sw} # z & l are of length = # of workers
    log.f.second <- function(mu, A){z=(mu+A$muk)/A$sw; l=lphi(z); (-A$N + sum(A$nk*(z*l$r + l$r2)))/A$sw2}  # z & l are of length = # of workers


    # new_0.11
    log.f.inv.remote <- function(target, A)
    {
      solutions <- numeric(0)

      # solutions on the low range

      theta <- logPhi.quadratic.approx.coeff(degree=3)
      O <- order(A$muk, decreasing=T)


      for (i in seq(along.width=O))
      {
        w <- O[seq(i)]

        c0 <- -A$N*A$mu.mean^2/2/A$sw2 - theta[1]*sum(A$nk[w]) - theta[2]/A$sw*sum(A$nk[w]*A$muk[w]) - theta[3]/A$sw2*sum(A$nk[w]*A$muk[w]^2) - theta[4]/A$sw^3*sum(A$nk[w]*A$muk[w]^3)
        c1 <- A$N*A$mu.mean/A$sw2 - 2*theta[3]/A$sw2*sum(A$nk[w]*A$muk[w]) - theta[2]/A$sw*sum(A$nk[w]) - 3*theta[4]/A$sw^3*sum(A$nk[w]*A$muk[w]^2)
        c2 <- -A$N/2/A$sw2 - theta[3]/A$sw2*sum(A$nk[w]) - 3*theta[4]/A$sw^3*sum(A$nk[w]*A$muk[w])
        c3 <- -theta[4]/A$sw^3*sum(A$nk[w])
        coeff <- c(c0, c1, c2, c3)

        tmp <- real.cubic.roots(coeff, target=target)
        solutions <- c(solutions, tmp)
      }


      # solutions on the high range

      O <- order(A$muk)


      for (i in seq(along.width=O))
      {
        w <- O[seq(i)]

        c0 <- -A$N*A$mu.mean^2/2/A$sw2 - theta[1]*sum(A$nk[w]) - theta[2]/A$sw*sum(A$nk[w]*A$muk[w]) - theta[3]/A$sw2*sum(A$nk[w]*A$muk[w]^2) - theta[4]/A$sw^3*sum(A$nk[w]*A$muk[w]^3)
        c1 <- A$N*A$mu.mean/A$sw2 - 2*theta[3]/A$sw2*sum(A$nk[w]*A$muk[w]) - theta[2]/A$sw*sum(A$nk[w]) - 3*theta[4]/A$sw^3*sum(A$nk[w]*A$muk[w]^2)
        c2 <- -A$N/2/A$sw2 - theta[3]/A$sw2*sum(A$nk[w]) - 3*theta[4]/A$sw^3*sum(A$nk[w]*A$muk[w])
        c3 <- -theta[4]/A$sw^3*sum(A$nk[w])
        coeff <- c(c0, c1, c2, c3)

        tmp <- real.cubic.roots(coeff, target=target)
        solutions <- c(solutions, tmp)
      }


      solutions
    } # end of log.f.inv.remote


    start <- function(A)
    {
      mu <- A$mu.mean
      z <- (mu+A$muk)/A$sw
      l <- lphi(z)
      g0 <- - sum(A$nk*l$r)/A$sw
      gp0 <- sum(A$nk*(z*l$r + l$r2))/A$sw2

      a <- A$N * A$mu.mean / A$sw2 + g0 - gp0 * mu
      b <- -A$N / A$sw2 + gp0

      start <- -a/b
      start[start>0]
    } # end of start


    o <- list(A=A, f=f, log.f=log.f, log.f.prime=log.f.prime, log.f.second=log.f.second,
             log.f.inv.remote=log.f.inv.remote,
             start=start, potentially.bimodal=F, math.lower.limit=-Inf)

    o
  } # end of mu.truncatedData.gen.local.object


  mu.worker.truncatedData.gen <- function(o, muk.star, s2k.star, mu, sigma.within, current.values, RData=list(save=F))
  {
    # muk.star, s2k.star, current.values: vectors of length '# of workers'
    # mu, sigma.within: scalar

    # new_0.15
    if (RData$save) save.objects <- paste(RData$dir, '_muWorkerTruncatedData.RData', sep='/')
    else save.objects <- character(0)

    A <- c(o$A, list(mu=mu, sw=sigma.within, sw2=sigma.within^2, muj.star=0, s2j.star=0, nj=0))
    # elements set to 0 above will be updated in the for-loop below

    new.mu <- rep(NA, A$n.workers)

    for (i in seq(A$n.workers))
    {
      A$muj.star <- muk.star[i]
      A$s2j.star <- s2k.star[i]
      A$nj       <- o$A$nk[i]

      start <- c(o$start(A), current.values[i])
      new.mu[i] <- dens.gen.icdf(o, A, range=c(-Inf,Inf), start=start, save.objects=save.objects)
    }


    new.mu
  } # end of mu.worker.truncatedData.gen


  mu.worker.truncatedData.gen.object <- function(nk)
  {
    # nk: vector of length '# of workers'

    n.workers <- length(nk)
    range <- c(-Inf, Inf)

    A <- list(n.workers=n.workers, nk=nk, range=range, M=0)

    f <- function(muj, A){exp(-((muj-A$muj.star)^2)/2/A$s2j.star - A$nj*pnorm((A$mu+muj)/A$sw, log.p=T) - A$M)}
    log.f <- function(muj, A){-((muj-A$muj.star)^2)/2/A$s2j.star - A$nj*pnorm((A$mu+muj)/A$sw, log.p=T)}
    # in below two lines:
    #   z: scalar
    #   l: list (dimensions $r & $r2)
    log.f.prime  <- function(muj, A){z=(A$mu+muj)/A$sw; l=lphi(z); (A$muj.star-muj)/A$s2j.star - A$nj*l$r/A$sw}
    log.f.second <- function(muj, A){z=(A$mu+muj)/A$sw; l=lphi(z); -1/A$s2j.star + A$nj*(z*l$r + l$r2)/A$sw2}


    # new_0.11
    log.f.inv.remote <- function(target, A)
    {
      # low-range solutions

      log.phi <- dnorm(A$mu/A$sw, log=T)
      log.H0 <- pnorm(A$mu/A$sw, log.p=T)
      log.Hp0 <- log.phi - log(A$sw)
      log.Hs0 <- log(abs(A$mu)) - 3*log(A$sw) + log.phi
      f0 <- -A$nj*log.H0
      fp0 <- -A$nj*exp(log.Hp0-log.H0)
      fs0 <- -A$nj*(exp(log.Hs0+log.H0-2*log.H0) - exp(2*(log.Hp0-log.H0)))

      aj <- -0.5/A$s2j.star
      C <- f0 + aj*A$muj.star^2 - target
      B <- fp0 - 2*A$muj.star*aj
      qA <- fs0/2 + aj

      roots <- quadratic.solution(c(C, B, qA))

      # high-range solutions

      if (target < 0) roots <- c(roots, A$muj.star + sqrt(-2*target*A$s2j.star))

      roots
    } # end of log.f.inv.remote


    start <- function(A)
    {
      muj <- A$muj.star
      z <- (A$mu + muj) / A$sw
      l <- lphi(z)
      g0 <- - A$nj*l$r / A$sw
      gp0 <- A$nj * (z*l$r + l$r2) / A$sw2
      a <- A$muj.star / A$s2j.star + g0 - gp0 * muj
      b <- -1/A$s2j.star + gp0

      start <- -a/b
      start[start>0]
    } # end of start


    o <- list(A=A, f=f, log.f=log.f, log.f.prime=log.f.prime, log.f.second=log.f.second,
              log.f.inv.remote=log.f.inv.remote,
              start=start, potentially.bimodal=F, math.lower.limit=-Inf)

    o
  } # end of mu.worker.truncatedData.gen.object


  out.logout <- function(any.me, logNormal.distrn, data, gen.y, true.values)
  {
    if (any.me)
    {
      if (logNormal.distrn)
      {
        y <- c(true.values$log$y, true.values$log$gt, true.values$log$lt, true.values$log$i)
      }
      else
      {
        y <- c(true.values$y, true.values$gt, true.values$lt, true.values$i)
      }
    }
    else if (logNormal.distrn)
    {
      y <- c(data$log$y, gen.y$log$gt, gen.y$log$lt, gen.y$log$i)
    }
    else
    {
      y <- c(data$y, gen.y$gt, gen.y$lt, gen.y$i)
    }

    y
  } # end of out.logout


  sigma.within.truncatedData.gen <- function(b, mu, mu.worker, start=numeric(0), o=o.sw, RData=list(save=F))
  {
    # b, mu, start.value: scalars
    # mu.worker: vector of length '# of workers'

    # new_0.15
    if (RData$save) save.objects <- paste(RData$dir, '_swTruncatedData.RData', sep='/')
    else save.objects <- character(0)

    A <- c(o$A, list(b=b, mu=mu, muk=mu.worker))
    sigma.within <- dens.gen.icdf(o, A, range=o$range, start=start, inestimable.lower.limit=o$inestimable.lower.limit, save.objects=save.objects)

    sigma.within
  } # end of sigma.within.truncatedData.gen


  sigma.within.truncatedData.gen.object <- function(N, nk, use.uniform.prior.on.sds, range=numeric(0), lnorm.mu=numeric(0), lnorm.sd=numeric(0))
  {
    # N: scalar
    # nk: vector of length '# of workers'
    # use.uniform.prior.on.sds: logical
    # range: vector of length 2 (when use.uniform.prior.on.sds = TRUE) or null vector (length 0)
    # lnorm.mu, lnorm.sd: left empty (when use.uniform.prior.on.sds = TRUE) or scalars


    if (use.uniform.prior.on.sds)
    {
      inestimable.lower.limit <- range[1] == 0
      A <- list(N=N, nk=nk, n.workers=length(nk), M=0)


      f <- function(sw, A){exp(-A$N*log(sw) - A$b/sw^2 - as.vector(matrix(A$nk, nrow=1) %*% pnorm((A$mu+A$muk)/matrix(sw, nrow=A$n.workers, ncol=length(sw), byrow=T), log.p=T)) - A$M)}

      log.f <- function(sw, A){-A$N*log(sw) - A$b/sw^2 - sum(A$nk*pnorm((A$mu+A$muk)/sw, log.p=T))}
      log.f.prime  <- function(sw, A){z=(A$mu+A$muk)/sw; l=lphi(z); -A$N/sw + 2*A$b/sw^3 + sum(A$nk*z*l$r)/sw}                      # z & l are vectors of length = # of workers
      log.f.second <- function(sw, A){z=(A$mu+A$muk)/sw; l=lphi(z); A$N/sw^2 - 6*A$b/sw^4 + sum(A$nk*z*((z^2-2)*l$r+z*l$r2))/sw^2}  # z & l are vectors of length = # of workers

      # modif_0.11
      log.f.inv.remote <- function(target, A)
      {
        # A$mu: scalar
        # A$muk: vector of length '# of workers'

        # low-range solutions

        c3 <- -A$N
        c2 <- A$N - target
        c0 <- -A$b
        theta <- c(c0, 0, c2, c3)

        w <- which((A$mu+A$muk) < 0)
        if (length(w) > 0)
        {
          t <- logPhi.quadratic.approx.coeff(degree=3)
          t <- -t * c(sum(A$nk[w]), sum(A$nk[w]*(A$mu+A$muk[w])), sum(A$nk[w]*(A$mu+A$muk[w])^2), sum(A$nk[w]*(A$mu+A$muk[w])^3))
          theta <- theta + t
        }

        roots <- real.cubic.roots(theta, l=0)

        # high-range solutions

        log.s <- (log(2)*sum(A$nk) - target)/A$N
        roots <- c(roots, exp(log.s))

        roots
      } # end of log.f.inv.remote
    }
    else
    {
      range <- c(0, Inf)
      inestimable.lower.limit <- T

      A <- list(N=N, nk=nk, n.workers=length(nk), lm=lnorm.mu, ls2=lnorm.sd^2, M=0)

      f <- function(sw, A){exp(-(A$N+1)*log(sw) - A$b/sw^2 - as.vector(matrix(A$nk, nrow=1)%*%pnorm((A$mu+A$muk)/matrix(sw, nrow=A$n.workers, ncol=length(sw), byrow=T), log.p=T)) - (log(sw)-A$lm)^2/2/A$ls2 - A$M)}

      log.f <- function(sw, A){-(A$N+1)*log(sw) - A$b/sw^2 - sum(A$nk*pnorm((A$mu+A$muk)/sw, log.p=T)) - (log(sw)-A$lm)^2/2/A$ls2}
      log.f.prime  <- function(sw, A){z=(A$mu+A$muk)/sw; l=lphi(z); -(A$N+1)/sw  + 2*A$b/sw^3 - (log(sw)-A$lm)/sw/A$ls2 + sum(A$nk*z*l$r)/sw}                        # z & l are vectors of length = # of workers
      log.f.second <- function(sw, A){z=(A$mu+A$muk)/sw; l=lphi(z); (A$N+1)/sw^2 - 6*A$b/sw^4 + (log(sw)-A$lm-1)/A$ls2/sw^2 + sum(A$nk*z*((z^2-2)*l$r+z*l$r2))/sw^2} # z & l are vectors of length = # of workers

      log.f.inv.remote <- function(target, A)
      {
        # A$mu: scalar
        # A$muk: vector of length '# of workers'

        # low-range solutions

        c0 <- -A$b
        c2 <- -A$lm^2/2/A$ls2 - target
        coeff <- c(c0, 0, c2)

        w <- which((A$mu+A$muk) < 0)
        if (length(w) > 0)
        {
          t <- logPhi.quadratic.approx.coeff()
          t <- t * c(sum(A$nk[w]), sum(A$nk[w]*(A$mu+A$muk[w])), sum(A$nk[w]*(A$mu+A$muk[w])^2))

          coeff <- coeff - t
        }

        roots <- quadratic.solution(coeff, l=0)

        # high-range solutions

        C <- -A$lm^2/2/A$ls2 + sum(A$nk)*log(2) - target
        B <- A$lm/A$ls2 - A$N - 1
        A <- -1/2/A$ls2
        tmp <- quadratic.solution(c(C, B, A), l=0)
        roots <- c(roots, exp(max(tmp)))

        roots
      } # end of log.f.inv.remote
    }


    o <- list(A=A, range=range, inestimable.lower.limit=inestimable.lower.limit,
              f=f, log.f=log.f, log.f.prime=log.f.prime, log.f.second=log.f.second,
              log.f.inv.remote=log.f.inv.remote,
              inestimable.lower.limit=T, potentially.bimodal=F, math.lower.limit=0)

    o
  } # end of sigma.within.truncatedData.gen.object


  truevalues.gen.local <- function(gen.y, data, me, mu.worker, mu=mu.overall, sigma=sigma.within, logNormal.distrn=outcome.is.logNormally.distributed, o=list(), RData=list(save=F))
  {
    new.truevalues <- list(y=rep(0, data$size$y), gt=rep(0, data$size$gt), lt=rep(0, data$size$lt), i=rep(0, data$size$i))


    if (me$through.sd & !logNormal.distrn)
    {
      me.sd <- me$parm
      tau.star <- 1/sigma^2 + 1/me.sd^2
      sd.star <- 1/sqrt(tau.star)

      if (data$size$y > 0)
      {
        tmp.mean <- (data$y/me.sd^2 + (mu + mu.worker[data$id$y])/sigma^2) / tau.star
        new.truevalues$y <- rnorm(data$size$y, tmp.mean, sd.star)
      }

      if (data$size$gt > 0)
      {
        tmp.mean <- (gen.y$gt/me.sd^2 + (mu + mu.worker[data$id$gt])/sigma^2) / tau.star
        new.truevalues$gt <- rnorm(data$size$gt, tmp.mean, sd.star)
      }

      if (data$size$lt > 0)
      {
        tmp.mean <- (gen.y$lt/me.sd^2 + (mu + mu.worker[data$id$lt])/sigma^2) / tau.star
        new.truevalues$lt <- rnorm(data$size$lt, tmp.mean, sd.star)
      }

      if (data$size$i > 0)
      {
        tmp.mean <- (gen.y$i/me.sd^2 + (mu + mu.worker[data$id$i])/sigma^2) / tau.star
        new.truevalues$i <- rnorm(data$size$i, tmp.mean, sd.star)
      }
    }
    else
    {
      o$A$sigma2 <- sigma^2


      if (data$size$y > 0)
      {
        for (j in 1:data$size$y)
        {
          new.truevalues$y[j] <- truevalue.gen(o, me, data$y[j], mu+mu.worker[data$id$y[j]], log.y=data$log$y[j], RData=RData)
        }
      }

      if (data$size$gt > 0)
      {
        for (j in 1:data$size$gt)
        {
          new.truevalues$gt[j] <- truevalue.gen(o, me, gen.y$gt[j], mu+mu.worker[data$id$gt[j]], log.y=gen.y$log$gt[j], RData=RData)
        }
      }

      if (data$size$lt > 0)
      {
        for (j in 1:data$size$lt)
        {
          new.truevalues$lt[j] <- truevalue.gen(o, me, gen.y$lt[j], mu+mu.worker[data$id$lt[j]], log.y=gen.y$log$lt[j], RData=RData)
        }
      }

      if (data$size$i > 0)
      {
        for (j in 1:data$size$i)
        {
          new.truevalues$i[j] <- truevalue.gen(o, me, gen.y$i[j], mu+mu.worker[data$id$i[j]], log.y=gen.y$log$i[j], RData=RData)
        }
      }


      if (logNormal.distrn) new.truevalues$log <- take.log(new.truevalues, include.ydim=T) # modif_0.11
    }


    new.truevalues
  } # end of truevalues.gen.local


  y.gen.local <- function(true.values, data, me, mu.worker, mu=mu.overall, sigma=sigma.within, logNormal.distrn=outcome.is.logNormally.distributed)
  {
    if (me$any)
    {
      new.gen.y <- y.gen(true.values, data, sigma, me, logNormal.distrn) # mu is irrelevant
    }
    else
    {
      new.gen.y <- list()

      if (logNormal.distrn)
      {
        if (data$any.censored$gt)
        {
          new.gen.y$log$gt <- rnorm.censored(mu+mu.worker[data$id$gt], sigma, lower=data$log.cens$gt)
          new.gen.y$gt <- exp(new.gen.y$log$gt)
        }

        if (data$any.censored$lt)
        {
          new.gen.y$log$lt <- rnorm.censored(mu+mu.worker[data$id$lt], sigma, upper=data$log.cens$lt)
          new.gen.y$lt <- exp(new.gen.y$log$lt)
        }

        if (data$any.censored$i)
        {
          new.gen.y$log$i  <- rnorm.censored(mu+mu.worker[data$id$i],  sigma, lower=data$log.cens$i$gt, upper=data$log.cens$i$lt)
          new.gen.y$i <- exp(new.gen.y$log$i)
        }
      }
      else
      {
        if (data$any.censored$gt) new.gen.y$gt <- rnorm.censored(mu+mu.worker[data$id$gt], sigma, lower=data$cens$gt)
        if (data$any.censored$lt) new.gen.y$lt <- rnorm.censored(mu+mu.worker[data$id$lt], sigma, upper=data$cens$lt)
        if (data$any.censored$i)  new.gen.y$i  <- rnorm.censored(mu+mu.worker[data$id$i],  sigma, lower=data$cens$i$gt, upper=data$cens$i$lt)
      }
    }

    new.gen.y
  } # end of y.gen.local


  # -------------------------------------------------------------------------------------------------------------


  if (length(worker) != length(y))     stop("y and worker must be of same length.")
  if (length(worker.lt) != length(lt)) stop("lt and worker.lt must be of same length.")
  if (length(worker.gt) != length(gt)) stop("gt and worker.gt must be of same length.")
  if (length(worker.interval) != length(interval.lower) | length(worker.interval) != length(interval.upper)) stop("interval.lower, interval.upper and worker.interval must be of same length.") # modif_0.11


  # new_0.15: added arguments save.RData and RData.dir
  RData <- list(save=save.RData, dir=RData.dir) # new_0.15

  # new_0.15
  if (RData$save)
  {
    sb.save.objects <- paste(RData$dir, '_sb.RData', sep='/')
    sw.save.objects <- paste(RData$dir, '_sw.RData', sep='/')
  }
  else
  {
    sb.save.objects <- character(0)
    sw.save.objects <- character(0)
  }


  b.min <- 1e-8

  me <- any.me(me.sd.range, cv.range) # see if measurement error is desired [new_0.10]

  data <- data.summary(y=y, lt=lt, gt=gt, interval.lower=interval.lower, interval.upper=interval.upper,
                       worker=worker, worker.lt=worker.lt, worker.gt=worker.gt, worker.interval=worker.interval,
                       logNormal.distrn=outcome.is.logNormally.distributed, me.through.cv=me$through.cv)
                       # me$through.cv argument was added above [new_0.10]

  # new_0.11
  default.inits <- Default.inits.local(data, outcome.is.logNormally.distributed, log.sigma.within.mu, use.uniform.prior.on.sds) # modif_0.12


  if (length(init.mu.overall) != n.chains) stop("init.mu.overall must be of length 'n.chains', that is: ", n.chains)


  # Identify lines corresponding to left-, right- or interval-censored entries

  # Definition of df was dropped at this point [modif_0.10]

  ids <- data$id$y
  if (data$any.censored$any) ids <- c(ids, data$id$gt, data$id$lt, data$id$i) # modif_0.10
  ids <- as.factor(ids)

  mu.overall.range <- c(mu.overall.lower, mu.overall.upper) # new_0.10

  log.sigma.within.sd  <- 1/sqrt(log.sigma.within.prec)
  log.sigma.between.sd <- 1/sqrt(log.sigma.between.prec)


  if (!data$any.censored$any & use.uniform.prior.on.sds)
  {
    # If there is no variation observed within subjects,
    # then we require a non-null lower bound for sigma.within

    problem <- all(data$worker$count==1)

    if (!problem)
    {
      df <- data.frame(y=default.inits$normalized.y, id=as.factor(data$id$y))
      y.worker.var <- aggregate(y~id, df, var)$y
      y.worker.var <- y.worker.var[!is.na(y.worker.var)]
      problem <- all(y.worker.var==0)
    }

    if (problem & sigma.within.range[1] == 0) stop("Lower bound for sigma.within must be > 0 due to observed null within-subjects variance.")
  }


  # Prepare dens.gen.icdf objects


  if (me$any) o.tv <- truevalue.gen.object(me, outcome.is.logNormally.distributed)


  if (me$through.cv & !outcome.is.logNormally.distributed)
  {
    o.mu.overall <- mu.truncatedData.gen.local.object(data$N, data$worker$count)
    o.mu.worker <- mu.worker.truncatedData.gen.object(data$worker$count)
  }


  if (use.uniform.prior.on.sds)
  {
    if (data$n.workers <= 1) o.sb <- sigma.gen.object(data$n.workers)
    else o.sb <- list()
  }
  else
  {
    o.sb <- sigma.gen.object(data$n.workers, log.sigma.between.mu, log.sigma.between.sd)
  }


  if (me$through.cv & !outcome.is.logNormally.distributed)
  {
    if (use.uniform.prior.on.sds)
    {
      o.sw <- sigma.within.truncatedData.gen.object(data$N, data$worker$count, T, range=sigma.within.range)
    }
    else
    {
      o.sw <- sigma.within.truncatedData.gen.object(data$N, data$worker$count, F, lnorm.mu=log.sigma.within.mu, lnorm.sd=log.sigma.within.sd)
    }
  }
  else
  {
    if (use.uniform.prior.on.sds)
    {
      if (data$N <= 1)
      {
        o.sw <- sigma.gen.object(data$N)
      }
      else
      {
        o.sw <- list()
      }
    }
    else
    {
      o.sw <- sigma.gen.object(data$N, lnorm.mu=log.sigma.within.mu, lnorm.sd=log.sigma.within.sd)
    }
  }


  if (me$any & !me$known) o.me <- me.gen.object(me, outcome.is.logNormally.distributed, data$N)


  # Prepare output objects with appropriate dimensions

  sample <- empty.matrices.local(n.iter, data$n.workers, n.chains, me)

  burnin <- list()
  if (monitor.burnin) burnin <- empty.matrices.local(n.burnin, data$n.workers, n.chains, me)


  sample.id <- data.frame(worker=rep(data$worker$patkey, n.chains), chain=rep(seq(n.chains), rep(data$n.workers, n.chains)))

  gen.y <- list()
  true.values <- list()

  # Start MCMC

  M.iter <- n.burnin + n.iter * n.thin

  for (ch in seq(n.chains))
  {
    saved.iter <- 0


    # Set initial values

    mu.overall   <- init.mu.overall[ch]
    sigma.within <- init.sigma.within[ch]


    # Initialize measured values for subjects with censored values [new_0.10]
    if (data$any.censored$any) gen.y <- y.gen.inits(data, mu.overall, sigma.within, me$through.cv, outcome.is.logNormally.distributed)

    # modif_0.12
    if (outcome.is.logNormally.distributed) tmp <- c(data$log$y, gen.y$log$gt, gen.y$log$lt, gen.y$log$i)
    else tmp <- c(data$y, gen.y$gt, gen.y$lt, gen.y$i)

    df <- data.frame(y=tmp, id=ids)

    # For mu.worker, take observed mean by worker
    mu.worker <- aggregate(y~id, data=df, mean)$y

    mu.overall <- mean(mu.worker)
    if (mu.overall < mu.overall.lower)      mu.overall <- mu.overall.lower # new_0.10
    else if (mu.overall > mu.overall.upper) mu.overall <- mu.overall.upper # new_0.10
    mu.worker <- mu.worker - mu.overall # center mu.worker

    # For sigma.within, use sd of initial values around worker predicted means
    predicted <- mu.overall + mu.worker[df$id]
    sigma.within <- sd(df$y-predicted)

    if (me$any) me$parm <- me$init # initialize Measurement Error parameter value


    for (iter in 1:M.iter)
    {
      # Sample true values (in presence of measurement error) [new_0.10]
      if (me$any) true.values <- truevalues.gen.local(gen.y, data, me, mu.worker, o=o.tv, RData=RData)

      # Sample y values for censored observations
      if (data$any.censored$any) gen.y <- y.gen.local(true.values, data, me, mu.worker)


      # Compute subject means (including newly imputed values, where indicated)

      tmp <- out.logout(me$any, outcome.is.logNormally.distributed, data, gen.y, true.values)

      df <- data.frame(y=tmp, id=ids)
      y.worker.avg <- aggregate(y~id, df, mean)$y
      y.avg <- mean(df$y)


      # Sample from f(sigma.within | other parms)

      residuals <- df$y - mu.overall - mu.worker[ids]
      b <- sum(residuals^2)/2


      if (me$through.cv & !outcome.is.logNormally.distributed)
      {
        A <- c(o.sw$A, list(b=b, mu=mu.overall, muk=mu.worker))
        sigma.within <- dens.gen.icdf(o.sw, A, range=o.sw$range, start=sigma.within, inestimable.lower.limit=o.sw$inestimable.lower.limit, save.objects=sw.save.objects)
      }
      else
      {
        if (use.uniform.prior.on.sds)
        {
          sigma.within <- sqrt.invertedGamma.gen(data$N, b, sigma.within.range, o=o.sw, RData=RData)
        }
        else
        {
          A <- c(o.sw$A, list(b=b, mu=mu.overall, muk=mu.worker))
          sigma.within <- dens.gen.icdf(o.sw, A, range=c(0, Inf), start=o.sw$start(A), inestimable.lower.limit=T, save.objects=sw.save.objects) # modif_0.12
        }
      }


      # Sample from f(sigma.between | other parms)

      b <- sum(mu.worker^2)/2


      if (use.uniform.prior.on.sds)
      {
        sigma.between <- sqrt.invertedGamma.gen(data$n.workers, b, sigma.between.range, o=o.sb, RData=RData)
      }
      else
      {
        A <- c(o.sb$A, list(b=b))
        sigma.between <- dens.gen.icdf(o.sb, A, range=c(0, Inf), start=o.sb$start(A), inestimable.lower.limit=T, save.objects=sb.save.objects)
      }


      # Sample from f(mu.overall | other parms)
      # modif_0.10

      tmp.mean <- y.avg - sum(data$worker$count*mu.worker)/data$N

      if (me$through.cv & !outcome.is.logNormally.distributed)
      {
        mu.overall <- mu.truncatedData.gen.local(o.mu.overall, tmp.mean, sigma.within, mu.worker, mu.overall.range, current.value=mu.overall, RData=RData)
      }
      else
      {
        tmp.sd <- sigma.within/sqrt(data$N)
        mu.overall <- rnorm.censored(tmp.mean, tmp.sd, lower=mu.overall.lower, upper=mu.overall.upper)
      }


      # Sample from f(mu.worker's | other parms)
      # modif_0.10

      sigma2.A <- (sigma.within^2)/data$worker$count          # vector of length '# of workers'
      sigma2.B <- sigma.between^2                             # scalar

      mu.A <- y.worker.avg - mu.overall                       # vector of length '# of workers'

      muk.star <- mu.A * sigma2.B / (sigma2.A + sigma2.B)     # vector of length '# of workers'
      s2k.star <- sigma2.A * sigma2.B / (sigma2.A + sigma2.B) # vector of length '# of workers'


      if (me$through.cv & !outcome.is.logNormally.distributed)
      {
        mu.worker <- mu.worker.truncatedData.gen(o.mu.worker, muk.star, s2k.star, mu.overall, sigma.within, mu.worker, RData=RData)
      }
      else
      {
        mu.worker <- rnorm(data$n.workers, mean=muk.star, sd=sqrt(s2k.star))
      }


      # Sample Measurement Error from its posterior density

      if (me$any & !me$known) me$parm <- me.gen(o.me, me, data, gen.y, true.values, RData=RData)


      # Save values only when iter# modulo thinning = 0

      if (iter <= n.burnin)
      {
        if (monitor.burnin)
        {
          burnin$mu.overall[ch, iter]    <- mu.overall
          burnin$sigma.within[ch, iter]  <- sigma.within
          burnin$sigma.between[ch, iter] <- sigma.between

          w <- seq(data$n.workers) + (ch-1) * data$n.workers + (iter-1) * n.chains * data$n.workers
          burnin$mu.worker[w] <- mu.worker

          if (me$any & !me$known) burnin$me.parm[ch, iter] <- me$parm
        }
      }
      else if ((iter-n.burnin)%%n.thin == 0)
      {
        saved.iter <- saved.iter + 1

        sample$mu.overall[ch, saved.iter]    <- mu.overall
        sample$sigma.within[ch, saved.iter]  <- sigma.within
        sample$sigma.between[ch, saved.iter] <- sigma.between

        w <- seq(data$n.workers) + (ch-1) * data$n.workers + (saved.iter-1) * n.chains * data$n.workers
        sample$mu.worker[w] <- mu.worker

        if (me$any & !me$known) sample$me.parm[ch, saved.iter] <- me$parm
      }
    }
  }


  # Prepare output list

  if (me$any & !me$known)
  {
    sample <- renamed.me.parm(sample, me$through.cv)
    if (monitor.burnin) burnin <- renamed.me.parm(burnin, me$through.cv)
  }

  if (n.chains == 1) sample <- lapply(sample, as.vector)

  out <- list(sample.id=sample.id, sample=sample)

  if (monitor.burnin)
  {
    if (n.chains == 1) burnin <- lapply(burnin, as.vector)
    out$burnin <- burnin

  }

  out$mcmc <- list(n.chains=n.chains, n.burnin=n.burnin, n.iter=n.iter, n.thin=n.thin, monitor.burnin=monitor.burnin)
  out$inits <- list(mu.overall=init.mu.overall, sigma.within=init.sigma.within) # modif_0.10
  out$data <- list(y=y, lt=lt, gt=gt, interval.lower=interval.lower, interval.upper=interval.upper,
                       worker=worker, worker.lt=worker.lt, worker.gt=worker.gt, worker.interval=worker.interval)


  if (use.uniform.prior.on.sds)
  {
    parms.list <- list(sigma.within.range=sigma.within.range, sigma.between.range=sigma.between.range)
  }
  else
  {
    parms.list <- list(log.sigma.between.mu=log.sigma.between.mu, log.sigma.between.prec=log.sigma.between.prec,
                       log.sigma.within.mu=log.sigma.within.mu, log.sigma.within.prec=log.sigma.within.prec)
  }

  parms.list$mu.overall.lower <- mu.overall.lower
  parms.list$mu.overall.upper <- mu.overall.upper
  parms.list$outcome.is.logNormally.distributed <- outcome.is.logNormally.distributed
  parms.list$use.uniform.prior.on.sds <- use.uniform.prior.on.sds

  out$parms <- parms.list

  me[c("known", "parm")] <- NULL # Drop these items from 'me' list
  out$me <- me

  out
} # end of Between.worker
