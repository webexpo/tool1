# Version 0.12 (July 2017)


  # Change Log *****************************************************************
  #
  #   When updated, look for comments with new_* and modif_*
  #   to rapidly identify new/modified code.
  #
  # Version 0.12
  # ------------
  #   Added argument save.RData
  #     -> useful at development stage only, for monitoring parameters values when dens.gen.icdf is crashing
  #
  # Version 0.10
  # ------------
  #   added arguments me.sd.range & cv.range
  #   Slightly modified how the outcome list 'out' is prepared
  #
  # Version 0.4 [replacesmodel.Kromhout.2levels]
  # -----------
  #   y1 & y2 were regrouped in : y
  #   level.index, level.index.lt, level.index.gt & level.index.interval were added [expected to be integer indices between 1 & number of levels]
  #



SEG.informedvar.MultiLevel <- function(y=numeric(0), lt=numeric(0), gt=numeric(0),
  interval.lower=numeric(0), interval.upper=numeric(0),
  level.index=rep(1, length(y)), level.index.lt=rep(1, length(lt)), level.index.gt=rep(1, length(gt)),
  level.index.interval=rep(1, length(interval.lower)),
  n.chains=1, n.iter=15000, n.burnin=500, n.thin=1, monitor.burnin=F,
  mu.lower=rep(-100, n.levels), mu.upper=rep(100, n.levels),
  log.sigma.mu=rep(-0.1744, n.levels), log.sigma.prec=rep(2.5523, n.levels),
  init.mu   =matrix(log(0.3), ncol=n.chains, nrow=n.levels),
  init.sigma=matrix(log(2.5), ncol=n.chains, nrow=n.levels),
  outcome.is.logNormally.distributed=T,
  me.sd.range=numeric(0), cv.range=numeric(0),
  save.RData=F, RData.dir='c:/users/jerome')
{
  if (length(interval.lower) != length(interval.upper)) stop("interval.lower and interval.upper must be of same length.")

  # Drop any data point (and its corresponding level index) that would be NA

  tmp <- is.na(y)
  if (any(tmp)) {y <- y[!tmp]; level.index <- level.index[!tmp]}
  tmp <- is.na(lt)
  if (any(tmp)) {lt <- lt[!tmp]; level.index.lt <- level.index.lt[!tmp]}
  tmp <- is.na(gt)
  if (any(tmp)) {gt <- gt[!tmp]; level.index.gt <- level.index.gt[!tmp]}
  tmp <- is.na(interval.lower) | is.na(interval.upper)
  if (any(tmp)) {interval.lower <- interval.lower[!tmp]; interval.upper <- interval.upper[!tmp]; level.index.interval <- level.index.interval[!tmp]}


  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol # function to test for presence of integer numbers in level.index.* (only integers are accepted)
  tmp <- c(level.index, level.index.lt, level.index.gt, level.index.interval)
  level.indices.are.all.integers <- all(is.wholenumber(tmp))
  if (!level.indices.are.all.integers) stop("Values in level.index and level.index.* must be integers.")

  level.index    <- round(level.index)
  level.index.lt <- round(level.index.lt)
  level.index.gt <- round(level.index.gt)
  level.index.interval <- round(level.index.interval)

  tmp <- c(level.index, level.index.lt, level.index.gt, level.index.interval)
  level.indices <- sort(unique(tmp))
  n.levels <- max(level.indices)

  if (any(level.indices != seq(n.levels)))
  {
    msg <- paste(c("Values in level.index must cover all integer values between 1 and number of levels (", n.levels, ")"), collapse="")
    stop(msg)
  }


  # Run Kromhout model for each level

  for (this.level in level.indices)
  {
    this.level.y  <-  y[level.index==this.level]
    this.level.lt <- lt[level.index.lt==this.level]
    this.level.gt <- gt[level.index.gt==this.level]
    this.level.interval.lower <- interval.lower[level.index.interval==this.level]
    this.level.interval.upper <- interval.upper[level.index.interval==this.level]

    tmp.out <- SEG.informedvar(y=this.level.y, lt=this.level.lt, gt=this.level.gt,
            interval.lower=this.level.interval.lower, interval.upper=this.level.interval.upper,
            n.chains=n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin, monitor.burnin=monitor.burnin,
            mu.lower=mu.lower[this.level], mu.upper=mu.upper[this.level], log.sigma.mu=log.sigma.mu[this.level], log.sigma.prec=log.sigma.prec[this.level],
            init.mu=init.mu[this.level,], init.sigma=init.sigma[this.level,],
            outcome.is.logNormally.distributed=outcome.is.logNormally.distributed,
            me.sd.range=me.sd.range, cv.range=cv.range,
            save.RData=save.RData, RData.dir=RData.dir)


    # Combine dimensions $sample (and $burnin, if any) of previous levels outputs and tmp.out

    if (this.level == 1)
    {
      out.sample <- tmp.out$sample
      if (monitor.burnin) out.burnin <- tmp.out$burnin
    }
    else
    {
      out.sample <- mapply(list, out.sample, tmp.out$sample, SIMPLIFY=F)
      if (monitor.burnin) out.burnin <- mapply(list, out.burnin, tmp.out$burnin, SIMPLIFY=F)
    }
  }

  # Prepare output object

  out <- list(sample=out.sample)
  if (monitor.burnin) out$burnin <- out.burnin


  out$mcmc <- list(n.chains=n.chains, n.burnin=n.burnin, n.iter=n.iter, n.thin=n.thin, monitor.burnin=monitor.burnin)
  out$inits <- list(mu=init.mu, sigma=init.sigma)
  out$data <- list(y=y, lt=lt, gt=gt, interval.lower=interval.lower, interval.upper=interval.upper,
                   level.index=level.index, level.index.lt=level.index.lt, level.index.gt=level.index.gt,
                   level.index.interval=level.index.interval,
                   n.levels=n.levels)
  out$parms <- list(mu.lower=mu.lower, mu.upper=mu.upper, log.sigma.mu=log.sigma.mu, log.sigma.prec=log.sigma.prec,
                    outcome.is.logNormally.distributed=outcome.is.logNormally.distributed)

  out$me <- tmp.out$me

  out
} # end of SEG.informedvar.MultiLevel
