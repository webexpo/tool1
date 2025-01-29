# Version 0.11 (February 2017)

  # Change Log *****************************************************************
  #
  #   When updated, look for comments with new_* and modif_*
  #   to rapidly identify new/modified code.
  #
  #
  # Version 0.11
  # ------------
  #  - changed argument take.log for logNormal.distrn
  #  - when logNormal.distrn=T, the output object now includes:
  #      log$y = log(y)
  #      log$uncensored.sum
  #      log$uncensored.sum2
  #
  # Version 0.10
  # ------------
  #   - data$n (the number of uncensored observations) was dropped from returned list
  #   - data$size was added to returned list
  #   - a block was added to check for irreconciliable negative data in presence of measurement error
  #       specified through a coefficient of variation
  #   - using new notation: gt and lt rather than l and r (left- and right- censoring values)


data.summary <- function(y=numeric(0), lt=numeric(0), gt=numeric(0),
  interval.lower=numeric(0), interval.upper=numeric(0),
  logNormal.distrn=T,
  worker=numeric(0), worker.lt=numeric(0), worker.gt=numeric(0), worker.interval=numeric(0), me.through.cv=F)
{
  if (length(interval.lower) != length(interval.upper)) stop("interval.lower and interval.upper must have same length.")

  data <- list()

  valid.y  <- !is.na(y)
  valid.lt <- !is.na(lt)
  valid.gt <- !is.na(gt)
  valid.interval <- !is.na(interval.lower) & !is.na(interval.upper)

  any.censored <- any(valid.lt) | any (valid.gt) | any(valid.interval) # modif_0.11
  any.censored <- list(any=any.censored)

  any.worker <- length(worker) > 0 | length(worker.lt) > 0 | length(worker.gt) > 0 | length(worker.interval) > 0   # modif_0.11


  if (any.worker)
  {
    # Keep worker patkeys where data is available

    worker.patkey <- numeric(0)

    if (any(valid.y))  {worker    <- worker[valid.y];     worker.patkey <- c(worker.patkey, worker)}
    if (any(valid.lt)) {worker.lt <- worker.lt[valid.lt]; worker.patkey <- c(worker.patkey, worker.lt)}
    if (any(valid.gt)) {worker.gt <- worker.gt[valid.gt]; worker.patkey <- c(worker.patkey, worker.gt)}
    if (any(valid.interval)) {worker.interval <- worker.interval[valid.interval]; worker.patkey <- c(worker.patkey, worker.interval)}

    k <- table(worker.patkey)
    worker.patkey <- names(k)
    n.workers <- length(worker.patkey)

    id <- list(y=match(worker, worker.patkey))
  }


  y  <-  y[valid.y]
  gt <- gt[valid.gt]
  lt <- lt[valid.lt]
  interval.lower <- interval.lower[valid.interval]
  interval.upper <- interval.upper[valid.interval]


  if (any.censored$any)
  {
    any.censored$gt <- any(valid.gt)
    any.censored$lt <- any(valid.lt)
    any.censored$i  <- any(valid.interval)

    if (any.censored$gt) data$cens$gt <- gt
    if (any.censored$lt) data$cens$lt <- lt
    if (any.censored$i)  data$cens$i <- list(gt=interval.lower, lt=interval.upper)

    if (logNormal.distrn)
    {
      if (any.censored$gt) data$log.cens$gt <- log(gt)
      if (any.censored$lt) data$log.cens$lt <- log(lt)
      if (any.censored$i)  data$log.cens$i <- list(gt=log(interval.lower), lt=log(interval.upper))
    }

    # For each censoring value, note to which worker each of them belongs

    if (any.worker)
    {
      if (any.censored$gt) id$gt <- match(worker.gt, worker.patkey)
      if (any.censored$lt) id$lt <- match(worker.lt, worker.patkey)
      if (any.censored$i)  id$i  <- match(worker.interval, worker.patkey)
    }
  }


  if (any(interval.lower >= interval.upper)) stop("Censoring-intervals lower bounds must be smaller than upper bounds.") # new_0.10


  # new_0.10
  # Check for irreconciliable data with model

  if (me.through.cv | logNormal.distrn)
  {
    problematic.arg <- character(0)
    if (any(             y < 0)) problematic.arg <- 'y'
    if (any(            lt < 0)) problematic.arg <- 'lt'
    if (any(            gt < 0)) problematic.arg <- 'gt'
    if (any(interval.lower < 0)) problematic.arg <- 'interval.lower'
    if (any(interval.upper < 0)) problematic.arg <- 'interval.upper'

    if (length(problematic.arg) > 0)
    {
      if (me.through.cv)
      {
        msg.header <- "Measurement Error specified through a Coefficient of Variation"
      }
      else
      {
        msg.header <- "A log-normally distributed outcome"
      }


      msg <- paste(msg.header, " and negative data points (as found in '", problematic.arg, "') are irreconciliable.", sep='')
      stop(msg)
    }
  }


  data$y <- y
  # data$n <- length(y) dropped [modif_0.10]


  if (any.worker)
  {
    data$id <- id
    data$n.workers <- n.workers
    data$worker <- list(patkey=worker.patkey, count=unname(k))
  }

  data$any.censored <- any.censored
  data$N <- length(y) + length(gt) + length(lt) + length(interval.lower) # total sample size, including number of censored values
  data$uncensored.sum  <- sum(y)
  data$uncensored.sum2 <- sum(y^2)

  if (logNormal.distrn)
  {
    log.y <- log(y)
    data$log <- list(y=log.y, uncensored.sum=sum(log.y), uncensored.sum2=sum(log.y^2))
  }


  data$size <- list(y=length(y), gt=length(gt), lt=length(lt), i=length(interval.lower))

  data
} # end of data.summary
