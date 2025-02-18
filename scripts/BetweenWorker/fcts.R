# Version 0.23 (Feb 2018)

  # Change Log *****************************************************************
  #
  #   When updated, look for comments with new_* and modif_*
  #   to rapidly identify new/modified code.
  #
  # Version 0.23
  # ------------
  #   Relaxed the precautions added in version 0.22
  #
  # Version 0.22
  # ------------
  #   Added some protections following changes in version 0.21
  #
  # Version 0.21
  # ------------
  #   Brought slight modifications to dens.gen.icdf
  #
  # Version 0.20
  # ------------
  #   Added a convergence criterion to secured.NR.search
  #
  # Version 0.19
  # ------------
  #   -> Modified log.f.inv.remote in truevalue.gen.object
  #   -> Modified dens.gen.icdf
  #   -> look for comments with _0.19 to viec each new/modified piece of code
  #
  # Version 0.18
  # ------------
  #   Added argument RData to each function calling dens.gen.icdf
  #     -> useful at development stage only, for monitoring parameters values when dens.gen.icdf is crashing
  #
  # Version 0.17
  # ------------
  #   Added yet another alternative starting point to truevalue.gen.object
  #
  # Version 0.16
  # ------------
  #   Added argument RData to function truevalues.gen and truevalue.gen
  #     -> useful at development stage only, for monitoring parameters values when dens.gen.icdf is crashing
  #
  # Version 0.15
  # ------------
  #   Added an alternative starting point to truevalue.gen.object
  #
  # Versions 0.12, 0.13 & 0.14
  # --------------------------
  #   Modified function Default.inits to deal with data sets where only one *y* point is not censored
  #
  # Version 0.11
  # ------------
  #   New function(s):
  #     - Default.inits
  #     - out.logout.moments
  #     - polynomial.product.coeff
  #     - take.log
  #
  #   All the functions log.f.inv.remote.left & log.f.inv.remote.right were
  #     combined into log.f.inv.remote
  #     - the new function split.starting.values.leftRight was written to split the 'remote'
  #       solutions into left- or right- side (relatively to mode) starting points
  #
  #   The following functions were modified:
  #     - logPhi.quadratic.approx.coeff (the argument 'degree' was added)
  #     - me.gen
  #     - me.gen.object
  #         > the condition if (me$through.cv & !logNormal.distrn)
  #           was changed to if (me$through.cv)
  #           and the "else" block was split in two
  #     - real.cubic.roots -> dropped argument accept.when.abs.soln.lt, added arguments l & u
  #     - rnorm.censored (argument take.log was dropped)
  #     - secured.NR.search (more changes than what is indicated through new_* and modif_*; sorry...)
  #     - smoothed.area.estimate
  #     - truevalue.gen
  #     - truevalue.gen.object
  #     - truevalues.gen
  #     - y.gen
  #
  #   A$mesd and A$mesd2 were changed for A$ksi and A$ksi2, respectively
  #
  #   The function(s)
  #     datalist.sum
  #     was (were) dropped
  #
  #   We added remote.right argument when calling smoothed.area.estimate on a few occasions
  #   In every function, math.limits (vector of length 2) was changed to math.lower.limit (scalar)
  #
  #
  # Version 0.10
  # ------------
  #   The following functions were added:
  #     - any.me
  #     - cond.values
  #     - datalist.sum (dropped in version 0.11)
  #     - dens.gen.icdf
  #     - empty.matrices
  #     - logPhi.quadratic.approx
  #     - logPhi.quadratic.approx.coeff
  #     - lphi
  #     - me.gen
  #     - me.gen.object
  #     - mu.truncatedData.gen
  #     - mu.truncatedData.gen.object
  #     - out.sample
  #     - quadratic.solution
  #     - real.cubic.roots
  #     - renamed.me.parm
  #     - sigma.gen.object
  #     - sigma.truncatedData.gen
  #     - sigma.truncatedData.gen.object
  #     - sqrt.invertedGamma.gen
  #     - truevalue.gen
  #     - truevalue.gen.object
  #     - truevalues.gen
  #     - y.gen (largely inspired from code present in model-Uninformative.R version 0.8)
  #     - y.gen.inits
  #
  #   - functions logp.from.logpcum and pnorm.logpcum were moved from sigmaGen.R to here
  #
  #   - rgamma.truncated was modified
  #   - rnorm.censored   was modified
  #   - runif.logp       was modified


any.me <- function(sd.minmax, cv.minmax)
{
  me <- list(any=F, through.sd=F, through.cv=F, known=F)

  if (length(sd.minmax) > 0)
  {
    if (length(sd.minmax) != 2) stop("me.sd.range must be of length 2.")
    me$any <- T
    me$through.sd <- T # indicates that measurement error was specified through a constant sd
    me$range <- sort(sd.minmax)
  }

  if (length(cv.minmax) > 0)
  {
    if (me$any) stop("Only me.sd.range or cv.range must be specified [if any measurement error is present].")
    if (length(cv.minmax) != 2) stop("cv.range must be of length 2.")
    me$any <- T
    me$through.cv <- T
    me$through.sd <- F
    me$range <- sort(cv.minmax)
  }

  if (me$any)
  {
    me$init <- mean(me$range)
    me$known <- diff(me$range) == 0
  }

  me
} # end if any.me


cond.values <- function(cond, T.values, F.values)
{
  if (cond)
  {
    T.values
  }
  else
  {
    F.values
  }
} # end of cond.values



Default.inits <- function(data, logNormal.distrn, mu.range, sigma.range, include.censored.data=F)
{
  # modif_0.13
  # added argument include.censored.data to this fct

  sigma.lower <- min(sigma.range)
  sigma.upper <- max(sigma.range)

  # new_0.12
  normalized.y <- numeric(0)

  if (data$size$y > 0)
  {
    if (logNormal.distrn) normalized.y <- data$log$y
    else normalized.y <- data$y
  }


  # new_0.12

  if (include.censored.data)
  {
    if (data$size$gt > 0)
    {
      if (logNormal.distrn) normalized.y <- c(normalized.y, data$log$gt)
      else normalized.y <- c(normalized.y, data$gt)
    }

    if (data$size$lt > 0)
    {
      if (logNormal.distrn) normalized.y <- c(normalized.y, data$log$lt)
      else normalized.y <- c(normalized.y, data$lt)
    }

    if (data$size$i > 0)
    {
      if (logNormal.distrn) normalized.y <- c(normalized.y, data$log$i)
      else normalized.y <- c(normalized.y, data$i)
    }
  }


  if (length(normalized.y) > 0)
  {
    mu <- mean(normalized.y)

    mu.lower <- min(mu.range)
    if (mu < mu.lower) mu <- mu.lower
    else
    {
      mu.upper <- max(mu.range)
      if (mu > mu.upper) mu <- mu.upper
    }


    # modif_0.13

    if (length(normalized.y) > 1)
    {
      sigma <- sqrt(var(normalized.y))
    }
    else if (!include.censored.data)
    {
      # new_0.12
      # Call this fct again, this time using censored data
      tmp <- Default.inits(data, logNormal.distrn, mu.range, sigma.range, include.censored.data=T)
      sigma <- tmp$sigma
    }
    else
    {
      # new_0.13
      sigma <- 0 # will be corrected below
    }

    if (sigma < sigma.lower) sigma <- sigma.lower
    if (sigma == 0) sigma <- sigma.upper/10 # modif_0.14
  }
  else
  {
    mu <- mean(mu.range)
    sigma <- ifelse(sigma.lower > 0, sigma.lower, sigma.upper/10)
  }

  list(mu=mu, sigma=sigma)
} # end of Default.inits


dens.gen.icdf <- function(o, A=o$A, range=numeric(0), u=runif(1), start=mean(range), precision=1e-6, inestimable.lower.limit=F, NR.max.iter=200, save.objects=character(0), unlink=T)
{
  # modif_0.11 changed NR.max.iter to 200
  # ICI remettre save.objects=temp(my.save.objects, time.stamp=T) pour simulation

  # o is a list of:
  #   f, log.f, log.f.prime, log.f.second: functions
  #   log.f.inv.remote:                    function
  #   start:                               function
  #   logNormal.distrn:                    logical
  #   A:                                   the list of functions arguments


  if (length(save.objects) > 0)
  {
    # save.objects was useful while debugging the development version of this software:
    # it may not be necessary to have it in other languages' versions.
    # It was left here in case we need it again.

    save('o', 'A', 'range', 'u', 'precision', 'inestimable.lower.limit', 'start', file=save.objects)
  }


  ping.pong.tie.breaker <- function(area, A, start, area.x, target,
                                    math.lower.limit, ref.range, inestimable.lower.limit, epsilon,
                                    distrn.leftSide=F, hp.mult=ifelse(distrn.leftSide, -1, 1),
                                    max.count=100)
  {
    continue <- T

    count <- 0
    x <- start
    visited.x <- x

    while (continue)
    {
      hp <- A$f(x, A) * hp.mult
      change <- (target - area.x)/hp
      x <- x + change

      # new_0.11
      if (x < ref.range[1])
      {
        x <- ifelse(ref.range[1] == math.lower.limit & inestimable.lower.limit, (x - change + math.lower.limit) / 2, ref.range[1])
      }


      area.x <- smoothed.area.estimate(area, x, A, math.lower.limit, ref.range, inestimable.lower.limit, hp.mult=hp.mult)

      count <- count + 1
      converged <- abs(area.x - target) < epsilon
      visited.x <- c(visited.x, x)
      caught.in.loop <- any(duplicated(visited.x) & is.finite(visited.x))
      continue <- !converged & count <= max.count & !caught.in.loop
    }


    if (!converged & caught.in.loop)
    {
      # We have found the series of points that are repeatedly visited:
      # recalibrate and try Newton-Raphson again

      w <- which(visited.x == x)
      w <- min(w) # repeated value (loop) start on visited.x[w]
      loop.x <- visited.x[seq(from=w+1, to=count+1)]

      if (distrn.leftSide) x <- max(loop.x)
      else x <- min(loop.x)

      area.x <- smoothed.area.estimate(area, x, A, math.lower.limit, ref.range, inestimable.lower.limit, hp.mult=hp.mult)

      if (distrn.leftSide) A$upper.limit <- x
      else A$lower.limit <- x

      target <- target - area.x
      area.x <- 0

      count <- 0
      continue <- T

      while (continue)
      {
        hp <- A$f(x, A) * hp.mult
        change <- (target - area.x)/hp
        x <- x + change
        area.x <- area(x, A)
        count <- count + 1
        converged <- abs(area.x - target) < epsilon
        continue <- !converged & count <= max.count
      }
    }


    if (!converged) stop("Newton-Raphson algorithm did not converge. Sorry.\n")

    x
  } # end of ping.pong.tie.breaker


  ref.points <- function(o, A, start, range, inestimable.lower.limit=F, f.ratio.remote=1e-8, epsilon=1e-6, max.niter=NR.max.iter)
  {
    # Arguments:
    # ----------------------------------------------
    # o: same nature as o in dens.gen.icdf arguments
    # start: EITHER a vector of length 2 [two potential starting points]
    #        or a scalar
    # f.ratio.remote: scalar
    # epsilon: scalar
    # range: vector of length 2


    # new_0.11
    higher.local.max <- function(o, A, dip.x, range, epsilon)
    {
      # Find a starting point on each side of dip.x
      start <- o$start(A)
      tmp <- max(abs(start-dip.x))

      start.left <- min(start)
      if (start.left > dip.x) start.left <- dip.x - tmp

      start.right <- max(start)
      if (start.right < dip.x) start.right <- dip.x + tmp

      # Look for starting points with negative values for h''
      #i) on left side
      continue <- o$log.f.second(start.left, A) > 0
      while (continue)
      {
        start.left <- start.left - tmp
        if (start.left < range[1]) start.left <- (range[1] + start.left + tmp) / 2
        continue <- o$log.f.second(start.left, A) > 0
      }

      #ii) on right side
      continue <- o$log.f.second(start.right, A) > 0
      while (continue)
      {
        start.right <- start.right + tmp
        continue <- o$log.f.second(start.right, A) > 0
      }

      # Run pure Newton-Raphson to find local max on each side

      # i) left side
      x <- start.left
      continue <- T
      hp <- o$log.f.prime(x, A)
      while (continue)
      {
        change <- hp/o$log.f.second(x, A)
        x <- x - change
        hp <- o$log.f.prime(x, A)
        continue <- abs(hp) > epsilon
      }
      local.mode.left <- x

      # ii) right-side
      x <- start.right
      continue <- T
      hp <- o$log.f.prime(x, A)
      while (continue)
      {
        change <- hp/o$log.f.second(x, A)
        x <- x - change
        hp <- o$log.f.prime(x, A)
        continue <- abs(hp) > epsilon
      }
      local.mode.right <- x

      h.left  <- o$log.f(local.mode.left, A)
      h.right <- o$log.f(local.mode.right, A)

      x <- ifelse(h.left > h.right, local.mode.left, local.mode.right)
      x
    } # end of higher.local.max


    # new_0.11
    max.fastTrack <- function(o, A, lower.limit, rightside.x, rightside.hp, epsilon=1e-4, epsilon.x=1e-20)
    {
      x <- rightside.x
      h <- o$log.f(x, A)
      hp <- rightside.hp

      b <- list(x=c(NA, x), h=c(NA, h), hp=c(NA, hp))

      x <- (x + lower.limit) / 2
      continue <- T


      # Find a point with positive slope (that is, to the left of the mode)
      while (continue)
      {
        hp <- o$log.f.prime(x, A)

        if (hp > 0)
        {
          continue <- is.infinite(hp)

          if (continue)
          {
            next.x <- (3*x - lower.limit) / 2
            lower.limit <- x

            continue <- abs(next.x - x) > epsilon.x
            if (continue) x <- next.x
          }
        }
        else
        {
          x <- (x + lower.limit) / 2
        }
      }


      hp <- o$log.f.prime(x, A)
      converged <- is.finite(hp) & hp > 0
      continue <- converged


      while (continue)
      {
        side <- ifelse(hp > 0, 1, 2)
        b$x[side] <- x
        b$h[side] <- o$log.f(x, A)
        b$hp[side] <- hp

        b.diff <- diff(b$hp)

        if (b.diff == 0) x <- mean(b$x)
        else
        {
          x <- (diff(b$hp*b$x) - diff(b$h)) / b.diff
          if (x <= b$x[1] | x >= b$x[2]) x <- mean(b$x)
        }

        hp <- o$log.f.prime(x, A)
        continue <- abs(hp) > epsilon
      }

      h <- o$log.f(x, A)

      list(converged=converged, x=x, h=h)
    } # end of max.fastTrack


    # new_0.11
    # modif_0.19 (now returns a list)
    split.starting.values.leftRight <- function(o, target, A, range, start.values)
    {
      # new_0.19
      point.with.slope.closest.to.secant <- function(start, o, A, target, range, right.side=T)
      {
        # modif_0.21 Line below was replaced with the next two
        # y <- o$log.f(start, A)
        y <- rep(NA, length(start))
        for (i in seq(along.width=start)) y[i] <- o$log.f(start[i], A)

        sup <- start[y>target]
        inf <- start[y<target]

        if (right.side)
        {
          if (length(sup) > 0)
          {
            sup <- max(sup)
            range[1] <- sup
          }


          if (length(inf) > 0)
          {
            inf <- min(inf)
            range[2] <- inf
          }
        }
        else
        {
          if (length(sup) > 0)
          {
            sup <- min(sup)
            range[2] <- sup
          }

          if (length(inf) > 0)
          {
            inf <- max(inf)
            range[1] <- inf
          }
        }


        x <- c(inf, sup) # one or two values

        if (length(x) > 1)
        {
          # pick the one with the slope closest to straight line cutting area in two equal parts

          # modif_0.21 Line below was replaced by the next two
          #slope <- o$log.f.prime(x, A)
          slope <- rep(NA, length(x))
          for (i in seq(along.width=x)) slope[i] <- o$log.f.prime(x[i], A)

          angle <- atan(slope)

          # modif_0.21 Line below was replaced by the next two
          #y <- o$log.f(x, A)
          y <- rep(NA, length(x))
          for (i in seq(along.width=x)) y[i] <- o$log.f(x[i], A)

          secant.slope <- diff(y)/diff(x)
          secant.angle <- atan(secant.slope)

          angle.diff <- abs(angle-secant.angle)
          w <- which.min(angle.diff)[1]
          x <- x[w]
        }


        list(x=x, range=range)
      } # end of point.with.slope.closest.to.secant

      #start <- rep(NA, 2)  modif_0.19

      start.values <- start.values[!is.na(start.values)] # new_0.20

      ls.start <- start.values[start.values <= A$mode & start.values >= range[1]] # modif_0.12
      rs.start <- start.values[start.values >= A$mode & start.values <= range[2]] # modif_0.12

      # new_0.19
      start <- list(left=NA, right=NA)
      range <- list(left=c(range[1], A$mode), right=c(A$mode, range[2]))

      # new_0.19
      tmp <- point.with.slope.closest.to.secant(ls.start, o, A, target, range$left, right.side=F)
      start$left <- tmp$x
      range$left <- tmp$range

      # new_0.19
      tmp <- point.with.slope.closest.to.secant(rs.start, o, A, target, range$right)
      start$right <- tmp$x
      range$right <- tmp$range

      # modif_0.19
      # lots of code was deleted here

      # new_0.19
      if (length(start$left) == 0)  start$left  <- NA
      if (length(start$right) == 0) start$right <- NA

      list(start=start, range=range) # modif_0.19
    } # end of split.starting.values.leftRight


    # --------------------------------------------------------------------------
    # If range is finite and log.f not inestimable at lower end,
    # then we first look for values at both ends before searching for a mode

    mode <- list(x=numeric(0), h=numeric(0), found=F) # modif_0.11

    if (all(is.finite(range)) & !inestimable.lower.limit)
    {
      log.f.prime <- rep(NA, 2)
      for (i in 1:2) log.f.prime[i] <- o$log.f.prime(range[i], A)

      # If slope (f') is the same at both ends, then there is no mode between the 2 endpoints (assuming a unimodal distrn)
      # and we then assume that the highest end value for log.f is hence the local mode

      log.f.prime.sign <- sign(log.f.prime)

      if (prod(log.f.prime.sign) > 0)
      {
        log.f <- rep(NA, 2)
        for (i in 1:2) log.f[i] <- o$log.f(range[i], A)
        w <- which.max(log.f)
        mode <- list(x=range[w], h=log.f[w], found=T)
      }
    }


    if (!mode$found)
    {
      # Choose a starting point among suggested starting points, if more than one point was suggested

      if (length(start) > 1)
      {
        start <- c(start, mean(start)) # Add the middle point to the two potential starting points
        start.len <- length(start)
        log.f <- rep(NA, start.len)
        for (i in 1:start.len) log.f[i] <- o$log.f(start[i], A=A)
        w <- which.max(log.f)
        start <- start[w]
      }


      # Find mode by Newton-Raphson

      o.mode <- list(h=o$log.f.prime, hp=o$log.f.second, range=range)
      nr <- secured.NR.search(o.mode, A, start, epsilon, range=range, max.niter=max.niter)

      if (nr$converged)
      {
        hs <- o$log.f.second(nr$x, A)
        if (hs > 0)
        {
          # we have found a mimimum point (between two local modes):
          # we redo the search on both sides of this dip
          mode$x <- higher.local.max(o, A, nr$x, range, epsilon)
        }
        else
        {
          mode$x <- nr$x
        }

        mode$found <- T
      }
      else
      {
        x <- nr$bounds$x
        h <- nr$bounds$h
        h.on.both.sides <- prod(sign(h)) < 0
        diff.x <- abs(diff(x))
        if (diff.x < epsilon & h.on.both.sides)
        {
          mode$x <- mean(x)
          mode$found <- T
        }
        else
        {
          converged <- F

          if (diff(nr$bounds$x) < epsilon & all(nr$bounds$h < 0))
          {
            if (is.finite(range[1]))
            {
              # We try a little bit further to find the actual mode,
              # since it looks like we are almost there...

              converged <- T
              tmp <- max.fastTrack(o, A, range[1], nr$bounds$x[1], nr$bounds$h[1]) # yes: 4th argument is bounds$h (and not hp!)

              if (tmp$converged)
              {
                mode$x <- tmp$x
                mode$found <- T
              }
              else
              {
                mode$x <- range[1]
                mode$found <- !inestimable.lower.limit
              }
            }
            else
            {
              x <- nr$bounds$x[1]
              h <- nr$bounds$h[1]
              tmp <- abs(h/o$log.f.second(x, A))
              if (tmp < 1e-12)
              {
                mode <- list(x=x, h=h, found=T)
                converged <- T
              }
            }
          }

          if (!converged) stop("Algorithm did not converge.\n", save.objects, '\n')
        }
      }


      if (mode$found) mode$h <- o$log.f(mode$x, A=A)
    }


    log.f.ratio.remote <- log(f.ratio.remote) # new_0.11
    if (mode$found) target <- mode$h + log.f.ratio.remote
    A$mode <- mode$x # some 'remote.*' functions need that information


    # modif_0.11
    # Find remote points on both sides of mode

    if (mode$found)
    {
      x <- rep(NA, 2)
      f <- rep(NA, 2)
      fp <- rep(NA, 2)

      fs <- o$log.f.second(mode$x, A=A) # scalar
      d <- 10/abs(fs)


      # new_0.11
      remote.start <- o$log.f.inv.remote(target, A=A) # vector
      # modif_0.19 commented out line below and replaced it by 2nd line below
      # start <- split.starting.values.leftRight(o, target, A, range, remote.start) # vector of length 2
      search <- split.starting.values.leftRight(o, target, A, range, remote.start) # a list with dimensions $start and $range (new_0.19)
      start <- c(search$start$left, search$start$right) # new_0.19

      mode.on.border <- range == mode$x # new_0.11
      j.seq <- which(!mode.on.border & is.na(start)) # modif_0.11


      for (j in j.seq)
      {
        # modif_0.11 (first block that appeared here was moved above)

        if (is.na(start[j]))
        {
          dir <- ifelse(j == 1, -1, 1)

          tmp <- mode$x + dir*d

          accepted <- tmp > range[1] & tmp < range[2]
          if (!accepted)
          {
            my.d <- d
            while (!accepted)
            {
              my.d <- my.d/2
              tmp <- mode$x + dir*my.d
              accepted <- tmp > range[1] & tmp < range[2]
            }
          }


          fp[1] <- o$log.f.prime(tmp, A=A)
          accepted <- is.finite(fp[1])

          while (!accepted)
          {
            tmp <- (tmp + mode$x) / 2
            fp[1] <- o$log.f.prime(tmp, A=A)
            accepted <- is.finite(fp[1])
          }

          a <- fp[1]/d # estimated rate of slope acceleration
          d2 <- sqrt(2*abs(log(f.ratio.remote)/a)) # estimated distance we need to get from mode,
                                       # at the rate above, to reach the f.ratio.remote distance

          x2 <- mode$x + dir*d2
          accepted <- x2 > range[1] & x2 < range[2]

          while (!accepted)
          {
            d2 <- d2/2
            x2 <- mode$x + dir*d2
            accepted <- x2 > range[1] & x2 < range[2]
          }

          fp2 <- o$log.f.prime(x2, A=A)
          accepted <- is.finite(fp2)

          while (!accepted)
          {
            d2 <- d2/2
            x2 <- mode$x + dir*d2
            fp2 <- o$log.f.prime(x2, A=A)
            accepted <- is.finite(fp2)
          }

          x <- c(tmp, x2)
          f <- rep(NA, 2)
          for (i in 1:2) f[i] <- o$log.f(x[i], A=A)
          fp[2] <- fp2

          # Approximating the shape of log.f.prime by a straight line,
          # we can estimate the point where the distance
          # prescribed by f.ratio.remote is reached

          X <- matrix(c(x, 1, 1), ncol=2) # 2 x 2 matrix
          theta <- as.vector(solve(X) %*% fp)

          qA <- theta[1]
          qB <- theta[2]

          inner.side <- ifelse(xor(j==1, x[1]>x[2]), 2, 1)
          inner.x <- x[inner.side]

          qC <- -qA*inner.x^2 - qB*inner.x - (target - f[inner.side])

          if (j == 1)
          {
            l <- range[1]
            u <- mode$x
          }
          else
          {
            l <- mode$x
            u <- range[2]
          }


          roots <- quadratic.solution(c(qC, qB, qA), l=l, u=u) # modif_0.11

          if (length(roots) == 1)
          {
            if (!is.na(roots))
            {
              start[j] <- roots
            }
            else
            {
              start[j] <- x[3 - inner.side]
            }
          }
          else if (j == 1)
          {
            start[j] <- max(roots)
          }
          else
          {
            start[j] <- min(roots)
          }
        }
      }


      # new_0.11
      # Suggest another starting point (on both sides)
      # based on the 2nd degree polynomial approximation (Taylor series devpmt) of log.f

      if (!any(mode.on.border))
      {
        delta <- sqrt(2*log.f.ratio.remote/fs)

        # Left side

        x <- mode$x - delta

        if (x > range[1])
        {
          alt.start <- c(start[1], x)
          f <- rep(NA, 2)
          for (i in 1:2) f[i] <- abs(o$log.f(alt.start[i], A) - target)
          d <- abs(f - target)
          w <- which.min(d)[1]
          start[1] <- alt.start[w]
        }

        # Right side

        x <- mode$x + delta

        if (x < range[2])
        {
          alt.start <- c(start[2], x)
          f <- rep(NA, 2)
          for (i in 1:2) f[i] <- abs(o$log.f(alt.start[i], A) - target)
          d <- abs(f - target)
          w <- which.min(d)[1]
          start[2] <- alt.start[w]
        }
      }


      oh <- list(h=o$log.f, hp=o$log.f.prime, hs=o$log.f.second) # modif_0.11

      if (is.na(start[1]))
      {
        remote.left <- mode$x
      }
      else
      {
        nr <- secured.NR.search(oh, A, start[1], epsilon, target=target,
                 range=search$range$left, max.point=mode,
                 inestimable.lower.limit=inestimable.lower.limit, max.niter=max.niter)
                 # modif_0.19 modified value for 'range' argument above

        if (nr$converged)
        {
          remote.left <- nr$x
        }
        else if (is.finite(range[1]))
        {
          remote.left <- range[1]
        }
        else
        {
          stop('fin tempo ICI\n')
        }
      }


      if (is.na(start[2]))
      {
        remote.right <- mode$x
      }
      else
      {
        nr <- secured.NR.search(oh, A, start[2], epsilon, target=target,
                 range=search$range$right, max.point=mode, max.niter=max.niter)
                 # modif_0.19 modified value for 'range' argument above

        if (nr$converged)
        {
          remote.right <- nr$x
        }
        else
        {
          stop("Algorithm did not converge.\n", save.objects, '\n') # should not happen
        }
      }
    }
    else
    {
      # new_0.11
      # mode was not found
      remote.left <- NA
      remote.right <- NA
    }


    # new_0.21 added log.f.ratio.remote to the returned list
    list(mode=mode, remote=c(remote.left, remote.right), log.f.ratio.remote=log.f.ratio.remote)
  } # end of ref.points


  secured.NR.search <- function(o, A, start, epsilon, target=0, range=numeric(0),
                                max.point=list(x=numeric(0), h=numeric(0)),
                                inestimable.lower.limit=F, max.niter=200, expected.hpsign=-1, cat.x=F)
  {
    # Throughout this function, 'bounds' is a list with the following elements/dimensions:
    # x, h, hp, range:   numeric (vectors of length 2)
    # higher.side:       numeric (1)
    # include.soln:      logical (1)


    cubic.extrapolation <- function(target, bounds, monotonic, range)
    {
      # target: scalar
      # bounds: list

      higher.side <- bounds$higher.side

      # Approximate h by a cubic curve
      # (with appropriate values and slopes at both ends)

      X <- matrix(c(1, 1, 0, 0,
                    bounds$x, 1, 1,
                    bounds$x^2, 2*bounds$x,
                    bounds$x^3, 3*bounds$x^2), nrow=4)

      y <- c(bounds$h, bounds$hp)

      theta <- Xinverse.y(X, y)


      if (any(is.nan(theta)) | any(is.infinite(theta)))
      {
        use.linear.extrapolation <- T
      }
      else
      {
        tmp <- real.cubic.roots(theta, target)
        use.linear.extrapolation <- length(tmp) == 0 # modif_0.11
      }


      if (use.linear.extrapolation)
      {
        # use a linear extrapolation instead since X is probably not invertible
        X <- matrix(c(bounds$x, 1, 1), nrow=2) # 2 x 2 matrix
        theta <- Xinverse.y(X, bounds$h)
        tmp <- (target - theta[2]) / theta[1]
      }


      inbound.solns <- tmp[tmp > bounds$x[1] & tmp <= bounds$x[2]]
      inrange.solns <- tmp[tmp > range[1] & tmp <= range[2]]


      if (bounds$include.soln)
      {
        m <- length(inbound.solns)

        if (m == 0)
        {
          # Use a linear extrapolation instead
          X <- matrix(c(bounds$x, 1, 1), nrow=2) # 2 x 2 matrix
          theta <- Xinverse.y(X, bounds$h)
          soln <- (target - theta[2]) / theta[1]
        }
        else if (m == 1)
        {
          soln <- inbound.solns
        }
        else
        {
          soln <- mean(inbound.solns)
        }
      }
      else
      {
        tmp <- inrange.solns
        m <- length(tmp)

        if (m == 0)
        {
          soln <- NA
        }
        else if (m == 1)
        {
          soln <- tmp
        }
        else if (monotonic)
        {
          if (higher.side == 1)
          {
            tmp <- tmp[tmp < bounds$x[1]]
            if (length(tmp) > 0)
            {
              soln <- max(tmp)
            }
            else
            {
              soln <- NA
            }
          }
          else
          {
            tmp <- tmp[tmp > bounds$x[2]]
            if (length(tmp) > 0)
            {
              soln <- min(tmp)
            }
            else
            {
              soln <- NA
            }
          }
        }
        else
        {
          # keep the closest solution to any bound (or center of bounds, equivalently)
          d <- abs(tmp - mean(bounds$x))
          w <- which.min(d)
          soln <- tmp[w]
        }
      }

      soln
    } # end of cubic.extrapolation


    # new_0.11
    leftSide.fastScan <- function(bounds, o, A, target, lowerside.limit, h2modeless, ls.start=list(x=numeric(0), h=numeric(0), hp=numeric(0)), epsilon=1e-8, lower.side=1)
    {
      # in input bounds, the slopes (hp values) must be negative & positive, respectively
      # (thus pointing towards a 'dip', or local minimum)

      dip.characteristics <- function(b, o, A, epsilon=1e-8)
      {
        continue <- T

        while (continue)
        {
          # intersection between the tangents at both ends of bounds (b)

          b.diff <- diff(b$hp)

          if (b.diff == 0)
          {
            x <- mean(b$x)
          }
          else
          {
            x <- (diff(b$hp*b$x) - diff(b$h)) / b.diff
            if (x <= b$x[1] | x >= b$x[2]) x <- mean(b$x)
          }

          h <- o$h(x, A)
          hp <- o$hp(x, A)

          side <- ifelse(hp < 0, 1, 2)
          b$x[side] <- x
          b$h[side] <- h
          b$hp[side] <- hp
          continue <- diff(b$x) > epsilon
        }

        list(x=x, h=h, hp=hp)
      } # end of dip.characteristics


      leftbump.characteristics <- function(o, start, hp.start, target, A, lowerside.limit, h2modeless, ls.start=list(x=numeric(0), h=numeric(0), hp=numeric(0)), epsilon=1e-8)
      {
        out <- list(x=numeric(0), h.max=numeric(0))

        # Find maximum point

        lim <- c(lowerside.limit, start)
        if (length(ls.start$x) == 1) lim[1] <- ls.start$x

        x <- start
        hp <- hp.start
        continue <- T

        while (continue)
        {
          if (hp < 0) lim[2] <- x
          else lim[1] <- x

          hs <- -abs(o$hs(x, A))
          change <- hp/hs
          x <- x - change

          if (x > lim[2])      x <- (x + change + lim[2]) / 2
          else if (x < lim[1]) x <- (x + change + lim[1]) / 2

          hp <- o$hp(x, A)
          converged <- abs(hp) < epsilon
          continue <- !converged

          if (continue)
          {
            h <- o$h(x, A)
            continue <- h < h2modeless
          }
        }


        if (converged)
        {
          out$h.max <- o$h(x, A)
          continue <- out$h.max > target
        }


        if (continue)
        {
          # Find bump-left-side solution

          lim <- c(lowerside.limit, x)

          if (length(ls.start$x) == 1)
          {
            x <- ls.start$x
            h <- ls.start$h
          }
          else
          {
            hs <- o$hs(x, A)
            change <- (hp-1)/hs
            x <- x - change
            if (x < lim[1]) x <- (x + change + lim[1]) / 2

            h <- o$h(x, A)
          }

          while (continue)
          {
            hp <- o$hp(x, A)
            change <- (h-target)/hp
            x <- x - change

            if (x > lim[2])      x <- (x + change + lim[2]) / 2
            else if (x < lim[1]) x <- (x + change + lim[1]) / 2

            h <- o$h(x, A)
            continue <- abs(h-target) > epsilon
          }

          out$x <- x
          out$h <- h
        }

        out$converged <- converged
        out
      } # end of leftbump.characteristics


      higher.side <- 3 - lower.side
      dip <- dip.characteristics(bounds, o, A, epsilon=epsilon)
      leftbump <- leftbump.characteristics(o, bounds$x[lower.side], bounds$hp[lower.side], target, A, lowerside.limit, h2modeless, ls.start=ls.start)


      if (leftbump$converged)
      {
        found.soln <- leftbump$h.max > target
        continue <- !found.soln

        if (found.soln)
        {
          x <- leftbump$x
          h <- leftbump$h
          hp <- NA

        }
        else
        {
          x <- dip$x
          h <- dip$h
          hp <- (bounds$x[higher.side] - h) / (bounds$x[higher.side] - x)
        }
      }
      else
      {
        x <- lowerside.limit
        h <- target
        hp <- NA
        continue <- F
      }


      list(x=x, h=h, hp=hp, continue=continue)
    } # end of leftSide.fastScan


    new.bound <- function(start, bounds, monotonic, o, A, target, range, above=wb$above)
    {
      x <- start
      h <- o$h(x, A)
      hp <- o$hp(x, A)
      accepted <- is.finite(h) & is.finite(hp)


      if (!accepted)
      {
        # Try a cubic extrapolation if we have two points in bounds

        n.bounds <- sum(!is.na(bounds$x))

        if (n.bounds == 2)
        {
          tmp <- cubic.extrapolation(target, bounds, monotonic, range)

          if (!is.na(tmp))
          {
            x <- tmp
            h <- o$h(x, A)
            hp <- o$hp(x, A)
            accepted <- is.finite(h) & is.finite(hp)
          }
        }
      }


      if (!accepted)
      {
        if (above)
        {
          bound <- max(bounds$x, na.rm=T)
        }
        else
        {
          bound <- min(bounds$x, na.rm=T)
        }
      }


      while (!accepted)
      {
        x <- (x + bound) / 2
        h <- o$h(x, A)
        hp <- o$hp(x, A)
        accepted <- is.finite(h) & is.finite(hp)
      }

      list(x=x, h=h, hp=hp)
    } # end of new.bound


    # new_0.19
    unexplored.range <- function(visited.x, epsilon)
    {
      L <- length(visited.x)
      j.odd  <- seq(from=1, to=L, by=2)
      j.even <- seq(from=2, to=L, by=2)

      x.odd  <- visited.x[j.odd]
      x.even <- visited.x[j.even]

      outer.range <- max(visited.x) - min(visited.x)

      mean.x.odd  <- mean(x.odd)
      mean.x.even <- mean(x.even)

      if (mean.x.odd < mean.x.even)
      {
        inner.lim <- c(max(x.odd), min(x.even))
      }
      else
      {
        inner.lim <- c(max(x.even), min(x.odd))
      }

      inner.range <- diff(inner.lim)

      continue <- inner.range > epsilon & inner.range/outer.range > 0.5
      x <- ifelse(continue, mean(inner.lim), NA)

      list(continue=continue, x=x)
    } # end of unexplored.range


    unreached.target <- function(target, h, higher.side, monotonic, mode.search, left.side=F)
    {
      if (!monotonic | mode.search)
      {
        unreached <- F
      }
      else
      {
        pos.slope <- higher.side == 2
        unreached <- xor(pos.slope, h > target)
        if (left.side) unreached <- !unreached
      }

      unreached
    } # end of unreached.target


    within.bounds <- function(x, bounds)
    {
      nbounds <- sum(!is.na(bounds$x))

      if (nbounds == 2)
      {
        above <- x > bounds$x[2]
        below <- x < bounds$x[1]
      }
      else
      {
        w <- which(!is.na(bounds$x))
        bound <- bounds$x[w]
        above <- x > bound
        below <- !above
      }

      within <- !above & !below
      out <- list(within=within, below=below, above=above)

      out # list with 3 dimensions: within, above & below
    } # end of within.bounds


    # --- Fct starts -----------------------------------------------------------

    mode.search <- length(max.point$x) == 0
    monotonic <- !mode.search

    limit <- list(x=range, checked=rep(F,2), usable=rep(F,2), h=rep(NA,2), hp=rep(NA,2))
    limit$checked[1] <- inestimable.lower.limit # new_0.11

    bounds <- list(x=rep(NA, 2), h=rep(NA, 2), hp=rep(NA, 2), include.soln=F)


    if (length(max.point$x) > 0)
    {
      higher.side <- ifelse(start < max.point$x, 2, 1)

      bounds$x[higher.side]  <- max.point$x
      bounds$h[higher.side]  <- max.point$h
      bounds$hp[higher.side] <- 0
      found2bounds <- T

      expected.hpsign <- 2*higher.side - 3
      correct4hp.sign <- start > max.point$x

      h2modeless <- 2*max.point$h - target
    }
    else
    {
      found2bounds <- F
      higher.side <- 1
      correct4hp.sign <- T
    }


    lower.side <- 3 - higher.side
    x <- start

    # make sure that x is not out of range
    if (x < limit$x[1]) x <- (max.point$x + limit$x[1])/2


    bounds$range <- range
    bounds$higher.side <- higher.side


    # Register initial x into bounds

    h <- o$h(x, A)
    hp <- o$hp(x, A)


    if (length(max.point$x) > 0)
    {
      bounds$include.soln <- h < target
      side <- lower.side
    }
    else
    {
      side <- 1
    }


    bounds$x[side] <- x
    bounds$h[side] <- h
    bounds$hp[side] <- hp
    visited.x <- x


    # Do a first step

    if (correct4hp.sign) hp <- expected.hpsign * abs(hp)
    change <- (h-target)/hp
    x <- x - change


    # continue until convergence

    unreachable.mode <- F
    continue <- T
    count <- 0

    while (continue)
    {
      computed.h <- F
      accepted <- F
      count <- count + 1

      while (!accepted)
      {
        wb <- within.bounds(x, bounds)

        if (bounds$include.soln)
        {
          if (wb$within)
          {
            accepted <- T
          }
          else
          {
            x <- cubic.extrapolation(target, bounds, monotonic, range)
            accepted <- !is.na(x)
          }
        }
        else if (wb$above)
        {
          # bounds do not include solution and we are looking above bounds

          if (x > limit$x[2])
          {
            if (limit$checked[2])
            {
              accepted <- T
              h <- limit$h[2]


              if (unreached.target(target, h, higher.side, monotonic, mode.search))
              {
                # force end of while-loop, as target is not reached even at this end
                x <- limit$x[2]
                h <- target
                computed.h <- T
              }
              else if (limit$usable[2])
              {
                x <- bounds$range[2]
                hp <- limit$hp[2]
                computed.h <- T
              }
              else
              {
                x <- (x + change + bounds$range[2])/2
              }
            }
            else
            {
              limit$checked[2] <- T
              h <- o$h(limit$x[2], A)


              if (unreached.target(target, h, higher.side, monotonic, mode.search))
              {
                # force end of while-loop, as target is not reached even at this end
                x <- limit$x[2]
                h <- target
                accepted <- T
                computed.h <- T
              }
              else
              {
                limit$h[2] <- h

                if (is.finite(h))
                {
                  hp <- o$hp(limit$x[2], A)
                  limit$hp[2] <- hp
                  limit$usable[2] <- is.finite(hp)
                  accepted <- limit$usable[2]
                }
                else
                {
                  accepted <- F
                }

                if (accepted)
                {
                  x <- bounds$range[2]
                  computed.h <- T
                  unreachable.mode <- mode.search & h > 0 # new_0.11
                }
                else
                {
                  x <- (x + change + bounds$range[2])/2
                }
              }
            }
          }
          else
          {
            tmp <- new.bound(x, bounds, monotonic, o, A, target, range)

            x <- tmp$x
            h <- tmp$h
            hp <- tmp$hp
            computed.h <- T
            accepted <- T
          }
        }
        else
        {
          # bounds do not include solution and we are looking below bounds

          if (x <= limit$x[1])
          {
            # we are looking out of the variable domain (bad!)

            if (!limit$checked[1])
            {
              limit$checked[1] <- T

              h <- o$h(limit$x[1], A)
              limit$h[1] <- h

              if (is.finite(h))
              {
                if (unreached.target(target, h, higher.side, monotonic, mode.search, left.side=T))
                {
                  # force end of while-loop, as target is not reached even at this end
                  x <- limit$x[1]
                  h <- target
                  accepted <- T
                }
                else
                {
                  hp <- o$hp(limit$x[1], A)
                  limit$hp[1] <- hp
                  limit$usable[1] <- is.finite(hp)
                  accepted <- limit$usable[1]
                }

                computed.h <- accepted
                if (accepted) x <- limit$x[1]
              }
              else
              {
                limit$usable[1] <- F
                x <- (min(bounds$x, na.rm=T) + limit$x[1]) / 2
              }
            }
            else if (limit$usable[1])
            {
              x <- limit$x[1]
              h <- limit$h[1]
              hp <- limit$hp[1]
              computed.h <- T
              accepted <- T
            }
            else
            {
              # not limit$usable[1]
              x <- (min(bounds$x) + limit$x[1]) / 2
            }
          }
          else
          {
            tmp <- new.bound(x, bounds, monotonic, o, A, target, range)

            x <- tmp$x
            h <- tmp$h
            hp <- tmp$hp
            computed.h <- T
            accepted <- T

            # new_0.11
            unreachable.mode <- mode.search & ((x == limit$x[2] & h > 0) | (x == limit$x[1] & h < 0))
          }
        }
      }


      if (!computed.h)
      {
        h <- o$h(x, A)
        hp <- o$hp(x, A)
      }

      converged <- abs(h - target) < epsilon | unreachable.mode  # modif_0.11

      # new_0.20
      if (!converged & bounds$include.soln)
      {
        converged <- diff(bounds$x) < epsilon
        if (converged) x <- mean(bounds$x)
      }

      continue <- !converged


      if (continue)
      {
        # register results in bounds

        h.lt.target <- h < target

        if (bounds$include.soln)
        {
          side <- ifelse(h.lt.target, lower.side, higher.side)
        }
        else
        {
          bounds$include.soln <- xor(h.lt.target, bounds$h[1] < target)

          if (found2bounds)
          {
            if (mode.search)
            {
              side <- ifelse(h.lt.target, lower.side, higher.side)
              change.opposite.side.bounds <- T
            }
            else if (bounds$include.soln)
            {
              # bounds newly include solution (happening for the first time with current x)
              wb <- within.bounds(x, bounds)

              if (wb$within)
              {
                if (higher.side == 2)
                {
                  side <- lower.side
                  change.opposite.side.bounds <- F


                  if (hp < 0)
                  {
                    bounds$x[lower.side] <- x
                    bounds$h[lower.side] <- h
                    bounds$hp[lower.side] <- hp

                    tmp <- leftSide.fastScan(bounds, o, A, target, range[lower.side], h2modeless)

                    x <- tmp$x
                    h <- tmp$h
                    hp <- tmp$hp
                    continue <- tmp$continue
                  }
                }
                else
                {
                  stop("Scenario imprevu /1a --- ICI\n", save.objects, '\n')
                }
              }
              else if (wb$below)
              {
                side <- 1
                change.opposite.side.bounds <- T
              }
              else
              {
                # wb$above
                side <- 2
                change.opposite.side.bounds <- T
              }
            }
            else
            {
              # bounds still do not include solution

              wb <- within.bounds(x, bounds)

              if (wb$within)
              {
                change.opposite.side.bounds <- F

                if (h > max(bounds$h))
                {
                  side <- higher.side
                }
                else if (h < min(bounds$h))
                {
                  side <- ifelse(hp < 0, lower.side, higher.side)
                }
                else
                {
                  side <- higher.side
                }
              }
              else if (wb$below)
              {
                if (higher.side == 2)
                {
                  if (hp < 0)
                  {
                    bounds$x[lower.side] <- x
                    bounds$h[lower.side] <- h
                    bounds$hp[lower.side] <- hp

                    tmp <- leftSide.fastScan(bounds, o, A, target, range[lower.side], h2modeless)

                    x <- tmp$x
                    h <- tmp$h
                    hp <- tmp$hp
                    continue <- tmp$continue
                    change.opposite.side.bounds <- T
                  }
                  else if (h < min(bounds$h))
                  {
                    side <- lower.side
                    change.opposite.side.bounds <- T
                  }
                  else
                  {
                    leftside.potential.start.point <- list(x=x, h=h, hp=hp)

                    lim <- c(x, bounds$x[lower.side])
                    x <- mean(lim)
                    hp <- o$hp(x, A)

                    while (hp > 0)
                    {
                      h <- o$h(x, A)
                      side <- ifelse(h > bounds$h[lower.side], 1, 2)
                      lim[side] <- x
                      x <- mean(lim)
                      hp <- o$hp(x, A)
                    }

                    h <- o$h(x, A)

                    bounds$x[higher.side] <- bounds$x[lower.side]
                    bounds$h[higher.side] <- bounds$h[lower.side]
                    bounds$hp[higher.side] <- bounds$hp[lower.side]

                    bounds$x[lower.side] <- x
                    bounds$h[lower.side] <- h
                    bounds$hp[lower.side] <- hp

                    tmp <- leftSide.fastScan(bounds, o, A, target, range[lower.side], h2modeless, ls.start=leftside.potential.start.point)

                    x <- tmp$x
                    h <- tmp$h
                    hp <- tmp$hp
                    continue <- tmp$continue

                    side <- higher.side
                    change.opposite.side.bounds <- F
                  }
                }
                else
                {
                  stop("Scenario imprevu /1b --- ICI\n", save.objects, '\n')
                }
              }
              else
              {
                # wb$above
                side <- lower.side
                change.opposite.side.bounds <- T
              }
            }


            if (change.opposite.side.bounds)
            {
              opposite.side <- 3 - side

              bounds$x[opposite.side]  <- bounds$x[side]
              bounds$h[opposite.side]  <- bounds$h[side]
              bounds$hp[opposite.side] <- bounds$hp[side]
            }
          }
          else
          {
            # !found2bounds
            found2bounds <- T

            side <- ifelse(h < bounds$h[1], lower.side, higher.side)

            if (side == 1)
            {
              bounds$x[2]  <- bounds$x[1]
              bounds$h[2]  <- bounds$h[1]
              bounds$hp[2] <- bounds$hp[1]
            }
          }
        }


        bounds$x[side]  <- x
        bounds$h[side]  <- h
        bounds$hp[side] <- hp
        visited.x <- c(visited.x, x)

        converged <- abs(h - target) < epsilon | unreachable.mode  # modif_0.11


        if (!converged)
        {
          continue <- count < max.niter # new_0.11: changed <= for <

          if (!continue)
          {
            # Before we give up, we check one last thing:
            # if the last few steps were all leaning in the same direction,
            # then convergence may only be a question of time & patience!
            # Give it (yet) another chance!

            tmp <- sign(diff(visited.x))
            last.change.direction <- tmp[length(tmp)]
            iter.in.opposite.direction <- which(tmp==-last.change.direction)

            if (length(iter.in.opposite.direction) == 0)
            {
              continue <- T
            }
            else
            {
              last.iter.in.opposite.direction <- max(iter.in.opposite.direction)
              if ((count - last.iter.in.opposite.direction) > 20) continue <- T
            }


            # Alternatively, if range was unsatisfactorily explored, take a point
            # in the middle of unexplored range and redo

            # new_0.19
            if (!continue)
            {
              tmp <- unexplored.range(visited.x, epsilon)
              continue <- tmp$continue
              if (continue) x <- tmp$x
            }


            if (continue)
            {
              count <- 0
              visited.x <- numeric(0)
            }
          }
        }


        if (continue)
        {
          found2bounds <- T
          abs.hp <- abs(hp)
          if (correct4hp.sign) hp <- expected.hpsign * abs.hp

          change <- (h - target)/hp
          pct.change <- abs(change)/diff(bounds$x)

          if (pct.change < 1e-4 & abs.hp > 1000)
          {
            if (change < 0) p <- c(0.9, 0.1)
            else p <- c(0.1, 0.9)
            x <- sum(p * bounds$x)
          }
          else x <- x - change
        }
        else if (!converged & diff(bounds$x) < epsilon)
        {
          # new_0.20

          if (bounds$include.soln)
          {
            converged <- T
            continue <- F
            x <- mean(bounds$x)
          }
          else
          {
            change <- - (max(bounds$h)-target)/mean(bounds$hp)
            x <- mean(bounds$x) + change + epsilon*sign(change)
            continue <- T
            count <- 0
          }
        }


        if (cat.x)
        {
          cat('count=', count, '\n')
          cat('x = ', x, '\n')
          cat('h = ', h, '\n')
          cat('target = ', target, '\n')
          cat('abs(h-target)', abs(h-target), '\n')
          cat('bounds$x = ', bounds$x, '\n')
          cat('diff(bounds$x) = ', diff(bounds$x), '\n')
          if (diff(bounds$x) < 0) cat('***************\n')
        }
      }
    }

    list(x=x, converged=converged, bounds=bounds)
  } # end of secured.NR.search


  smoothed.area.estimate <- function(area, x, A, math.lower.limit, ref.range, inestimable.lower.limit, d=min(c(1e-4, diff(ref.range)/1000)), max.count=10000, hp.mult=1, remote.right=F)
  {
    f.x <- A$f(x, A)

    a <- rep(NA, 3)
    # values around x
    x.star <- x + c(-1, 0, 1)*d


    if (x.star[1] < math.lower.limit)
    {
      if (inestimable.lower.limit)
      {
        x.star[1] <- (x + math.lower.limit)/2
      }
      else
      {
        x.star[1] <- math.lower.limit
      }
    }


    for (i in 1:3) a[i] <- area(x.star[i], A)
    a.x <- a[2]

    observed.slopes <- hp.mult*diff(a)/diff(x.star)
    expected.slope <- f.x

    observed.slope.angles <- atan(observed.slopes)
    expected.slope.angle  <- atan(expected.slope)
    angle.diff <- max(abs(expected.slope.angle-observed.slope.angles))

    estimate.ok <- angle.diff < 0.035 & all(observed.slopes > 0)
    # 0.035 radians is approximately 2 degrees

    continue <- !estimate.ok


    if (continue)
    {
      # We look for a point on the left side of x and one on its right side
      # where the integral [area] seems to be correctly estimated
      # (with positive slopes of value close to expected value [density])

      # If we are estimating the area on the remote right end of the distrn (remote.right=T)
      # then we will be happy if an estimate to the left of x is obtained

      found <- list(ok=rep(F,2), x=rep(NA,2), a=rep(NA,2), f=rep(NA,2))
      side <- 0

        # Store starting points
        tmp.x <- x.star
        tmp.a <- a


      while (continue)
      {
        side <- side + 1 # side = 1 -> Left  of x
                         # side = 2 -> Right of x

        direction <- ifelse(side==1, -1, 1)


        if (side == 2)
        {
          x.star <- tmp.x
          a <- tmp.a
        }

        j <- ifelse(side==1, 1, 3)
        z <- x.star[j]

        count <- 0
        on.border <- F

        while (continue)
        {
          z <- z + direction * d

          # make sure that we don't step to the left of mathematical lower limit
          if (z <= math.lower.limit)
          {
            if (inestimable.lower.limit)
            {
              z <- (math.lower.limit + z + d) / 2
            }
            else
            {
              z <- math.lower.limit
              on.border <- T
            }
          }


          if (side == 1 & z == math.lower.limit)
          {
            # This is still possible even with the above 'protection' against
            # this scenario, due to numerical imprecision
            a.z <- 0
            continue <- F
            estimate.ok <- T
          }
          else
          {
            a.z <- area(z, A)


            if (side == 1)
            {
              x.star <- c(z, x.star[-3])
              a <- c(a.z, a[-3])
            }
            else
            {
              x.star <- c(x.star[-1], z)
              a <- c(a[-1], a.z)
            }


            observed.slopes <- hp.mult*diff(a)/diff(x.star)
            expected.slope <- A$f(z, A)

            observed.slope.angles <- atan(observed.slopes)
            expected.slope.angle  <- atan(expected.slope)
            angle.diff <- max(abs(expected.slope.angle-observed.slope.angles))

            estimate.ok <- (angle.diff < 0.035 & all(observed.slopes > 0)) | angle.diff < 1e-6
            continue <- !estimate.ok & !on.border
          }


          if (continue)
          {
            if (remote.right & side == 1)
            {
              continue <- expected.slope/f.x < 10000
            }
            else
            {
              count <- count + 1
              continue <- count <= max.count
            }
          }
        }


        if (side == 1 & remote.right)
        {
          continue <- !estimate.ok
          if (estimate.ok) a.x <- a.z
        }
        else continue <- side == 1


        # Store results
        found$ok[side] <- estimate.ok
        found$x[side]  <- z
        found$a[side]  <- a.z
        found$f[side]  <- expected.slope

      } # end of while-continue


      if (!remote.right | !found$ok[1])
      {
        if (found$ok[1] & found$ok[2])
        {
          f.diff <- diff(found$f)
          extrapolate <- f.diff == 0

          if (!extrapolate)
          {
            x.intersect <- - (diff(found$a) - diff(found$f*found$x)) / f.diff
            extrapolate <- x.intersect < found$x[1] | x.intersect > found$x[2]

            if (!extrapolate)
            {
              j <- ifelse(x <= x.intersect, 1, 2)
              a.x <- found$a[j] + found$f[j]*(x-found$x[j])
            }
          }


          if (extrapolate) a.x <- found$a[1] + diff(found$a)/diff(found$x) * (x - found$x[1])
        }
        else if (found$ok[1] | found$ok[2])
        {
          j <- which(found$ok)
          a.x <- found$a[j] + found$f[j]*(x-found$x[j])
        }
        else
        {
          stop('Could not get a smoothed cumulative density estimate.\n', save.objects, '\n')
        }
      }
    }


    a.x
  } # end of smoothed.area.estimate


  ref <- ref.points(o, A, start, range, inestimable.lower.limit=inestimable.lower.limit, epsilon=precision)


  # modif_0.11
  if (ref$mode$found)
  {
    math.lower.limit <- o$math.lower.limit

    A$f <- o$f
    A$lower.limit <- ref$remote[1]
    A$M <- ref$mode$h # (kind of a) standardizing constant
    area <- function(x, A){integrate(A$f, A$lower.limit, x, A=A)$value}

    area.tot <- smoothed.area.estimate(area, ref$remote[2], A, math.lower.limit, ref$remote, inestimable.lower.limit, remote.right=T)
    target <- u*area.tot
    precision <- precision*area.tot

    x <- ref$mode$x
    area.x <- 0
    area.mode <- smoothed.area.estimate(area, x, A, math.lower.limit, ref$remote, inestimable.lower.limit)
    u.mode <- area.mode/area.tot

    converged <- abs(area.mode - target) < precision
    continue <- !converged
  }
  else
  {
    stop('mode not found\n')
  }


  if (continue)
  {
    distrn.leftSide <- u < u.mode

    if (distrn.leftSide)
    {
      area <- function(x, A){integrate(A$f, x, A$upper.limit, A=A)$value}
      A$upper.limit <- ref$mode$x
      range <- c(ref$remote[1], ref$mode$x)
    }
    else
    {
      A$lower.limit <- ref$mode$x
      range <- c(ref$mode$x, ref$remote[2])
    }
  }



  if (continue & o$potentially.bimodal)
  {
    # Slower but safer algorithm for posterior distrns that are potentially bimodal

    f.mode <- A$f(ref$mode$x, A)
    start <- ref$mode$x  + (target-area.mode)/f.mode

    continue <- F
    converged <- T

    if (distrn.leftSide)
    {
      highest.point <- ref$remote[1]
      h.max <- area.mode
      target <- (u.mode - u)/u.mode * h.max
    }
    else
    {
      highest.point <- ref$remote[2]
      h.max <- smoothed.area.estimate(area, ref$remote[2], A, math.lower.limit, range, inestimable.lower.limit, remote.right=T)
      target <- (u-u.mode)/(1-u.mode) * h.max
    }


    o.cum <- list(h=area, hp=o$f)

    nr <- secured.NR.search(o.cum, A, start, precision, target=target,
                             range=range, max.point=list(x=highest.point, h=h.max),
                             max.niter=NR.max.iter)

    if (nr$converged)
    {
      x <- nr$x
    }
    else
    {
      # it seems we were caught in an infinite loop, had we not limited the number of iterations;
      # see if we can rule that problem out

      x <- nr$bounds$x[1]
      h <- nr$bounds$h[1]

      x <- ping.pong.tie.breaker(area, A, x, h, target, math.lower.limit, range, inestimable.lower.limit, precision, distrn.leftSide=distrn.leftSide)
    }
  }



  if (continue)
  {
    count <- 0
    direction <- ifelse(distrn.leftSide, -1, 1)

    if (distrn.leftSide)
    {
      target <- (u.mode - u)/u.mode * area.mode
    }
    else
    {
      area.remote.right <- smoothed.area.estimate(area, ref$remote[2], A, math.lower.limit, range, inestimable.lower.limit, remote.right=T)
      target <- (u-u.mode)/(1-u.mode) * area.remote.right
    }


    # new_0.21 New starting point (based on a normal approximation)
    z.max <- sqrt(-2*ref$log.f.ratio.remote)
    z <- qnorm(u)
    if (abs(z) > z.max) z <- sign(z) * z.max * 0.99
    if (distrn.leftSide)
    {
      z <- min(z, 0)
      side <- 1
    }
    else
    {
      z <- max(z, 0)
      side <- 2
    }
    side.len <- abs(ref$mode$x - ref$remote[side])
    side.sigma <- side.len / z.max
    x <- ref$mode$x + z * side.sigma
    area.x <- area(x, A)
  }


  # modif_0.23  The second while-block below existed before, but is now embedded in this if-block, and
  #             preceded by an almost identical while-block, which is more cautious since we do not start
  #             at mode anymore, which ensured convergence to solution without further precautions
  #             (on possible stepping out of bounds)

  if (continue)
  {
    continue <- area.x > target # new_0.23

    # new_0.23
    while (continue)
    {
      hp <- o$f(x, A)
      change <- (target - area.x)/hp*direction
      x <- x + change

      if (direction > 0)
      {
        if (x < A$lower.limit) x <- (x - change + A$lower.limit) / 2
      }
      else if (x > A$upper.limit) x <- (x - change + A$upper.limit) / 2


      area.x <- area(x, A)
      count <- count + 1
      converged <- abs(area.x - target) < precision
      continue <- !converged  & count <= NR.max.iter & area.x > target
    }

    continue <- !converged & count <= NR.max.iter # new_0.23

    # same as above, but without the direction-checks, which slow down the process
    while (continue)
    {
      hp <- o$f(x, A)
      change <- (target - area.x)/hp*direction
      x <- x + change

      area.x <- area(x, A)
      count <- count + 1
      converged <- abs(area.x - target) < precision
      continue <- !converged  & count <= NR.max.iter
    }
  }


  if (!converged)
  {
    if (x > ref$remote[2] | x < ref$remote[1])
    {
      stop("stepped out of bounds -- due to numerical imprecision?", save.objects, '\n')
    }
    else
    {
      # it seems we were caught in an infinite loop, had we not limited the number of iterations;
      # see if we can rule that problem out

      x <- ping.pong.tie.breaker(area, A, x, area.x, target, math.lower.limit, range, inestimable.lower.limit, precision, distrn.leftSide=distrn.leftSide)
    }
  }


  if (length(save.objects) > 0 & unlink) unlink(save.objects)

  x
} # end of dens.gen.icdf


empty.matrices <- function(n.iter, n.chains, me)
{
  # n.iter & n.chains: scalars
  # me: list

  template <- matrix(NA, nrow=n.chains, ncol=n.iter)

  out <- list(mu=template, sd=template)
  if (me$any & !me$known) out$me.parm <- template

  out
} # end of empty.matrices


logp.from.logpcum <- function(log.pcum)
{
  # log.pcum: vector of length 2

  if (log.pcum$lower.tail)
  {
    j <- 1
    s <- -1
  }
  else
  {
    j <- length(log.pcum$log.pcum)
    s <- 1
  }

  log.pcum$log.pcum[-j] + log(1-exp(s*diff(log.pcum$log.pcum))) # scalar output

} # end of logp.from.logpcum


logPhi.quadratic.approx.coeff <- function(degree=2)
{
  theta <- c(-log(2), sqrt(2/pi), -1/pi)
  if (degree == 3) theta <- c(theta, (4/pi-1)*sqrt(2/pi)/6)
  theta
} # end of logPhi.quadratic.approx.coeff


lphi <- function(z)
{
  # z: can be scalar or vector

  log.phi <- dnorm(z, log=T)      # log(phi(z))
  log.Phi <- pnorm(z, log.p=T)    # log(Phi(z))

  r  <- exp(log.phi-log.Phi)      # phi(z)/Phi(z)
  r2 <- exp(2*(log.phi-log.Phi))  # phi^2(z) / Phi^2 (z)

  list(r=r, r2=r2) # r and r2 are of same length as z
} # end of lphi


me.gen <- function(o, me, data, gen.y, true.values, RData=list(save=F))
{
  done <- F

  # new_0.18
  if (RData$save) save.objects <- paste(RData$dir, '_me.RData', sep='/')
  else save.objects <- character(0)


  if (me$through.cv)
  {
    if (o$logNormal.distrn)
    {
      log.y <- c(data$log$y, gen.y$log$gt, gen.y$log$lt, gen.y$log$i)
      log.t <- c(true.values$log$y, true.values$log$gt, true.values$log$lt, true.values$log$i)
      b <- sum((exp(log.y-log.t) - 1)^2) / 2
    }
    else
    {
      y <- c(data$y, gen.y$gt, gen.y$lt, gen.y$i)
      t <- c(true.values$y, true.values$gt, true.values$lt, true.values$i)
      b <- sum((y/t-1)^2) / 2
    }

    A <- c(o$A, list(b=b, range=me$range))
    me.parm <- dens.gen.icdf(o, A, range=me$range, start=o$start(A), inestimable.lower.limit=me$range[1]==0, save.objects=save.objects)
    done <- T
  }
  else
  {
    y <- c(data$y, gen.y$gt, gen.y$lt, gen.y$i)
    t <- c(true.values$y, true.values$gt, true.values$lt, true.values$i)
    b <- sum((y-t)^2) / 2

    if (o$logNormal.distrn)
    {
      A <- c(o$A, list(b=b, true.values=t, range=me$range))
      me.parm <- dens.gen.icdf(o, A, range=me$range, inestimable.lower.limit=me$range[1]==0, save.objects=save.objects)
      done <- T
    }
  }


  if (!done) me.parm <- sqrt.invertedGamma.gen(data$N, b, me$range, o=o, RData=RData)

  me.parm
} # end of me.gen


me.gen.object <- function(me, logNormal.distrn, N)
{
  if (me$through.cv)
  {
    A <- list(N=N, M=0)
    f <- function(v, A){exp(-A$N*log(v) - A$b/v^2 - A$N*pnorm(1/v, log.p=T) - A$M)}
    log.f <- function(v, A){-A$N*log(v) - A$b/v^2 - A$N*pnorm(1/v, log.p=T)}
    log.f.prime  <- function(v, A){l=lphi(1/v); -A$N/v + 2*A$b/v^3 + A$N*l$r/v^2}
    log.f.second <- function(v, A){l=lphi(1/v); A$N/v^2 - 6*A$b/v^4 + A$N/v^4 * (l$r*(1/v - 2*v) + l$r2)}


    # new_0.11
    log.f.inv.remote <- function(target, A)
    {
      D <- -A$b
      B <- -A$N*(log(A$mode)-1) - target
      qA <- - A$N/A$mode
      roots <- real.cubic.roots(c(D, 0, B, qA), l=0, u=A$mode) # modif_0.11

      roots <- c(roots, exp(log(2)-target/A$N))

      roots
    } # end of log.f.inv.remote


    start <- function(A)
    {
      start <- sqrt(2*A$b/A$N)
      if (start < A$range[1] | start > A$range[2]) start <- mean(A$range)
      start
    } # end of start


    # modif_0.11
    o <- list(A=A, f=f, log.f=log.f, log.f.prime=log.f.prime, log.f.second=log.f.second,
              log.f.inv.remote=log.f.inv.remote,
              potentially.bimodal=F, math.lower.limit=0,
              start=start, logNormal.distrn=logNormal.distrn)
  }
  else if (logNormal.distrn)
  {
    A <- list(N=N, M=0)
    f <- function(ksi, A){exp(-A$N*log(ksi) - as.vector(pnorm(matrix(A$true.values, byrow=T, nrow=length(ksi), ncol=A$N)/ksi, log.p=T)%*%matrix(1,ncol=1,nrow=A$N)) - A$b/ksi^2 - A$M)}
    log.f <- function(ksi, A){-A$N*log(ksi) - sum(pnorm(A$true.values/ksi, log.p=T)) - A$b/ksi^2}
    log.f.prime <- function(ksi, A){l=lphi(A$true.values/ksi); -A$N/ksi + sum(l$r*A$true.values/ksi^2) + 2*A$b/ksi^3}  # modif_0.11
    log.f.second <- function(ksi, A){l=lphi(A$true.values/ksi); A$N/ksi^2 + sum(A$true.values*l$r*(A$true.values^2 + ksi*l$r*A$true.values - 2*ksi^2))/ksi^5  - 6*A$b/ksi^4} # modif_0.11


    # new_0.11
    log.f.inv.remote <- function(target, A)
    {
      theta <- c(-A$b, 0, 3*A$N/2 - target, -2*A$N)
      roots <- real.cubic.roots(theta, l=0)

      roots <- c(roots, 2*exp(-target/A$N))
      roots
    } # end of log.f.inv.remote

    # modif_0.11
    o <- list(A=A, f=f, log.f=log.f, log.f.prime=log.f.prime, log.f.second=log.f.second,
              log.f.inv.remote=log.f.inv.remote,
              potentially.bimodal=F, math.lower.limit=0,
              logNormal.distrn=logNormal.distrn)
  }
  else
  {
    o <- sigma.gen.object(N)
    o$logNormal.distrn <- logNormal.distrn
  }

  o
} # end of me.gen.object


mu.truncatedData.gen <- function(o, range, mu.mean, sigma, RData=list(save=F))
{
  # o: list
  # range: vector of length 2
  # mu.mean & sigma: scalars

  # new_0.18
  if (RData$save) save.objects <- paste(RData$dir, '_muTruncatedData.RData', sep='/')
  else save.objects <- character(0)

  A <- c(o$A, list(mu.mean=mu.mean, s=sigma, s2=sigma^2))

  mu <- dens.gen.icdf(o, A, range=range, save.objects=save.objects)
  mu
}  # end of mu.truncatedData.gen


mu.truncatedData.gen.object <- function(N)
{
  A <- list(N=N, M=0) # list of arguments of f

  f <- function(mu, A){exp(-A$N/2*((mu-A$mu.mean)/A$s)^2 - A$N*pnorm(mu/A$s, log.p=T) - A$M)}
  log.f <- function(mu, A){-A$N/2*((mu-A$mu.mean)/A$s)^2 - A$N*pnorm(mu/A$s, log.p=T)}
  log.f.prime  <- function(mu, A){z=mu/A$s; l=lphi(z); -A$N*(mu-A$mu.mean)/A$s2 - A$N*l$r/A$s}
  log.f.second <- function(mu, A){z=mu/A$s; l=lphi(z); A$N/A$s2 * (z*l$r + l$r2 - 1)}

  # new_0.11
  log.f.inv.remote <- function(target, A)
  {
    k <- -A$N/2/A$s2

    # right-side roots
    B <- -2*A$mu.mean
    C <- A$mu.mean^2
    coeff <- k * c(C, B, 1)
    roots <- quadratic.solution(coeff, target=target)


    # left-side roots
    coeff <- coeff - A$N * logPhi.quadratic.approx.coeff() / c(1, A$s, A$s2)
    tmp <- quadratic.solution(coeff, target=target)
    roots <- c(roots, tmp)

    roots
  } # end of log.f.inv.remote


  o <- list(A=A, f=f, log.f=log.f, log.f.prime=log.f.prime, log.f.second=log.f.second,
            log.f.inv.remote=log.f.inv.remote,
            potentially.bimodal=F, math.lower.limit=-Inf)

  o
} # end of mu.truncatedData.gen.object


out.logout.moments <- function(any.me, logNormal.distrn, data, gen.y, true.values)
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

    out <- list(sum=sum(y), sum2=sum(y^2))
  }
  else
  {
    if (logNormal.distrn)
    {
      out <- list(sum=data$log$uncensored.sum, sum2=data$log$uncensored.sum2)
      y <- c(gen.y$log$gt, gen.y$log$lt, gen.y$log$i)
    }
    else
    {
      out <- list(sum=data$uncensored.sum, sum2=data$uncensored.sum2)
      y <- c(gen.y$gt, gen.y$lt, gen.y$i)
    }

    out <- list(sum=out$sum + sum(y), sum2=out$sum2 + sum(y^2))
  }

  out
} # end of out.logout.moments


out.sample <- function(sample, burnin, n.chains, monitor.burnin, me, logNormal.distrn)
{
  if (me$any & !me$known)
  {
    sample <- renamed.me.parm(sample, me$through.cv)
    if (monitor.burnin) burnin <- renamed.me.parm(burnin, me$through.cv)
  }


  if (logNormal.distrn)
  {
    sample$gm  <- exp(sample$mu)
    sample$gsd <- exp(sample$sd)
  }

  if (n.chains == 1) sample <- lapply(sample, as.vector)
  out <- list(sample=sample)


  if (monitor.burnin)
  {
    if (logNormal.distrn)
    {
      burnin$gm  <- exp(burnin$mu)
      burnin$gsd <- exp(burnin$sd)
    }

    if (n.chains == 1) burnin <- lapply(burnin, as.vector)
    out$burnin <- burnin
  }


  out
} # end of out.sample


pnorm.logpcum <- function(x, mean=0, sd=1)
{
  # x: can be vector or scalar

  if (length(x) > 1)
  {
    z <- c(min(x), max(x))
    z <- abs(z - mean)
    lower.tail <- z[1] > z[2]
  }
  else
  {
    lower.tail <- x < mean
  }

  log.pcum <- pnorm(x, mean=mean, sd=sd, log.p=T, lower.tail=lower.tail)
  list(log.pcum=log.pcum, lower.tail=lower.tail)
} # end of pnorm.logpcum


polynomial.product.coeff <- function(theta1, theta2)
{
  len1 <- length(theta1)
  len2 <- length(theta2)
  len <- len1 + len2 - 1
  coeff <- rep(0, len)

  for (i in seq(len1))
  {
    tmp <- theta1[i] * theta2
    w <- seq(from=i, to=i+len2-1)
    coeff[w] <- coeff[w] + tmp
  }

  coeff
} # end of polynomial.product.coeff


# modif_0.11 (added target)
quadratic.solution <- function(theta, target=0, l=-Inf, u=Inf)
{
  C <- theta[1] - target
  B <- theta[2]
  A <- theta[3]

  delta <- B^2 - 4*A*C

  if (delta < 0)
  {
    soln <- NA
  }
  else
  {
    soln <- (-B + c(-1,1)*sqrt(delta))/2/A
    soln <- soln[soln > l & soln < u]
    if (length(soln) == 0) soln <- NA
  }

  soln
} # end of quadratic.solution


random.pow <- function(a, range)
{
  # sample a value from f(x) = 1/x^a on the range specified

  U <- runif(1)

  if (a==1)
  {
    if (is.infinite(range[2])) range[2] <- 1e8
    log.z <- diff(c(U-1,U)*log(range))
    z <- exp(log.z)
  }
  else
  {
    fcum <- range^(1-a)
    tmp <- sum(c(1-U,U)*fcum)
    z <- tmp^(1/(1-a))
  }

  z
} # end of random.pow


real.cubic.roots <- function(theta, target=0, l=numeric(0), u=numeric(0))
{
  # Find real cubic roots of   theta[1] + theta[2]*x + theta[3]*x^2 + theta[4]*x^3  = target

  # Returns an empty vector rather than NA when no solution is found [modif_0.11]

  # *** IMPORTANT ***
  # this function is based on the built-in function polyroot, which solves a polynomial of degree n (here of third degree):
  # if such a function is not accessible in your programming language, you could use the
  # Newton-Raphson-based algorithm alt.real.cubic.roots, found in alt-realCubicRoots.R

  theta[1] <- theta[1] - target
  solns <- polyroot(theta)
  w <- which(abs(Im(solns)) < 1e-6) # indicate which solns are real (with negligeable imaginary part)

  if (length(w) > 0)
  {
    solns <- Re(solns[w])

    # reject out-of-bounds solutions

    if (length(l) > 0)
    {
      w <- which(solns > l)
      solns <- solns[w]
    }

    if (length(u) > 0)
    {
      w <- which(solns < u)
      solns <- solns[w]
    }
  }
  else
  {
    solns <- numeric(0)
  }

  solns
} # end of real.cubic.roots


renamed.me.parm <- function(obj, through.cv)
{
  # rename me.parm either me.sd or me.cv
  obj.names <- names(obj)
  w <- which(obj.names=="me.parm")
  new.name <- ifelse(through.cv, "me.cv", "me.sd")
  names(obj)[w] <- new.name
  obj
} # end of renamed.me.parm


rgamma.truncated <- function(alpha, beta, range)
{
  # We first look on which side of the distribution (comparing to its mode)
  # the range lies, in order to decide on which side we will compute the log.prob
  # (choosing the correct side gives more accuracy in the sampling in the eventuality of a remote range)

  if (alpha < 1)
  {
    lower.tail <- F
  }
  else
  {
    mode <- (alpha-1)/beta

    min.range <- min(range)
    max.range <- max(range)

    if (min.range > mode)
    {
      lower.tail <- F
    }
    else if (max.range < mode)
    {
      lower.tail <- T
    }
    else
    {
      logp.left  <- pgamma(min.range, alpha, beta, log.p=T, lower.tail=T)
      logp.right <- pgamma(max.range, alpha, beta, log.p=T, lower.tail=F)
      lower.tail <- logp.left < logp.right
    }
  }

  logp.lim <- pgamma(range, alpha, beta, log.p=T, lower.tail=lower.tail)
  logp <- runif.logp(logp.lim, lower.tail=lower.tail)
  qgamma(logp, alpha, beta, log.p=T, lower.tail=lower.tail)
} # end of rgamma.truncated


rnorm.censored <- function(mu, sd, lower=numeric(0), upper=numeric(0), negative.values.disallowed=F)
{
  # Useful functions

  rnorm.interval.censored <- function(mu, sd, lower, upper)
  {
    # mu, sd: same length (1, or same as length(lower))
    # lower and upper: same length, but length can be > 1

    log.p.lower <- pnorm(lower, mean=mu, sd=sd, log.p=T)
    log.p.upper <- pnorm(upper, mean=mu, sd=sd, log.p=T)

    l <- length(lower)
    log.p <- numeric(l)

    for (i in seq(l)) log.p[i] <- runif.logp(c(log.p.lower[i], log.p.upper[i]))
    y <- qnorm(log.p, mean=mu, sd=sd, log.p=T)
    y
  } # end of rnorm.interval.censored


  rnorm.left.censored <- function(mu, sd, lower)
  {
    -rnorm.right.censored(-mu, sd, -lower)
  } # end of rnorm.left.censored


  rnorm.right.censored <- function(mu, sd, upper)
  {
    # This function was incorrectly named rnorm.left.censored in previous versions

    # mu, sd: same length (1, or same as length(upper))

    log.p <- log(runif(length(upper))) + pnorm(upper, mean=mu, sd=sd, log.p=T)
    y <- qnorm(log.p, mean=mu, sd=sd, log.p=T)
    y
  } # end of rnorm.right.censored


  # Beginning of calculations


  # new_0.4
  # (note that negative.values.disallowed was added to fct arguments)

  if (negative.values.disallowed)
  {
    if (length(lower) == 0)
    {
      lower.len <- max(1, length(upper))
      lower <- rep(0, lower.len)
    }
  }

  left.censored <- length(lower) > 0


  if (left.censored)
  {
    right.censored <- length(upper) > 0

    if (right.censored)
    {
      # it is interval-censored
      z <- rnorm.interval.censored(mu, sd, lower, upper)
    }
    else
    {
      # is is left-censored
      z <- rnorm.left.censored(mu, sd, lower)
    }
  }
  else
  {
    # then it is right-censored
    z <- rnorm.right.censored(mu, sd, upper)
  }

  z
} # end of rnorm.censored


runif.logp <- function(logp.lim, lower.tail=T, size=1, u=runif(size))
{
  # To sample 'size' values uniformly in c(exp(logp.lim[1]), exp(logp.lim[2]))
  w <- diff(logp.lim)
  if (!lower.tail) w <- abs(w)
  log.p <- log(u) + pexp(w, log.p=T)
  x <- qexp(log.p, log.p=T)
  max(logp.lim) - x
} # end of runif.logp



sigma.gen.object <- function(N, lnorm.mu=numeric(0), lnorm.sd=numeric(0))
{
  if (length(lnorm.mu) > 0)
  {
    A <- list(N=N, lm=lnorm.mu, ls2=lnorm.sd^2, M=0)
    f <- function(sigma, A){exp(-(A$N+1)*log(sigma) - A$b/sigma^2 - (log(sigma)-A$lm)^2/(2*A$ls2) - A$M)}
    log.f <- function(sigma, A){-(A$N+1)*log(sigma) - A$b/sigma^2 - (log(sigma)-A$lm)^2/(2*A$ls2)}
    log.f.prime  <- function(sigma, A){-(A$N+1)/sigma + 2*A$b/(sigma^3) - (log(sigma)-A$lm)/(sigma*A$ls2)}
    log.f.second <- function(sigma, A){(A$N+1)/sigma^2 - 6*A$b/sigma^4 + (log(sigma)-A$lm-1)/(A$ls2*sigma^2)}

    # new_0.11
    log.f.inv.remote  <- function(target, A)
    {
      # small values
      c3 <- - (A$N + 1) + A$lm/A$ls2
      c2 <- A$N + 1 - A$lm^2/2/A$ls2 - target
      c0 <- -A$b
      roots <- real.cubic.roots(c(c0, 0, c2, c3), l=0)

      # large values
      C <- -A$lm^2/2/A$ls2 - target
      B <- A$lm/A$ls2 - (A$N + 1)
      A <- -1/2/A$ls2
      tmp <- quadratic.solution(c(C, B, A), l=0)
      roots <- c(roots, exp(tmp))

      roots
    } # end of log.f.inv.remote


    start <- function(A)
    {
      start <- numeric(0)
      tmp <- 2*A$b/(A$N+1-A$lm/A$ls2)
      if (tmp > 0) start <- sqrt(tmp)
      start <- c(exp(A$lm), start)
      start
    } # end of start
  }
  else
  {
    A <- list(N=N, M=0)
    f <- function(sigma, A){exp(-A$N*log(sigma) - A$b/sigma^2 - A$M)}
    log.f <- function(sigma, A){-A$N*log(sigma) - A$b/sigma^2}
    log.f.prime  <- function(sigma, A){-A$N/sigma + 2*A$b/sigma^3}
    log.f.second <- function(sigma, A){A$N/sigma^2 - 6*A$b/sigma^4}

    # new_.11
    log.f.inv.remote  <- function(target, A)
    {
      c3 <- -A$N
      c2 <- A$N - target
      c0 <- -A$b
      roots <- real.cubic.roots(c(c0, 0, c2, c3), l=0)

      roots <- c(roots, -target/A$N)
      roots
    } # end of log.f.inv.remote


    start <- function(A){sqrt(2*A$b/A$N)}
  }

  o <- list(f=f, log.f=log.f, log.f.prime=log.f.prime, log.f.second=log.f.second, A=A,
            log.f.inv.remote=log.f.inv.remote,
            potentially.bimodal=F, start=start, math.lower.limit=0)

  o
} # end of sigma.gen.object


sigma.truncatedData.gen <- function(o, range, b, mu, current.sigma, RData=list(save=F))
{
  # new_0.18
  if (RData$save) save.objects <- paste(RData$dir, '_sigmaTruncatedData.RData', sep='/')
  else save.objects <- character(0)

  A <- c(o$A, list(b=b, mu=mu))
  start <- o$start(A)
  if (length(start) == 0) start <- current.sigma
  sigma <- dens.gen.icdf(o, A, range=range, start=start, inestimable.lower.limit=range[1]==0, save.objects=save.objects)
  sigma
} # end of sigma.truncatedData.gen


sigma.truncatedData.gen.object <- function(N)
{
  A <- list(N=N, M=0)
  f <- function(s, A){exp(-A$N*log(s) - A$b/s^2 - A$N*pnorm(A$mu/s,log.p=T) - A$M)}
  log.f <- function(s, A){-A$N*log(s) - A$b/s^2 - A$N*pnorm(A$mu/s,log.p=T)}
  log.f.prime  <- function(s, A){z=A$mu/s; l=lphi(z); -A$N/s + 2*A$b/s^3 + A$N*z*l$r/s}
  log.f.second <- function(s, A){z=A$mu/s; l=lphi(z); A$N/s^2 - 6*A$b/s^4 + A$N*z*((z^2-2)*l$r+z*l$r2)/s^2}


  # new_.11
  log.f.inv.remote <- function(target, A)
  {
    c3 <- -A$N
    c2 <- A$N - target
    c0 <- -A$b

    t <- logPhi.quadratic.approx.coeff(degree=3)
    t <- -A$N * t * c(1, A$mu, A$mu^2, A$mu^3)
    theta <- c(c0, 0, c2, c3) + t
    roots <- real.cubic.roots(theta, l=0)

    roots <- c(roots, 2*exp(-target/A$N))
    roots
  } # end of log.f.inv.remote



  # modif_0.11
  start <- function(A)
  {
    s0 <- sqrt(2*A$b/A$N)
    g0 <- pnorm(A$mu/s0, log.p=T)
    gp0 <- -dnorm(A$mu/s0) *A$mu/s0^2 / pnorm(A$mu/s0)
    c3 <- A$N * (gp0 - 1)
    c2 <- A$N * (1 - g0 + gp0*s0)
    c0 <- -A$b
    theta <- c(c0, 0, c2, c3)
    tmp <- real.cubic.roots(theta, l=0)
    if (length(tmp) == 0) tmp <- s0
    tmp
  } # end of start


  o <- list(A=A, f=f, log.f=log.f, log.f.prime=log.f.prime, log.f.second=log.f.second,
            log.f.inv.remote=log.f.inv.remote,
            start=start, potentially.bimodal=F, math.lower.limit=0)
  o
} # end of sigma.truncatedData.gen.object


sqrt.invertedGamma.gen <- function(n, beta, xrange, o=list(), beta.min=1e-8, RData=list(save=F))
{
  # Sample a value from the distrn
  #
  # f(x) =     1
  #         ------- . exp(-beta/x^2)
  #           x^n

  # new_0.18
  if (RData$save) save.objects <- paste(RData$dir, '_sqrtInvGamma.RData', sep='/')
  else save.objects <- character(0)

  if (n <= 1)
  {
    A <- c(o$A, list(b=beta))
    x <- dens.gen.icdf(o, A, range=xrange, start=sqrt(2*beta), inestimable.lower.limit=xrange[1]==0, save.objects=save.objects)
  }
  else if (beta < beta.min)
  {
    if (xrange[1] == 0) xrange[1] <- 0.0001
    x <- random.pow(n, xrange)
  }
  else
  {
    alpha <- (n-1)/2
    invx2.range <- 1/rev(xrange^2)
    invx2 <- rgamma.truncated(alpha, beta, invx2.range)
    x <- 1/sqrt(invx2)
  }

  x
} # end of sqrt.invertedGamma.gen


take.log <- function(obj, include.ydim=F)
{
  out <- list()

  if (length(obj$gt) > 0) out$gt <- log(obj$gt)
  if (length(obj$lt) > 0) out$lt <- log(obj$lt)
  if (length(obj$i) > 0)  out$i  <- log(obj$i)

  if (include.ydim & length(obj$y) > 0) out$y <- log(obj$y)

  out
} # end of take.log


truevalue.gen <- function(o, me, y, mu, u=runif(1), log.y=numeric(0), RData=list(save=F))
{
  # o:     list
  # me:    list
  # mu:    scalar
  # y:     scalar

  # modif_0.16 added argument RData

  A <- o$A
  A$mu <- mu

  if (o$logNormal.distrn) A$log.y <- log.y

  A$y  <- y
  A$y2 <- y^2


  if (o$through.sd)
  {
    A$ksi  <- me$parm
    A$ksi2 <- me$parm^2
  }
  else
  {
    A$cv2 <- me$parm^2
  }


  start <- o$start(A) # can be a scalar or a vector of length 2

  # new_0.16
  if (RData$save) save.objects <- paste(RData$dir, '_tv.RData', sep='/')
  else save.objects <- character(0)

  x <- dens.gen.icdf(o, A, range=o$range, u=u, start=start, inestimable.lower.limit=!o$logNormal.distrn, save.objects=save.objects) # modif_0.11, modif_0.16

  if (o$logNormal.distrn) x <- exp(x) # new_0.11

  x
} # end of truevalue.gen


truevalue.gen.object <- function(me, logNormal.distrn)
{
  A <- list(M=0)


  if (me$through.sd)
  {
    # modif_0.11
    # ME through SD and T ~ logNormal

      # We will sample a value for s = log(t)  [rather than for t]
      # and the functions below will therefore be expressed in terms of s, whose density function is given by:
      # dens(s) = f(t(s)) . |Jacobean|
      #         = f(exp(s)) exp(s)

    range <- c(-Inf, Inf)

    f <- function(s, A){exp(- pnorm(exp(s)/A$ksi, log.p=T) - (A$y-exp(s))^2/2/A$ksi2 - (s-A$mu)^2/2/A$sigma2 - A$M)}
    log.f <- function(s, A){- pnorm(exp(s)/A$ksi, log.p=T) - (A$y-exp(s))^2/2/A$ksi2 - (s-A$mu)^2/2/A$sigma2}
    log.f.prime <- function(s, A){z <- exp(s)/A$ksi; l <- lphi(z); -l$r*z + A$y*z/A$ksi - z^2 - (s-A$mu)/A$sigma2}
    log.f.second <- function(s, A){z <- exp(s)/A$ksi; l <- lphi(z); l$r*(z^3-z) + (l$r*z)^2 + (A$y*exp(s)-2*exp(2*s))/A$ksi2 - 1/A$sigma2}


    log.f.inv.remote <- function(target, A)
    {
      # solutions for large s
      tmp <- A$y + c(-1,1)*sqrt(2*abs(target)*A$ksi2) # vector of length 2
      tmp <- tmp[tmp>0]
      roots <- log(tmp)

      # solutions for small s (large negative values)
      C <- log(2) -A$y2/2/A$ksi2 - A$mu^2/2/A$sigma2
      B <- A$mu/A$sigma2
      qA <- -1/2/A$sigma2
      tmp <- quadratic.solution(c(C, B, qA), target=target)
      roots <- c(roots, tmp)

      # solutions around 0
      C <- log(1/A$ksi) - (A$y-1)^2/2/A$ksi2 - A$mu^2/2/A$sigma2
      B <- A$mu/A$sigma2
      qA <- -1/2/A$sigma2
      tmp <- quadratic.solution(c(C, B, qA), target=target)
      roots <- c(roots, tmp)

      roots
    } # end of log.f.inv.remote


    start <- function(A){c(A$log.y, A$mu)}
  }
  else if (logNormal.distrn)
  {
    # modif_0.11
    # ME through CV and T ~ logNormal

      # We will sample a value for s = log(t)  [rather than for t]
      # and the functions below will therefore be expressed in terms of s, whose density function is given by:
      # dens(s) = f(t(s)) . |Jacobean|
      #         = f(exp(s)) exp(s)

    range <- c(-Inf, Inf)

    f <- function(s, A){exp(-s - (A$y*exp(-s)-1)^2/2/A$cv2 - (s-A$mu)^2/2/A$sigma2 - A$M)}
    log.f <- function(s, A){-s - (A$y*exp(-s)-1)^2/2/A$cv2 - (s-A$mu)^2/2/A$sigma2}
    log.f.prime <- function(s, A){-1 + (A$y2*exp(-2*s) - A$y*exp(-s))/A$cv2 - (s-A$mu)/A$sigma2}
    log.f.second <- function(s, A){-2*A$y2*exp(-2*s)/A$cv2 + A$y*exp(-s)/A$cv2 - 1/A$sigma2}


    start <- function(A)
    {
      C <- -1 + A$y2/A$cv2 - A$y/A$cv2 + A$mu/A$sigma2
      B <- -2*A$y2/A$cv2 + A$y/A$cv2 - 1/A$sigma2
      qA <- 2*A$y2/A$cv2 - A$y/2/A$cv2
      start <- quadratic.solution(c(C, B, qA))

      c3 <- (-4/3*A$y2 + A$y/6) / A$cv2 # new_0.15
      start <- c(start, real.cubic.roots(c(C, B, qA, c3))) # modif_0.17 --- dropped argument l=0

      start <- c(start, A$mu, A$mu-A$sigma2, -B/2/qA, log(A$y)) # new_0.17

      start
    } # end of start


    log.f.inv.remote <- function(target, A)
    {
      tmp <- 1 + c(-1,1)*sqrt(2*A$cv2*abs(target))
      tmp <- tmp[tmp>0]
      roots <- log(tmp/A$y)

      C <- -(A$y-1)^2/2/A$cv2 - A$mu^2/2/A$sigma2
      B <- -1 + (A$y-1)*A$y/A$cv2 + A$mu/A$sigma2
      qA <- -A$y2/2/A$cv2 - 1/2/A$sigma2
      tmp <- quadratic.solution(c(C, B, qA), target=target)

      roots <- c(roots, tmp)

      # new_0.19
      D <- (-A$y2+2*A$y-1)/2/A$cv2 - A$mu^2/2/A$sigma2
      C <- -1 + (A$y2-A$y)/A$cv2 + A$mu/A$sigma2
      B <- (-2*A$y2+A$y)/2/A$cv2 -1/2/A$sigma2
      qA <- (4*A$y2-A$y)/6/A$cv2
      theta <- c(D, C, B, qA)
      tmp <- real.cubic.roots(theta, target)
      roots <- c(roots, tmp)

      roots
    } # end of log.f.inv.remote
  }
  else
  {
    # ME through CV and T ~ Normal

    range <- c(0, Inf)

    f <- function(t, A){exp(-log(t) - (A$y-t)^2/2/A$cv2/t^2 - (t-A$mu)^2/2/A$sigma2 - A$M)}
    log.f <- function(t, A){-log(t) - (A$y-t)^2/2/A$cv2/t^2 - (t-A$mu)^2/2/A$sigma2}
    log.f.prime <- function(t, A){-1/t - (A$y/t^2-A$y2/t^3)/A$cv2 + (A$mu-t)/A$sigma2}
    log.f.second <- function(t, A){1/t^2 + (2*A$y/t^3 - 3*A$y2/t^4)/A$cv2 -1/A$sigma2}


    # new_.11
    log.f.inv.remote <- function(target, A)
    {
      roots <- A$y / (1 + sqrt(-2*A$cv2*target))


      # We try a solution with a small t
      D <- - A$y2/2/A$cv2
      C <- A$y/A$cv2
      B <- 3/2 - 1/2/A$cv2 - A$mu^2/2/A$sigma2 - target
      qA <- -2 + A$mu/A$sigma2
      theta <- c(D, C, B, qA)

      tmp <- real.cubic.roots(theta)
      roots <- c(roots, tmp)


      # Look also for a solution with a large value for t
      tmp <- -2*A$sigma2*(target + 1/2/A$cv2)
      if (tmp > 0)
      {
        root <- A$mu + sqrt(tmp)
        if (root > 0) roots <- c(roots, root)
      }

      roots
    } # end of log.f.inv.remote


    # modif_0.11
    start <- function(A)
    {
      # solutions in small values for t
      C <- A$y2/A$cv2
      B <- -A$y/A$cv2
      roots.small <- quadratic.solution(c(C, B, -1), l=0)

      # solutions in large values for t
      D <- A$y/A$cv2
      B <- -A$mu/A$sigma2
      qA <- 1/A$sigma2
      theta <- c(D, 1, B, qA)
      roots.large <- real.cubic.roots(theta, l=0)

      roots <- c(roots.small, roots.large)
      roots[!is.na(roots)]
    } # end of start
  }


  # modif_0.11
  o <- list(f=f, log.f=log.f, log.f.prime=log.f.prime, log.f.second=log.f.second,
            A=A, range=range, logNormal.distrn=logNormal.distrn,
            through.sd=me$through.sd, through.cv=me$through.cv,
            log.f.inv.remote=log.f.inv.remote,
            start=start, potentially.bimodal=T, math.lower.limit=range[1])

  o
} # end of truevalue.gen.object


truevalues.gen <- function(gen.y, data, mu, sigma, me, logNormal.distrn, o=list(), RData=list(save=F))
{
  # gen.y:             list
  # data:              list
  # mu:                scalar
  # sigma:             scalar
  # me:                list
  # logNormal.distrn:  logical scalar

  # new_0.16 added argument RData


  new.truevalues <- list()


  if (me$through.sd & !logNormal.distrn)
  {
    me.sd <- me$parm
    tau.star <- 1/sigma^2 + 1/me.sd^2
    sd.star <- 1/sqrt(tau.star)

    if (data$size$y > 0)
    {
      tmp.mean <- (data$y/me.sd^2 + mu/sigma^2) / tau.star
      new.truevalues$y <- rnorm(data$size$y, tmp.mean, sd.star)
    }

    if (data$size$gt > 0)
    {
      tmp.mean <- (gen.y$gt/me.sd^2 + mu/sigma^2) / tau.star
      new.truevalues$gt <- rnorm(data$size$gt, tmp.mean, sd.star)
    }

    if (data$size$lt > 0)
    {
      tmp.mean <- (gen.y$lt/me.sd^2 + mu/sigma^2) / tau.star
      new.truevalues$lt <- rnorm(data$size$lt, tmp.mean, sd.star)
    }

    if (data$size$i > 0)
    {
      tmp.mean <- (gen.y$i/me.sd^2 + mu/sigma^2) / tau.star
      new.truevalues$i <- rnorm(data$size$i, tmp.mean, sd.star)
    }
  }
  else
  {
    o$A$sigma2 <- sigma^2

    if (data$size$y > 0)  new.truevalues$y  <- rep(0, data$size$y)
    if (data$size$gt > 0) new.truevalues$gt <- rep(0, data$size$gt)
    if (data$size$lt > 0) new.truevalues$lt <- rep(0, data$size$lt)
    if (data$size$i > 0)  new.truevalues$i  <- rep(0, data$size$i)


    if (data$size$y > 0)
    {
      for (j in 1:data$size$y)
      {
        new.truevalues$y[j] <- truevalue.gen(o, me, data$y[j], mu, log.y=data$log$y[j], RData=RData) # modif_0.16
      }
    }


    if (data$size$gt > 0)
    {
      for (j in 1:data$size$gt)
      {
        new.truevalues$gt[j] <- truevalue.gen(o, me, gen.y$gt[j], mu, log.y=gen.y$log$gt[j], RData=RData) # modif_0.16
      }
    }


    if (data$size$lt > 0)
    {
      for (j in 1:data$size$lt)
      {
        new.truevalues$lt[j] <- truevalue.gen(o, me, gen.y$lt[j], mu, log.y=gen.y$log$lt[j], RData=RData) # modif_0.16
      }
    }


    if (data$size$i > 0)
    {
      for (j in 1:data$size$i)
      {
        new.truevalues$i[j] <- truevalue.gen(o, me, gen.y$i[j], mu, log.y=gen.y$log$i[j], RData=RData) # modif_0.16
      }
    }


    if (logNormal.distrn) new.truevalues$log <- take.log(new.truevalues, include.ydim=T)
  }


  new.truevalues
} # end of truevalues.gen


Xinverse.y <- function(X, y)
{
  R <- nrow(X)
  z <- cbind(X, y)

  for (r in 1:(R-1))
  {
    for (s in (r+1):R) z[s,] <- z[s,] - z[s,r]/z[r,r]*z[r,]
  }

  for (r in 1:(R-1))
  {
    for (s in (r+1):R) z[r,] <- z[r,] - z[r,s]/z[s,s]*z[s,]
  }

  y <- z[,R+1]
  z <- diag(z)
  log.abs.theta <- log(abs(y))-log(abs(z))
  theta <- exp(log.abs.theta) * sign(y) * sign(z)
  theta
} # end of Xinverse.y


y.gen <- function(true.values, data, sigma, me, logNormal.distrn, mu=numeric(0))
{
  new.gen.y <- list()


  if (me$any)
  {
    # Sample y (censored) values | true values

    if (data$any.censored$gt)
    {
      tmp.mean <- true.values$gt
      tmp.sd <- cond.values(me$through.cv, me$parm*tmp.mean, me$parm)
      new.gen.y$gt <- rnorm.censored(tmp.mean, tmp.sd, lower=data$cens$gt)
    }

    if (data$any.censored$lt)
    {
      tmp.mean <- true.values$lt
      tmp.sd <- cond.values(me$through.cv, me$parm*tmp.mean, me$parm)
      new.gen.y$lt <- rnorm.censored(tmp.mean, tmp.sd, upper=data$cens$lt, negative.values.disallowed=logNormal.distrn | me$through.cv)
    }

    if (data$any.censored$i)
    {
      tmp.mean <- true.values$i
      tmp.sd <- cond.values(me$through.cv, me$parm*tmp.mean, me$parm)
      new.gen.y$i <- rnorm.censored(tmp.mean, tmp.sd, lower=data$cens$i$gt, upper=data$cens$i$lt)
    }

    if (logNormal.distrn) new.gen.y$log <- take.log(new.gen.y)
  }
  else if (logNormal.distrn)
  {
    if (data$any.censored$gt)
    {
      new.gen.y$log$gt <- rnorm.censored(mu, sigma, lower=data$log.cens$gt)
      new.gen.y$gt <- exp(new.gen.y$log$gt)
    }

    if (data$any.censored$lt)
    {
      new.gen.y$log$lt <- rnorm.censored(mu, sigma, upper=data$log.cens$lt)
      new.gen.y$lt <- exp(new.gen.y$log$lt)
    }

    if (data$any.censored$i)
    {
      new.gen.y$log$i <- rnorm.censored(mu, sigma, lower=data$log.cens$i$gt, upper=data$log.cens$i$lt)
      new.gen.y$i <- exp(new.gen.y$log$i)
    }
  }
  else
  {
    if (data$any.censored$gt) new.gen.y$gt <- rnorm.censored(mu, sigma, lower=data$cens$gt)
    if (data$any.censored$lt) new.gen.y$lt <- rnorm.censored(mu, sigma, upper=data$cens$lt)
    if (data$any.censored$i)  new.gen.y$i  <- rnorm.censored(mu, sigma, lower=data$cens$i$gt, upper=data$cens$i$lt)
  }

  new.gen.y
} # end of y.gen


y.gen.inits <- function(data, mu, sigma, me.through.cv, logNormal.distrn)
{
  gen.y <- list()

  if (logNormal.distrn)
  {
    if (data$any.censored$gt)
    {
      gen.y$log$gt <- rnorm.censored(mu, sigma, lower=data$log.cens$gt)
      gen.y$gt <- exp(gen.y$log$gt)
    }

    if (data$any.censored$lt)
    {
      gen.y$log$lt <- rnorm.censored(mu, sigma, upper=data$log.cens$lt, negative.values.disallowed=me.through.cv & !logNormal.distrn)
      gen.y$lt <- exp(gen.y$log$lt)
    }

    if (data$any.censored$i)
    {
      gen.y$log$i  <- rnorm.censored(mu, sigma, lower=data$log.cens$i$gt, upper=data$log.cens$i$lt)
      gen.y$i <- exp(gen.y$log$i)
    }
  }
  else
  {
    if (data$any.censored$gt) gen.y$gt <- rnorm.censored(mu, sigma, lower=data$cens$gt)
    if (data$any.censored$lt) gen.y$lt <- rnorm.censored(mu, sigma, upper=data$cens$lt, negative.values.disallowed=me.through.cv & !logNormal.distrn)
    if (data$any.censored$i)  gen.y$i  <- rnorm.censored(mu, sigma, lower=data$cens$i$gt, upper=data$cens$i$lt)
  }

  gen.y
} # end of y.gen.inits


############################################################################################################
#
# Fcts of local interest only
#


date.fhead <- function()
{
  tmp <- as.character(Sys.time())
  tmp <- sub(" ", "_", tmp)
  tmp <- gsub("[-\\:]", "", tmp)
  tmp
} # end of date.fhead


temp <- function(fhead, dir='c:/users/p0093616/documents/home/tmp', ext='RData', time.stamp=F)
{
  dot <- grep("\\.", fhead)
  fname <- fhead

  if (time.stamp)
  {
    tmp <- date.fhead()
    fname <- paste(fname, tmp, sep="-")
  }

  if (length(dot) == 0) fname <- paste(fname, ext, sep='.')
  else fname <- fhead

  prefix <- '_'
  continue <- T

  while (continue)
  {
    path <- paste(dir, fname, sep='/')
    continue <- file.exists(path)
    fname <- paste(prefix, fname, sep='')
  }

  path
} # end of temp


