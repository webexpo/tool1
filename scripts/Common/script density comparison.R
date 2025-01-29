############ graphical comparison of JAGS , STAN and WEBEXPO

# V1.00   20 march 2018

#library(rethinking)


fun.compare.dens <-function(jags.chain,stan.chain,webexpo.chain) {

 # jags.chain <-mu.chain.1
  #stan.chain <-mu.chain.2
  #webexpo.chain <-mu.chain.3

  jags.est <- mean( jags.chain  )

  stan.est <- mean ( stan.chain  )

  webexpo.est <- mean ( webexpo.chain  )


  ###function to compute density from empirical density function


  dens.stan <- approxfun(density(stan.chain))

  dens.jags <- approxfun(density(jags.chain))

  dens.webexpo <- approxfun(density(webexpo.chain))

  ######density values for the estimates

  jags.estimate.dens <- dens.jags(jags.est)

  stan.estimate.dens <- dens.stan(stan.est)

  webexpo.estimate.dens <- dens.webexpo(webexpo.est)


  ###graph

  dens(stan.chain , col="black" , lwd=2 , xlab="posterior + 89% HPDI" , show.HPDI = 0.89)

  lines( rep(stan.est,2) , c( 0  , stan.estimate.dens )  , lwd=2 )

  dens(jags.chain , col=rangi2 , add= TRUE , lwd=2 , lty=2 , show.HPDI = 0.89)

  lines( rep(jags.est,2) , c( 0  , jags.estimate.dens )  , lwd=2 , col=rangi2 , lty=2 )

  dens(webexpo.chain , col="red" , add= TRUE , lwd=2 , lty=2 , show.HPDI = 0.89)

  lines( rep(webexpo.est,2) , c( 0  , webexpo.estimate.dens )  , lwd=2 , col=rangi2 , lty=2 )


  legend.x <-max(stan.chain)-0.35*(max(stan.chain)-min(stan.chain))
  legend.y <-max(density(stan.chain)$y)

  legend(x=legend.x,y=legend.y,legend=c("STAN","JAGS","Webexpo"), lty = c(1,2,1) , col=c("black",rangi2,"red"),bty="n")

}
