# Libraries
library(tidyverse)
library(rethinking)

# Coin flips code
pos <- replicate( 1000 , sum( runif(16,-1,1) ) )
hist(pos)

# Growth code
growth <- replicate( 10000 , prod( 1 + runif(12,0,0.1) ) )
dens( growth , norm.comp=TRUE )

# Verifying
big <- replicate( 10000 , prod( 1 + runif(12,0,0.5) ) )
small <- replicate( 10000 , prod( 1 + runif(12,0,0.01) ) )
dens(big)
dens(small)
log.big <- replicate( 10000 , log(prod(1 + runif(12,0,0.5))) )
dens(log.big)
# Adding logs = multiplying original numbers

# Example bayes
w <- 6; n <- 9;
p_grid <- seq(from=0,to=1,length.out=100)
posterior <- dbinom(w,n,p_grid)*dunif(p_grid,0,1)
posterior <- posterior/sum(posterior)

# Loading data
data(Howell1)
d <- Howell1

# Subsetting
d2 <- d[ d$age >= 18 , ]

# Dist
dens(d2$height)

# Plotting priors #

# mu
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )

# sigma
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

# Sampling from the priors
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

# Manually calculating posterior
mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

# Inspecting
contour_xyz( post$mu , post$sigma , post$prob )
image_xyz( post$mu , post$sigma , post$prob )
