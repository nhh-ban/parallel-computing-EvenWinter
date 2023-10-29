# ----------------------
# BAN400 - Assignment 8  
# Problem 2.1 - Unchanged Solution
# ----------------------

# Loading packages  
library(tweedie) 
library(doParallel)
library(future.apply)


maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores) 

plan(multisession, workers = Cores)


simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


MTweedieTests <-  
  function(N,M,sig){ 
    sum(future_replicate(M,simTweedieTest(N)) < sig)/M 
  } 


df3 <-  
  expand.grid( 
    N = c(10,100,1000,5000,10000), 
    M = 1000, 
    share_reject = NA) 

for(i in 1:nrow(df3)){ 
  df3$share_reject[i] <-  
    MTweedieTests( 
      N=df3$N[i], 
      M=df3$M[i], 
      sig=.05) 
} 
