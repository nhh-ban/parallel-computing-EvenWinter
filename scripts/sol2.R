# ----------------------
# BAN400 - Assignment 8  
# Problem 2.2 - Parallel Computing
# ----------------------

# Loading packages  
library(tweedie)
library(doParallel)


simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 


df2 <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 


# Setting a maximum number of cores, then choosing the minimum of the max cores
# and the available cores.
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)

# Instantiate the cores
c1 <- makeCluster(Cores)

# Register the cluster
registerDoParallel(c1)

df2 <-
  foreach(
    i = 1:nrow(df2),
    .combine = 'rbind',
    .packages = c('magrittr', 'dplyr', 'tweedie')
  ) %dopar%
  tibble(
    N = df2$N[i],
    M = df2$M[i],
    share_reject =  
      MTweedieTests( 
        N=df2$N[i], 
        M=df2$M[i], 
        sig=.05) 
) 

# Close of clusters
stopCluster(c1)
