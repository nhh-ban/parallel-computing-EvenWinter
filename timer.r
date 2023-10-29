# ----------------------
# BAN400 - Assignment 8  
# Timer File 
# ----------------------

# Loading packages
library(tictoc)
library(tidyverse)


# Creating a function for printing out timer logs in a data frame.
printTicTocLog <-
  function() {
    tic.log() %>%
      unlist %>%
      tibble(logvals = .) %>%
      separate(logvals,
               sep = ":",
               into = c("Function type", "log")) %>%
      mutate(log = str_trim(log)) %>%
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }


tic.clearlog()

tic('Method 1')
source("scripts/sol1.R")
toc(log = TRUE)

tic('Method 2')
source('scripts/sol2.R')
toc(log = TRUE)

tic('Method 3')
source('scripts/sol3.R')
toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()


# Method 3 is clearly the fastest solution. The reason why method 3 is faster 
# than method 2 seems to be that the MTweedieTest function is more
# computationally diffictult than the final for-loop. There are significantly 
# more steps that need to be computed sequentially in MTweedieTest, thus it 
# benefits more from parallel computing.


