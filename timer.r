# ----------------------
# BAN400 - Assignment 8  
# Timer File
# ----------------------

# Loading packages
library(tictoc)
library(dplyr)

# Creating a function to time measure run time of a file
time_func <- 
  function(file, name) {
    tic.clearlog()
    tic("Regular loop")
    # Run sol1
    toc(log = TRUE)
    printTicTocLog() %>% 
    knitr::kable()
  }

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

tic('Unchanged Solution')
source("scripts/sol1.R")
toc()

tic('Parallel Computing')
source('scripts/sol2.R')
toc()

tic('Multi Core')
source('scripts/sol3.R')
toc()

printTicTocLog() %>% 
  knitr::kable()

