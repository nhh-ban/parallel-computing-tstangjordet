# Timer script

# Loading packages ----
library(tweedie) 
library(ggplot2)
library(lubridate)
library(purrr)
library(forcats)
library(tictoc)
library(tidyverse)
library(doParallel)
library(magrittr)
library(dplyr)
library(foreach)
library(furrr)
library(tweedie)


# Given functions and definitions ----
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


df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 



# Creating a log/ table here we can save all test results
# Defining the log name
TicTocLog <- 
  # Creating a function without any variables
  function() {
    # Using a "tictoc"-package-function that logs the timing events "tic" and
    # "toc"
    tic.log() %>%
      # Simplifying the list into a vector
      unlist %>%
      # Converting the vector into a tibble
      tibble(logvals = .) %>%
      # Separating the table into to columns
      separate(logvals,
               sep = ":",
               into = c("Function type", "log")) %>%
      # Modifying log column by removing unnecessary
      mutate(log = str_trim(log)) %>%
      # Separating the column log into seconds
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }

# Clearing the log if I do some modifications in the log-function
tic.clearlog()


# Problem 2.1 ----
test_1 <- source("scripts/test_1.r")


# Problem 2.2 ----
test_2 <- source("scripts/test_2.r")


# Problem 2.3 ----
test_3 <- source("scripts/test_3.r")


# Print tictoc log into a nice table
TicTocLog() |>
  knitr::kable()

# This test shows that method 3 test 3 is fastest. I believe it is because of 
# 2 reasons. The first is that it forces the machine to use several cores to
# calculate lines in parallel. The second reason is the location of the foreach
# function, which is the difference between method 2 and 3. Instead of 
# moving back and forth between the calculation and the function, method 3
# makes sure to finish the calculations within the function, and then god back
# to the loop. This saves some seconds. 