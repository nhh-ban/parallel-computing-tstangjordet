# Assignment 1:  
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



simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


# Assignment 2:  
MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 

# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 


# Problem 2.1 ----
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

# Starting the timer, and defining a test name
tic("test_1") 
for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 
# Stopping the timer and adding the test to the log
toc(log = TRUE)

# Print tictoc log into a nice table
TicTocLog() |>
knitr::kable()
  

# Problem 2.2 ----
# Defining the number of cores in my computer
maxcores <- 4

#Calculating number of cores I should use
Cores <- min(parallel::detectCores(), maxcores)

# Creating a cluster of the cores so they can work parallel
cl <- makeCluster(Cores)

# Register the cluster
registerDoParallel(cores = Cores)

# Starting the timer, and defining a test name
tic(paste0("test_2,", Cores," cores")) 
# Using foreach-function to make it possible for the cores to calculate 
# separate lines parallel
foreach(
  # Value i equal the number of each line in df, starting from 1.
  i = 1:nrow(df),
  # Merge all rows that is returned
  .combine = 'rbind',
  # Packages required to execute the loop
  .packages = c('magrittr', 'dplyr', 'tweedie')
# Initiating parallel calculations
) %dopar%
# Creating a tibble for each iteration  
tibble(
  # Value N equals value N in row i
  N = df$N[i], 
  # Value M equalls value M in row i
  M = df$M[i], 
  # Using the MtweedieTests-function to calculate the share reject from row i
  share_reject =
   MTweedieTests(
     N=df$N[i],
     M=df$M[i],
     sig = .05
     )
)
# Releasing the extra cores used to force the calculation to be faster
stopCluster(cl)
# Stopping the timer and adding the test to the log
toc(log = TRUE)

# Print tictoc log into a nice table
TicTocLog() |>
  knitr::kable()


# Problem 2.3 ----
MTweedieTests_parallel <- function(N, M, sig) {
  results <- foreach(
    j = 1:M,
    .combine = 'rbind',
    .packages = c('magrittr', 'dplyr', 'tweedie'),
    .export = 'simTweedieTest'
    ) %dopar% {
      simTweedieTest(N) < sig
    }
  sum(results) / M
  }

# Creating a cluster of the cores so they can work parallel
cl <- makeCluster(Cores)

# Register the cluster
registerDoParallel(cores = Cores)

# Starting the timer, and defining a test name
tic(paste0("test_3,", Cores," cores")) 
for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests_parallel( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 
# Releasing the extra cores used to force the calculation to be faster
stopCluster(cl)
# Stopping the timer and adding the test to the log
toc(log = TRUE)

# Print tictoc log into a nice table
TicTocLog() |>
  knitr::kable()


