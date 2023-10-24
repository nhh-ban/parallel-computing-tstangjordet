# Assignment 1:  
library(tweedie) 
library(ggplot2)
library(tictoc)
library(tidyverse)
library(doParallel)
library(magrittr)
library(dplyr)
library(foreach)



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
# Defining the number of cores in mye computer
maxcores <- 4

#Calculating number of cores I should use
Cores <- min(parallel::detectCores(), maxcores)

# Creating a cluster of the cores so they can work parallely
cl <- makeCluster(Cores)

# Register the cluster
registerDoParallel(cores = Cores)

# Starting the timer, and defining a test name
tic(paste0("test_2,", Cores," cores")) 
# Using foreach-function to make it possible for the cores to calculate 
# separate line s

foreach(
  i = 1:nrow(df),
  .combine = 'rbind',
  .packages = c('magrittr', 'dplyr', 'tweedie')
) %dopar%
tibble(
   N = df$N[i], 
   M = df$M[i], 
   share_reject =
     MTweedieTests(
       N=df$N[i],
       M=df$M[i],
       sig = .05
       )
  )
stopCluster(cl)
# Stopping the timer and adding the test to the log
toc(log = TRUE)

# Print tictoc log into a nice table
TicTocLog() |>
  knitr::kable()


# Problem 2.2 ----

## Assignemnt 4 
   
# This is one way of solving it - maybe you have a better idea? 
# First, write a function for simulating data, where the "type" 
# argument controls the distribution. We also need to ensure 
# that the mean "mu" is the same for both distributions. This 
# argument will also be needed in the t-test for the null 
# hypothesis. Therefore, if we hard code in a value here 
# we may later have an inconsistency between the mean of the 
# distributions and the t-test. So, we add it as an explicit 
# argument:  


library(magrittr)
library(tidyverse)

simDat <-
  function(N, type, mu) {
    if (type == "tweedie") {
      return(rtweedie(
        N,
        mu = mu,
        phi = 100,
        power = 1.9
      ))
    }
    if (type == "normal") {
      return(rnorm(N, mean = mu))
    }
    else{
      stop("invalid distribution")
    }
  }


# Next, the test. Note, we use mu two places:
# both for the data simulation and as the null.
simTest <-
  function(N, type, mu) {
    t.test(simDat(N = N,
                  type = type,
                  mu = mu),
           mu = mu)$p.value
  }


# Running many tests is almost the same as before.
# Here the mean is hard coded in, as we're not
# going to change it.
MTests <-
  function(N, M, type, sig) {
    sum(replicate(M,
                  simTest(
                    N = N,
                    type =
                      type,
                    mu =
                      10000
                  )) < sig) / M
  }


# We can now repeat the same analysis as before,
# but for both the tweedie and the normal:
df <-
  expand.grid(
    N = c(10, 100, 1000, 5000),
    M = 1000,
    type = c("tweedie", "normal"),
    share_reject = NA
  ) %>%
  as_tibble()


for (i in 1:nrow(df)) {
  print(i)
  df$share_reject[i] <-
    MTests(df$N[i],
           df$M[i],
           df$type[i],
           .05)
}

# As you see, with normally distributed data, N can
# be very small and the t-test is fine. With a tweedie,
# "large enough" can be many thousands. If we try
# different distributions or parameterizations, we might
# also get different results.
df %>%
  ggplot2::ggplot(aes(x = log(N), y = share_reject, col = type)) +
  geom_line() +
  geom_hline(yintercept = .05) +
  theme_bw() 
