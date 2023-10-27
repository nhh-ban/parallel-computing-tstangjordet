# Problem 2.3 ----

# Modifying the original MTweedieTests function
# Defining a name
MTweedieTests_parallel <-
  # Creating a function with 3 arguments
  function(N, M, sig) {
    # Defining a name for the foreach function
    results <- foreach(
      # Making the loop repeat M times
      j = 1:M,
      # Combining each line that returns to a list
      .combine = 'rbind',
      # Packages needed to run the test
      .packages = c('magrittr', 'dplyr', 'tweedie'),
      # Making the test function available in the loop
      .export = 'simTweedieTest'
    # Making sure the loops can run in parallel  
    ) %dopar% {
      # Running the test wwith a logical check if the value is smaller than sig
      simTweedieTest(N) < sig
    }
    # Summing all TRUE values from the loop and dividing it with M
    sum(results) / M
  }

# Defining the number of cores in my computer
maxcores <- 4

#Calculating number of cores I should use
Cores <- min(parallel::detectCores(), maxcores)

# Creating a cluster of the cores so they can work parallel
cl <- makeCluster(Cores)

# Register the cluster
registerDoParallel(cores = Cores)

# Starting the timer, defining a test name and running the function from the 
# original solution
tic(paste0("test_3,", Cores," cores")) 
for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    # Changing from MTweedieTests to the function MTweedieTests_parallel
    MTweedieTests_parallel( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 
# Releasing the extra cores used to force the calculation to be faster
stopCluster(cl)
# Stopping the timer and adding the test to the log
toc(log = TRUE)
