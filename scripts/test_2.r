# Problem 2.2

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