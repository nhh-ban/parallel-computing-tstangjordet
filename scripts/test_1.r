# Problem 2.1

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

