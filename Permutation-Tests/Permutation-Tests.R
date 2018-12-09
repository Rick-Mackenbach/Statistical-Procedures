"""
All these exercises can be ran seperately. Just be sure to load in the dataset and load the
dependencies. This will make sure your Rstudio is not going to be flooded with variables. Just run
each exercise without the remove unnecessary variables code at the end.
"""

set.seed(782848)

##  Dependencies   ##
library(ggplot2)
## --------------Loading in the data---------------- ##
## Load in the original dataset
fires_data <- read.csv('forestfires.csv')

## Subset late summer
fires_data_late <- subset(fires_data, month == 'aug' | month == 'sep')

## Subset early summer
fires_data_early <- subset(fires_data, month =='jul' | month == 'jun')
## ------------------------------------------------- ##


"Exercise 1.a"
## --------------Plotting differences--------------- ##
## Calculate mean early summer
mean_early_summer <- mean(fires_data_early$area)

## Plot early summer areas
ggplot(data=fires_data_early, aes(x=area)) +
  geom_histogram() + 
  theme_minimal() + 
  ggtitle(mean_early_summer)
#  ggsave('early_summer.png')

## Calculate mean late summer
mean_late_summer <- mean(fires_data_late$area)

## Plot late summer areas
ggplot(data=fires_data_late, aes(x=area)) +
  geom_histogram() + 
  theme_minimal() +
  ggtitle(mean_late_summer)
#  ggsave('late_summer.png')

## Remove uneccessary values
rm(mean_early_summer); rm(mean_late_summer)
## ------------------------------------------------- ##


"Exercise 1.b"
## --------------Statistical Testing---------------- ##
homogeneity <- var.test(fires_data_late$area, fires_data_early$area)
t_results <- t.test(fires_data_late$area, fires_data_early$area, var.equal = FALSE, alternative = 'greater')
rm(homogeneity); rm(t_results)
## ------------------------------------------------- ##


"Exercise 1.c"
## ----------------------------Bootstrap procedure------------------------------ ##
bootstrap_values <- data.frame(matrix(ncol = 1))
bootstrap_values <- na.omit(bootstrap_values)

## For loop setting initial values
iterations = 10000

## Loop to do the bootstrap procedure
for (i in c(1:iterations)) {
  # n choose k from your dataset
    a <- fires_data_early[sample(nrow(fires_data_early), replace = TRUE), ]
    b <- fires_data_late[sample(nrow(fires_data_late), replace = TRUE), ]
    
  # Compute mean
    mean_bootstrap_early <- mean(a$area)
    mean_bootstrap_late <- mean(b$area)
    bootstrapped_means <- c(mean_bootstrap_late, mean_bootstrap_early)
    
  # Add them in the global variable
    bootstrap_values <- rbind(bootstrapped_means, bootstrap_values)
  
  # Remove the values for overview
    rm(a)
    rm(b)
    rm(mean_bootstrap_early)
    rm(mean_bootstrap_late)
    rm(bootstrapped_means)
}

  ## Remove unecessary variables
  rm(i)
  rm(iterations)

  ## Change name of column
  colnames(bootstrap_values) <- c('Late', 'Early')

  
## Calculate mean differences and set as dataframe
mean_differences <- c(bootstrap_values$Late - bootstrap_values$Early)
mean_differences <- as.data.frame(mean_differences)
colnames(mean_differences) <- 'mdif'

  ## Plot the histogram values
  ggplot(data=mean_differences, aes(x=mdif)) +
    geom_histogram() + 
    theme_light()

  ## Bias 
  a <- mean(mean_differences$mdif)
  b <- mean(fires_data_late$area) - mean(fires_data_early$area)
  bias <- b - a
  rm(a)
  rm(b)

## Confidence intervals
  ## Bootstrap T-confidence intervals (Approximately normal and small bias)
    tcv <- qt(0.975, bootstrap_values$Late + bootstrap_values$Early - 2)
    lowerbound <- mean(mean_differences$mdif)-(tcv*sd(mean_differences$mdif))
    upperbound <- mean(mean_differences$mdif)+(tcv*sd(mean_differences$mdif))
    
  ## Bootstrap percentile interval
    lower_quantile <- quantile(mean_differences$mdif, 0.025)
    upper_quantile <- quantile(mean_differences$mdif, 0.975)
    
  ## Remove unnecessary variables
    rm(bias); rm(lower_quantile); rm(lowerbound); rm(tcv); rm(upper_quantile); rm(upperbound); rm(mean_differences); rm(bootstrap_values)
    
## -----Permutation procedure----- ##
  n <- length(fires_data_late$area)
  m <- length(fires_data_early$area)
  permutation_fires_data <- fires_data$area
  permutation_fires_data <- as.data.frame(permutation_fires_data)
  
  ## Set number of permutation samples
  Psize <- 1000 #nr of permutation samples
  out <- matrix(nrow = Psize, ncol = 2)
  for (i in 1:Psize) {
    permnrs <- sample(1:(n+m))
    xbar_early <- mean(permutation_fires_data[permnrs[1:n], 1])
    xbar_late <- mean(permutation_fires_data[permnrs[(n+1):(n+m)], 1])
    out[i,] <- c(xbar_early,xbar_late)
  }
  
  ## Calculate the P-values
  diff<- out[,2]-out[,1]
  obsdiff<- mean(fires_data_late$area) - mean(fires_data_early$area)
  pval<- (1 + sum(diff > (obsdiff)) ) / (Psize+1)
  pval
  hist(diff, 50, main='Permutation Distribution')
  

  ## Remove unnecessary variables
  rm(Psize); rm(diff); rm(i); rm(m); rm(n); rm(obsdiff); rm(permnrs); rm(pval); rm(xbar_early); rm(xbar_late); rm(out)
  
## ------------------------------------------------------------------------ ##