## Dependencies
library(MASS)
library(glmnetUtils)


## ---- QUESTION 1 ---- ##

## Firstly, define the covariance matrices
## Just the identity matrix
no_correlation <- diag(x=1, nrow = 2, ncol = 2)

## Matrix with 0.2 on the T
low_correlation <- c(1, 0.2 , 0.2, 1)
low_correlation <- matrix(low_correlation, nrow = 2, ncol = 2)

## Matrix with 0.95 on the T diagonal
high_correlation <- c(1, 0.95 , 0.95, 1)
high_correlation <- matrix(high_correlation, nrow = 2, ncol = 2)

## Matrix with 0.999 on the T diagonal
perfect_correlation <- c(1, 0.999 , 0.999, 1)
perfect_correlation <- matrix(perfect_correlation, nrow = 2, ncol = 2)

## Now generate the datasets.
no_correlation  <- mvrnorm(100, mu = c(1,1), Sigma = no_correlation)
no_correlation <- as.data.frame(no_correlation)
low_correlation <- mvrnorm(100, mu = c(1,1), Sigma = low_correlation)
low_correlation <- as.data.frame(low_correlation)
high_correlation <- mvrnorm(100, mu = c(1,1), Sigma = high_correlation)
high_correlation <- as.data.frame(high_correlation)
perfect_correlation <-  mvrnorm(100, mu = c(1,1), Sigma = perfect_correlation)
perfect_correlation <- as.data.frame(perfect_correlation)

## Add assign values
no_correlation <- assign('no_correlation', no_correlation)
low_correlation <- assign('low_correlation', low_correlation)
high_correlation <- assign('high_correlation', high_correlation)
perfect_correlation <- assign('perfect_correlation', perfect_correlation)


## Add the Y variable
no_correlation['y'] <- no_correlation$V1 + no_correlation$V2 + rnorm(100, 0, 0.01)
low_correlation['y'] <- low_correlation$V1 + low_correlation$V2 + rnorm(100, 0, 0.01)
high_correlation['y'] <- high_correlation$V1 + high_correlation$V2 + rnorm(100, 0, 0.01)
perfect_correlation['y'] <- perfect_correlation$V1 + perfect_correlation$V2 + rnorm(100, 0, 0.01)

## MC Simulations Function ## 
monte_carlo <- function(dataset = no_correlation, model = 'lm', iterations = 100, samplesize = 40){

    ## Define empty matrix to input our estimators
  values_lm <- data.frame(matrix(ncol = 2))
  
  ## Amount of simulations for Monte-Carlo specified by User
  S <- iterations
  
    ## User specified model
  if (model == 'lm') {
    ## Do the monte carlo stuff
    for (i in c(1:S)) {
      a <- dataset[sample(nrow(dataset), samplesize), ]
      model1 <- lm(a$y ~ a$V1 + a$V2 + 0)
      coeff <- model1$coefficients
      values_lm <- rbind(coeff, values_lm)
    }
  }
  
  if (model == 'ridge') {
    ## Do the monte carlo stuff
    for (i in c(1:S)) {
      a <- dataset[sample(nrow(dataset), samplesize), ]
      model1 <- glmnet(y ~ V1 + V2 + 0, alpha = 0, lambda=0.1, data = a)
      coeff <- c(model1$beta[1], model1$beta[2])
      values_lm <- rbind(coeff, values_lm)
    }
  }
  
  if (model == 'lasso') {
    ## Do the monte carlo stuff
    for (i in c(1:S)) {
      a <- dataset[sample(nrow(dataset), samplesize), ]
      model1 <- glmnet(y ~ V1 + V2 + 0, alpha = 1, lambda=0.1, data = a)
      coeff <- c(model1$beta[1], model1$beta[2])
      values_lm <- rbind(coeff, values_lm)
    }
  }

  ## Remove NA values
  values_lm <- na.omit(values_lm)
  
  ## Compute the final results
  bias_p_1 <- 1 - mean(values_lm$X1)
  bias_p_2 <- 1 - mean(values_lm$X2)
  var_p_1  <- var(values_lm$X1)
  var_p_2 <- var(values_lm$X2)
  mse_p_1 <- ( var_p_1 + (bias_p_1)^2 )
  mse_p_2 <- ( var_p_2 + (bias_p_2)^2 )
  results <- c(bias_p_1, bias_p_2, var_p_1, var_p_2, mse_p_1, mse_p_2)
  
  return(results)

}


## Linear models Monte Carlo
data_list <- c('no_correlation', 'low_correlation', 'high_correlation', 'perfect_correlation')

results_matrix <- data.frame(matrix(ncol = 6))
colnames(results_matrix) <- c('BiasPred1', 'BiasPred2', 'VarPred1', 'VarPred2', 'MSEPred1', 'MSEPred2')

for (word in data_list) {
  data <- (get(word))
  results <- monte_carlo(dataset = data, model='lm', iterations = 1000)
  results_matrix <- rbind(results, results_matrix)
}

results_matrix <- na.omit(results_matrix)
row.names(results_matrix) <- data_list

write.csv2(results_matrix, 'LM Unequal Coefficients')

#rm(results_matrix)

monte_carlo(dataset = perfect_correlation, model = 'lm', iterations = 1000, samplesize = 40)
