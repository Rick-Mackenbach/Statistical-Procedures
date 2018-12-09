## ============================= Exercise 1 ============================= ##
## Read in the data
training <- read.delim2('ChallengeTrainingSet(1).txt')

## Subset IV and DV
trainingX <- training[, c(11:30)]
trainingY <- training$data.scientist.earns
trainingY <- as.data.frame(trainingY)

## We need to clean our data a bit, to remove NA values, and change the outliers
trainingX <- trainingX[-7, ]
trainingY <- na.omit(trainingY)

## Change of large outliers dividing by 12, since yearly estimate was given instead of monthly
trainingY[trainingY > 10000] <- trainingY[trainingY > 10000]/12

## Change it back as a dataframe
trainingY <- as.data.frame(trainingY)

## CV to find best lambda
lasso.mod <- cv.glmnet(as.matrix(trainingX), as.matrix(trainingY), alpha = 1)
plot(lasso.mod)
lasso.mod$lambda.min
lasso.mod$lambda.1se

## Create a model with the best lambda as found by CV
lasso_model <- glmnet(as.matrix(trainingX), as.matrix(trainingY), alpha = 1, lambda = lasso.mod$lambda.min)
lasso_model$beta


## ============================= Exercise 2 ============================= ##
## Include 'Extraversion'
training[81] <- 20 + training[11] + training[21] + training[31] + training[41] + training[51] - 
  training [16] - training[26] - training[36] - training[46] - training[56]

colnames(training)[81] <- 'Extraversion'

## Include 'Agreeableness
training[82] <- 14 - training[12] + training[17] - training[22] + training[27] - training[32] + 
  training[37] - training[42] + training[47] + training[52] + training[57]

colnames(training)[82] <- 'Agreeableness'

## Include 'Conscientiousness'
training[83] <- 14 + training[13] - training[18] + training[23] - training[28] + training[33] -
  training[38] + training[43] - training[48] + training[53] + training[58]

colnames(training)[83] <- 'Conscientiousness'

## Include Neuroticism
training[84] <- 38 - training[14] + training[19] - training[24] + training[29] - training[34] - training[39] -
  training[44] - training[49] - training[54] - training[59]

colnames(training)[84] <- 'Neuroticism'

## Include Openness
training[85] <- 8 + training[15] - training[20] + training[25] - training[30] + training[35] - training[40] +
  training[45] + training[50] + training[55] + training[60]

colnames(training)[85] <- 'Openness'

## Change outcome variables to 0 and 1
training$relationship. <- training$relationship. - 1

fitted_model = glm(formula = relationship.~Extraversion+Agreeableness+Conscientiousness+Neuroticism+Openness, data = training, family = binomial)

fitted_model
summary(fitted_model)

## Predicting stuff
predict_prob <- predict(fitted_model, type = 'response')
predict_prob[1:10]

## Compare the predict vector with the actual vector
predict_prob[predict_prob > .5] <- 'Relationship'
predict_prob[predict_prob < .5] <- 'NotRelationship'

actuals <-training$relationship.
actuals[actuals == 1] <- 'Relationship'
actuals[actuals == 0] <- 'NotRelationship'

## Remove problematic values
actuals[7] <- NA
actuals[48] <- NA
actuals <- na.omit(actuals)

## Create confusion matrix and create accuracy
table(predict_prob,actuals)
prediction = mean(predict_prob==actuals)

## Question 2, include professor score ##
professor <- c(11,7,29,36,24)
professor <- data.frame(professor)
professor <- t(professor)
colnames(professor) <- c('Extraversion', 'Agreeableness', "Conscientiousness", 'Neuroticism', 'Openness')
professor <- data.frame(professor)

predict(fitted_model, professor, type = 'response')


## ============================= Exercise 3 ============================= ##
training <- read.delim2('ChallengeTrainingSet(1).txt')
training <- training[11:60]

## Remove problematic values
training <- na.omit(training)

## Calculate PCA
PCA = prcomp(training)

## PVE 
PVA_var = PCA$sdev^2
PVE = PVA_var/sum(PVA_var)
PVE


plot(PVE, xlab = 'Principal Component', ylab = 'Proportion of Var explained', ylim = c(0,1), type = 'b')

  
  
  