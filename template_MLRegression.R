# Regression Template

# Importing the dataset
dataset = read.csv('50_Startups.csv')
#dataset = dataset[,-4]
dataset$State = factor(dataset$State,
                       levels = c('New York','California','Florida'),
                       labels = c(1,2,3))
# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)
#This package will not need manual feature scaling


# Fitting the Regression Model to the dataset
regressor = lm(formula = Profit ~ .,
                data = training_set)



#Building the optimal model using backward elimination
#Manual Break down for backward elimination
#Sprint 1
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
#to get complete info to get the variables that are stat significant we use the whole dataset and not training set

summary(regressor)

#Sprint 2
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend + State,
               data = dataset)
#to get complete info to get the variables that are stat significant we use the whole dataset and not training set

summary(regressor)


#Sprint 3
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
#to get complete info to get the variables that are stat significant we use the whole dataset and not training set

summary(regressor)



#Automatic implementation of Backward Elimination in R
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)




# Predicting a new result
y_pred = predict(regressor,newdata = test_set)

# Visualising the Regression Model results (Test set)
