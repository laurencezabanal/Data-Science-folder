#Decision Tree
#install.packages('rpart')
# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset= dataset[2:3]


# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
#library(caTools)
#set.seed(123)
#split = sample.split(dataset$Salary, SplitRatio = 2/3)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)

#Feature Scaling
#training_set = scale(training_set)
#test_set = scale(test_set)
#This package will not need manual feature scaling


# Fitting the Regression Model to the dataset
library(rpart)
regressor = rpart(formula = Salary ~ .,
               data = dataset, 
               control = rpart.control(minsplit = 1))
#~ means is proportional
#check and input "summary(regressor)" in the console

# Create your regressor here

# Predicting a new result
y_pred = predict(regressor,data.frame(Level=6.5))


# Visualising the Regression Model results (High resolution for non linear, non continuous model)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level),max(dataset$Level),0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level=x_grid))),
            colour = 'blue') +
  ggtitle('Salary vs Level (Decision Tree Model)') +
  xlab('Level') +
  ylab('Salary')
