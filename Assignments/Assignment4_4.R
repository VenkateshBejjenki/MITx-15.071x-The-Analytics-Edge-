# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment4_4.R </p>
# *
# * <p> Description: An R script for Assignment 4.4 State Data Revisted of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyleft: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------

# 1.1 - Linear Regression Models

# Let's recreate the linear regression models we made in the previous homework question. First, predict Life.Exp using all of the other
# variables as the independent variables (Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area ). Use the entire dataset to build the model.

# What is the adjusted R-squared of the model?
# Ans: 0.6922
statedata = read.csv("statedataSimple.csv")
RegModel = lm(Life.Exp ~ ., data=statedata)
summary(RegModel)


# 1.2 - Linear Regression Models

# Calculate the sum of squared errors (SSE) between the predicted life expectancies using this model and the actual life expectancies:
# Ans: 23.29
Predictions = predict(RegModel)
sum((statedata$Life.Exp - Predictions)^2)
# [1] 23.29714
sum(RegModel$residuals^2)

# 1.3 - Linear Regression Models

# Build a second linear regression model using just Population, Murder, Frost, and HS.Grad as independent variables
# (the best 4 variable model from the previous homework). What is the adjusted R-squared for this model?
# Ans:0.712
RegModel2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=statedata)
summary(RegModel2)


# 1.4 - Linear Regression Models

# Calculate the sum of squared errors again, using this reduced model:
# Ans: 23.30804
Predictions2 = predict(RegModel2)
sum((statedata$Life.Exp - Predictions2)^2)
SSE = sum(RegModel2$residuals^2)
SSE

# 1.5 - Linear Regression Models

# Which of the following is correct?
# Ans: Trying different combinations of variables in linear regression is like trying different numbers of splits
# in a tree - this controls the complexity of the model
cor(statedata$Life.Exp, statedata$Income)
cor(statedata$Life.Exp, statedata$Illiteracy)
cor(statedata$Life.Exp, statedata$Area)


# 2.1 - CART Models

# Let's now build a CART model to predict Life.Exp using all of the other variables as independent variables
# (Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area). We'll use the default minbucket parameter, so don't add the minbucket argument.
# Remember that in this problem we are not as interested in predicting life expectancies for new observations as we are understanding how they
# relate to the other variables we have, so we'll use all of the data to build our model. You shouldn't use the method="class" argument since
# this is a regression tree.
library(rpart)
library(rpart.plot)
# Plot the tree. Which of these variables appear in the tree? Select all that apply.
# Ans: Murder.
CARTmodel = rpart(Life.Exp ~ ., data=statedata)
prp(CARTmodel)


# 2.2 - CART Models

# Use the regression tree you just built to predict life expectancies (using the predict function), and calculate the sum-of-squared-errors (SSE)
# like you did for linear regression. What is the SSE?
# Ans: 28.998
PredictionsCART = predict(CARTmodel)
sum((statedata$Life.Exp - PredictionsCART)^2)

# 2.3 - CART Models

# The error is higher than for the linear regression models. One reason might be that we haven't made the tree big enough.
# Set the minbucket parameter to 5, and recreate the tree.

# Which variables appear in this new tree? Select all that apply.
# Ans: Murder, H.S.Grad, Area
CARTmodel2 = rpart(Life.Exp ~ ., data=statedata, minbucket=5)
prp(CARTmodel2)

# 2.4 - CART Models

# Do you think the default minbucket parameter is smaller or larger than 5 based on the tree that was built?
# Ans: Larger

# 2.5 - CART Models

# What is the SSE of this tree?
# Ans: 23.6
PredictionsCART2 = predict(CARTmodel)
sum((statedata$Life.Exp - PredictionsCART2)^2)


# 2.6 - CART Models

# Can we do even better? Create a tree that predicts Life.Exp using only Area, with the minbucket parameter to 1.
# What is the SSE of this newest tree?

# Ans: 9.31
CARTmodel3 = rpart(Life.Exp ~ Area, data=statedata, minbucket=1)
PredictionsCART3 = predict(CARTmodel3)
sum((statedata$Life.Exp - PredictionsCART3)^2)

# 2.7 - CART Models

# This is the lowest error we have seen so far. What would be the best interpretation of this result?
# Ans: We can build almost perfect models given the right parameters, even if they violate our intuition of what a
# good model should be. correct

# 3.1 - Cross-validation

# Adjusting the variables included in a linear regression model is a form of model tuning. In Problem 1 we showed that by removing variables in
# our linear regression model (tuning the model), we were able to maintain the fit of the model while using a simpler model. A rule of thumb
# is that simpler models are more interpretable and generalizeable. We will now tune our regression tree to see if we can improve the fit of
# our tree while keeping it as simple as possible.

# Load the caret library, and set the seed to 111. Set up the controls exactly like we did in the lecture (10-fold cross-validation) with cp
# varying over the range 0.01 to 0.50 in increments of 0.01. Use the train function to determine the best cp value for a CART model using all
# of the available independent variables, and the entire dataset statedata. What value of cp does the train function recommend?
# (Remember that the train function tells you to pick the largest value of cp with the lowest error when there are ties, and explains this
# at the bottom of the output.)
# Ans: 0.12 cp value.
library(caret)
set.seed(111)
fitControl = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01) )
train(Life.Exp ~ ., data=statedata, method="rpart", trControl = fitControl, tuneGrid = cartGrid)


# 3.2 - Cross-Validation

# Create a tree with the value of cp you found in the previous problem, all of the available independent variables, and the entire dataset
# "statedata" as the training data. Then plot the tree. You'll notice that this is actually quite similar to the first tree we created with
# the initial model. Interpret the tree: we predict the life expectancy to be 70 if the murder rate is greater than or equal to

# Ans: 6.6
CARTmodel4 = rpart(Life.Exp ~ ., data=statedata, cp=0.12)
prp(CARTmodel4)
# and is less than

# Ans:11

# 3.3 - Cross-Validation

# Calculate the SSE of this tree:
# Ans: 32.86
PredictionsCART4 = predict(CARTmodel4)
sum((statedata$Life.Exp - PredictionsCART4)^2)

# 3.4 - Cross-Validation

# Recall the first tree (default parameters), second tree (minbucket = 5), and the third tree (selected with cross validation) we made.
# Given what you have learned about cross-validation, which of the three models would you expect to be better if we did use it for prediction
# on a test set? For this question, suppose we had actually set aside a few observations (states) in a test set, and we want to make predictions
# on those states.
# Ans: The model we just made with the "best" cp correct


# 3.5 - Cross-Validation

# At the end of Problem 2 we made a very complex tree using just Area. Use train with the same parameters as before but just using Area as an
# independent variable to find the best cp value (set the seed to 111 first). Then build a new tree using just Area and this value of cp.

# How many splits does the tree have?
# Ans:  4 splits.
set.seed(111)
train(Life.Exp ~ Area, data=statedata, method="rpart", trControl = fitControl, tuneGrid = cartGrid )
CARTmodel5 = rpart(Life.Exp ~ Area, data=statedata, cp=0.02)
prp(CARTmodel5)

# 3.6 - Cross-Validation

# The lower left leaf (or bucket) corresponds to the lowest predicted Life.Exp of 70. Observations in this leaf correspond to states with area
# greater than or equal to

# Ans: 9579

# and area less than

# Ans: 51000


# 3.7 - Cross-Validation

# We have simplified the previous "Area tree" considerably by using cross-validation. Calculate the SSE of the cross-validated "Area tree",
# and select all of the following correct statements that apply:
# Ans: The Area variable is not as predictive as Murder rate.
PredictionsCART5 = predict(CARTmodel5)
sum((statedata$Life.Exp - PredictionsCART5)^2)
