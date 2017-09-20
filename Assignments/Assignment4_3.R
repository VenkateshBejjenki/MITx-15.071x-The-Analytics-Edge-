# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment4_3.R </p>
# *
# * <p> Description: An R script for Assignment 4.3 Predicting Earnings from Census Data of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyleft: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------

census = read.csv("census.csv")

# 1.1 - A Logistic Regression Model

# Let's begin by building a logistic regression model to predict whether an individual's earnings are above $50,000 (the variable "over50k")
# using all of the other variables as independent variables. First, read the dataset census.csv into R.

# Then, split the data randomly into a training set and a testing set, setting the seed to 2000 before creating the split. Split the data so
# that the training set contains 60% of the observations, while the testing set contains 40% of the observations.

# Next, build a logistic regression model to predict the dependent variable "over50k", using all of the other variables in the dataset as
# independent variables. Use the training set to build the model.

# Which variables are significant, or have factors that are significant? (Use 0.1 as your significance threshold, so variables with a period
# or dot in the stars column should be counted too. You might see a warning message here - you can ignore it and proceed. This message is a
# warning that we might be overfitting our model to the training set.) Select all that apply.

library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)
censusglm = glm( over50k ~ . , family="binomial", data = train)

summary(censusglm)

# Ans:

# 1.2 - A Logistic Regression Model

# What is the accuracy of the model on the testing set? Use a threshold of 0.5. (You might see a warning message when you make predictions
# on the test set - you can safely ignore it.)
predictTest = predict(censusglm, newdata = test, type = "response")
table(test$over50k, predictTest >= 0.5)

# Ans: (9051+1888)/(9051+662+1190+1888) = 0.8552107

# 1.3 - A Logistic Regression Model

# What is the baseline accuracy for the testing set?
table(train$over50k)
table(test$over50k)
# Ans: 9713/(9713+3078) = 0.7593621.


# 1.4 - A Logistic Regression Model

# What is the area-under-the-curve (AUC) for this model on the test set?
library(ROCR)
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
# Ans: 0.9061598

# 2.1 - A CART Model

# We have just seen how the logistic regression model for this data achieves a high accuracy. Moreover, the significances of the variables
# give us a way to gauge which variables are relevant for this prediction task. However, it is not immediately clear which variables are more
# important than the others, especially due to the large number of factor variables in this problem.

# Let us now build a classification tree to predict "over50k". Use the training set to build the model, and all of the other variables as
# independent variables. Use the default parameters, so don't set a value for minbucket or cp. Remember to specify method="class" as an argument
# to rpart, since this is a classification problem. After you are done building the model, plot the resulting tree.

# How many splits does the tree have in total?
> library(rpart)
> library(rpart.plot)
> censustree = rpart( over50k ~ . , method="class", data = train)
> prp(censustree)
# Ans:

# 2.2 - A CART Model

# Which variable does the tree split on at the first level (the very first split of the tree)?
prp(censustree)
# Ans: relationship

# 2.3 - A CART Model

# Which variables does the tree split on at the second level (immediately after the first split of the tree)? Select all that apply.
prp(censustree)
# Ans:education, capitalgain

# 2.4 - A CART Model

# What is the accuracy of the model on the testing set? Use a threshold of 0.5. (You can either add the argument type="class",
# or generate probabilities and use a threshold of 0.5 like in logistic regression.)
predictTest = predict(censustree, newdata = test, type = "class")
table(test$over50k, predictTest)
# Ans:(9243+1596)/(9243+470+1482+1596) = 0.8473927

# 2.5 - A CART Model
# 1 point possible (graded)
# Let us now consider the ROC curve and AUC for the CART model on the test set. You will need to get predicted probabilities for the
# observations in the test set to build the ROC curve and compute the AUC. Remember that you can do this by removing the type="class"
# argument when making predictions, and taking the second column of the resulting object.

# Plot the ROC curve for the CART model you have estimated. Observe that compared to the logistic regression ROC curve, the CART ROC
# curve is less smooth than the logistic regression ROC curve. Which of the following explanations for this behavior is most correct?
# (HINT: Think about what the ROC curve is plotting and what changing the threshold does.)

# Ans:  The probabilities from the CART model take only a handful of values (five, one for each end bucket/leaf of the tree); the changes
#       in the ROC curve correspond to setting the threshold to one of those values.


# 2.6 - A CART Model

# What is the AUC of the CART model on the test set?
library(ROCR)
predictTest = predict(censustree, newdata = test)
predictTest = predictTest[,2]
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Ans: 0.8470256

# 3.1 - A Random Forest Model

# Before building a random forest model, we'll down-sample our training set. While some modern personal computers can build a random
# forest model on the entire training set, others might run out of memory when trying to train the model since random forests is much
# more computationally intensive than CART or Logistic Regression. For this reason, before continuing we will define a new training set
# to be used when building our random forest model, that contains 2000 randomly selected obervations from the original training set.
# Do this by running the following commands in your R console (assuming your training set is called "train"):

set.seed(1)

trainSmall = train[sample(nrow(train), 2000), ]

# Let us now build a random forest model to predict "over50k", using the dataset "trainSmall" as the data used to build the model.
# Set the seed to 1 again right before building the model, and use all of the other variables in the dataset as independent variables.
# (If you get an error that random forest "can not handle categorical predictors with more than 32 categories", re-build the model without
# the nativecountry variable as one of the independent variables.)

# Then, make predictions using this model on the entire test set. What is the accuracy of the model on the test set, using a threshold of 0.5?
# (Remember that you don't need a "type" argument when making predictions with a random forest model if you want to use a threshold of 0.5.
# Also, note that your accuracy might be different from the one reported here, since random forest models can still differ depending on
# your operating system, even when the random seed is set. )
set.seed(1)
censusrf = randomForest(over50k ~ . , data = trainSmall)
predictTest = predict(censusrf, newdata=test)
table(test$over50k, predictTest)
(9614+1050)/nrow(test)
# Ans:= 0.8337112


# 3.2 - A Random Forest Model

# As we discussed in lecture, random forest models work by building a large collection of trees. As a result, we lose some of the
# interpretability that comes with CART in terms of seeing how predictions are made and which variables are important. However,
# we can still compute metrics that give us insight into which variables are important.

# One metric that we can look at is the number of times, aggregated over all of the trees in the random forest model, that a certain variable is
# selected for a split. To view this metric, run the following lines of R code (replace "MODEL" with the name of your random forest model):

# vu = varUsed(MODEL, count=TRUE)

# vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

# dotchart(vusorted$x, names(MODEL$forest$xlevels[vusorted$ix]))

# This code produces a chart that for each variable measures the number of times that variable was selected for splitting
# (the value on the x-axis). Which of the following variables is the most important in terms of the number of splits?
vu = varUsed(censusrf, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(censusrf$forest$xlevels[vusorted$ix]))

# Ans: age

# 3.3 - A Random Forest Model

# A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is.
# In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. Therefore, one way to
# measure the importance of a variable is to average the reduction in impurity, taken over all the times that variable is selected for
# splitting in all of the trees in the forest. To compute this metric, run the following command in R
# (replace "MODEL" with the name of your random forest model):

# varImpPlot(MODEL)

# Which one of the following variables is the most important in terms of mean reduction in impurity?
varImpPlot(censusrf)
# Ans:occupation

# 4.1 - Selecting cp by Cross-Validation

# We now conclude our study of this data set by looking at how CART behaves with different choices of its parameters.

# Let us select the cp parameter for our CART model using k-fold cross validation, with k = 10 folds. Do this by using the train function.
# Set the seed beforehand to 2. Test cp values from 0.002 to 0.1 in 0.002 increments, by using the following command:

# cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

# Also, remember to use the entire training set "train" when building this model. The train function might take some time to run.

# Which value of cp does the train function recommend?
library(caret)
set.seed(2)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train( over50k ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

# Ans:Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was cp = 0.002.

# 4.2 - Selecting cp by Cross-Validation

# Fit a CART model to the training data using this value of cp. What is the prediction accuracy on the test set?
model = rpart(over50k~., data=train, method="class", cp=0.002)
predictTest = predict(model, newdata=test, type="class")
table(test$over50k, predictTest)
(9178+1838)/(9178+535+1240+1838)
# Ans: 0.8612306


# 4.3 - Selecting cp by Cross-Validation

# Compared to the original accuracy using the default value of cp, this new CART model is an improvement, and so we should clearly favor this new model over the old one -- or should we? Plot the CART tree for this model. How many splits are there?
prp(model)
# Ans: 18.
