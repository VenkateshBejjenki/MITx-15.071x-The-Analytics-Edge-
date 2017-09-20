# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment4_2.R </p>
# *
# * <p> Description: An R script for Assignment 4.2 Letter Recognition of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyleft: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------

letters = read.csv("letters_ABPR.csv")
str(letters)
# 1.1 - Predicting B or not B

# Let's warm up by attempting to predict just whether a letter is B or not. To begin, load the file letters_ABPR.csv into R,
# and call it letters. Then, create a new variable isB in the dataframe, which takes the value "TRUE" if the observation corresponds to the
# letter B, and "FALSE" if it does not. You can do this by typing the following command into your R console:

letters$isB = as.factor(letters$letter == "B")

# Now split the data set into a training and testing set, putting 50% of the data in the training set. Set the seed to 1000 before making the split.
# The first argument to sample.split should be the dependent variable "letters$isB". Remember that TRUE values from sample.split should go in the
# training set.

# Before building models, let's consider a baseline method that always predicts the most frequent outcome, which is "not B".
# What is the accuracy of this baseline method on the test set?

# Ans: 1175+(1175+383) = 0.7541
# letters$letter = as.factor( letters$letter )
set.seed(1000)
spl = sample.split(letters$isB,SplitRatio=0.5)
train = subset(letters,spl==TRUE)
test = subset(letters,spl==FALSE)
table(test$isB)

# 1.2 - Predicting B or not B

# Now build a classification tree to predict whether a letter is a B or not, using the training set to build your model. Remember to remove the
# variable "letter" out of the model, as this is related to what we are trying to predict! To just remove one variable, you can either write out
# the other variables, or remember what we did in the Billboards problem in Week 3, and use the following notation:

CARTb = rpart(isB ~ . - letter, data=train, method="class")

# We are just using the default parameters in our CART model, so we don't need to add the minbucket or cp arguments at all.
# We also added the argument method="class" since this is a classification problem.

# What is the accuracy of the CART model on the test set? (Use type="class" when making predictions on the test set.)
# Ans: (1118+340)/nrow(test)
predictions = predict(CARTb, newdata=test, type="class")
table(test$isB,predictions)

# 1.3 - Predicting B or Not B

# Now, build a random forest model to predict whether the letter is a B or not (the isB variable) using the training set.
# You should use all of the other variables as independent variables, except letter (since it helped us define what we are trying to predict!).
# Use the default settings for ntree and nodesize (don't include these arguments at all). Right before building the model, set the seed to 1000.
# (NOTE: You might get a slightly different answer on this problem, even if you set the random seed. This has to do with your operating system and
# the implementation of the random forest algorithm.)
library(randomForest)
# What is the accuracy of the model on the test set?
# Ans: (1165+374)/nrow(test) = 0.9878049
set.seed(1000)
RFb = randomForest(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor, data=train)
RFb = randomForest(isB ~ . - letter, data=train)
predictions = predict(RFb, newdata=test)
table(test$isB,predictions)


# 2.1 - Predicting the letters A, B, P, R
# Let us now move on to the problem that we were originally interested in, which is to predict whether or not a letter is one of the
# four letters A, B, P or R.

# As we saw in the D2Hawkeye lecture, building a multiclass classification CART model in R is no harder than building the models for binary
# classification problems. Fortunately, building a random forest model is just as easy.

# The variable in our data frame which we will be trying to predict is "letter". Start by converting letter in the original data set
# (letters) to a factor by running the following command in R:

letters$letter = as.factor( letters$letter )

# Now, generate new training and testing sets of the letters data frame using letters$letter as the first input to the sample.split function.
# Before splitting, set your seed to 2000. Again put 50% of the data in the training set. (Why do we need to split the data again? Remember
# that sample.split balances the outcome variable in the training and testing sets. With a new outcome variable, we want to re-generate our split.)

# In a multiclass classification problem, a simple baseline model is to predict the most frequent class of all of the options.

# What is the baseline accuracy on the testing set?
# Ans: P got 401 in table(test2$letter).
# 401/nrow(test2) = 0.257381
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, spl == TRUE)
test2 = subset(letters, spl == FALSE)
table(train2$letter)
table(test2$letter)

# 2.2 - Predicting the letters A, B, P, R

# Now build a classification tree to predict "letter", using the training set to build your model. You should use all of the other variables as
# independent variables, except "isB", since it is related to what we are trying to predict! Just use the default parameters in your CART model.
# Add the argument method="class" since this is a classification problem. Even though we have multiple classes here, nothing changes in how we
# build the model from the binary case.

# What is the test set accuracy of your CART model? Use the argument type="class" when making predictions.

# (HINT: When you are computing the test set accuracy using the confusion matrix, you want to add everything on the main diagonal and divide by
# the total number of observations in the test set, which can be computed with nrow(test), where test is the name of your test set).
# Ans:(348+318+363+340)/nrow(test2) = 0.8786906
CARTletter = rpart(letter ~ . - isB, data=train2, method="class")
predictLetter = predict(CARTletter, newdata=test2, type="class")
table(test2$letter, predictLetter)


# 2.3 - Predicting the letters A, B, P, R
# Now build a random forest model on the training data, using the same independent variables as in the previous problem -- again,
# don't forget to remove the isB variable. Just use the default parameter values for ntree and nodesize (you don't need to include these
# arguments at all). Set the seed to 1000 right before building your model. (Remember that you might get a slightly different result even
# if you set the random seed.)

# What is the test set accuracy of your random forest model?
# Ans: (390+380 +393+364)/nrow(test2) = 0.9801027
set.seed(1000)
RFletter = randomForest(letter ~ . - isB, data=train2)
predictLetter = predict(RFletter, newdata=test2)
table(test2$letter, predictLetter)








