# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment3_2.R </p>
# *
# * <p> Description: An R script for Assignment 3.2 Predicting Parole Violators of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyleft: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------



parole = read.csv("parole.csv")

# 1.1 How many parolees are contained in the dataset?
nrow(parole)
# Ans:675

# 1.2 How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)
# Ans: 78

# 2.1 Which variables in this dataset are unordered factors with at least three levels? Select all that apply.
# Ans: state, crime
# 2.2 - Preparing the Dataset

# How does the output of summary() change for a factor variable as compared to a numerical variable?
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole$state)
summary(parole$crime)
table(parole$state)
table(parole$crime)
# Ans: The output becomes similar to that of the table() function applied to that variable


# 3.1 Roughly what proportion of parolees have been allocated to the training and testing sets?
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole,split==TRUE)
test = subset(parole,split==FALSE)
str(train)
# Ans: 70% to the training set, 30% to the testing set

# 3.2 Now, suppose you re-ran lines [1]-[5] of Problem 3.1. What would you expect?
# Ans:The exact same training/testing set split as the first execution of [1]-[5]
# If you instead ONLY re-ran lines [3]-[5], what would you expect?
# Ans:A different training/testing set split from the first execution of [1]-[5]
# If you instead called set.seed() with a different number and then re-ran lines [3]-[5] of Problem 3.1, what would you expect?
# Ans:A different training/testing set split from the first execution of [1]-[5]

# 4.1 Using glm (and remembering the parameter family="binomial"), train a logistic regression model on the training set.
# Your dependent variable is "violator", and you should use all of the other variables as independent variables.

# What variables are significant in this model? Significant variables should have a least one star, or should have a
# probability less than 0.05 (the column Pr(>|z|) in the summary output). Select all that apply.

mod = glm(violator~., data=train, family="binomial")
summary(mod)
# Ans: race,state4, multiple.ofenses


# 4.2 - Building a Logistic Regression Model

# What can we say based on the coefficient of the multiple.offenses variable?

# The following two properties might be useful to you when answering this question:

# 1) If we have a coefficient c for a variable, then that means the log odds (or Logit) are increased by c for a unit increase in the variable.

# 2) If we have a coefficient c for a variable, then that means the odds are multiplied by e^c for a unit increase in the variable.
# Ans:Our model predicts that a parolee who committed multiple offenses has 5.01 times higher odds of being a violator than a parolee who did not commit multiple offenses but is otherwise identical.



# 4.3 According to the model, what are the odds this individual is a violator?
# log(odds) = -4.2411574 + 0.3869904*1 + 0.8867192*1 - 0.0001756*50 + 0.4433007*0 + 0.8349797*0 - 3.3967878*0 - 0.1238867*3 + 0.0802954*12 + 1.6119919*0 + 0.6837143*1 - 0.2781054*0 - 0.0117627*0 = -1.700629
exp(-1.700629)
# Ans: 0.1825687

# According to the model, what is the probability this individual is a violator?
1/(1+exp(1.700629))
# Ans: 0.1543831

# 5.1 What is the maximum predicted probability of a violation?
predictions = predict(mod, newdata=test, type="response")
summary(predictions)

# Ans:0.907


# 5.2 What is the model's sensitivity?
table(test$violator, as.numeric(predictions >= 0.5))
# Ans:0.522

# What is the model's specificity?
# Ans:0.933

# What is the model's accuracy?
# Ans:0.886


# 5.3 What is the accuracy of a simple model that predicts that every parolee is a non-violator?
table(test$violator)
# Ans: (179/202) = 0.886

# 5.4 Which of the following most likely describes their preferences and best course of action?
# Ans:The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5.


# 5.5 Which of the following is the most accurate assessment of the value of the logistic regression model with a cutoff 0.5 to a parole board,
# based on the model's accuracy as compared to the simple baseline model?
# Ans: The model is likely of value to the board, and using a different logistic regression cutoff is likely to improve the model's value.

# 5.6 Using the ROCR package, what is the AUC value for the model?
library(ROCR)
pred = prediction(predictions, test$violator)
as.numeric(performance(pred, "auc")@y.values)
# Ans: 0.8945



# 5.7 Describe the meaning of AUC in this context.
# Ans: The probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator.

# 6.1 How could we improve our dataset to best address selection bias?
# Ans: We should use a dataset tracking a group of parolees from the start of their parole until either they violated parole or they completed their term.

