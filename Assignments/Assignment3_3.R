# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment3_3.R </p>
# *
# * <p> Description: An R script for Assignment 3.3 Predicting Loan Repayment of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyleft: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------

loans=read.csv("loans.csv")

#1.What proportion of the loans in the dataset were not paid in full? Please input a number between 0 and 1.
str(loans)
table(loans$not.fully.paid)
1533/(1533+8045)
#Ans: 0.1600543

#1.2 Which of the following variables has at least one missing observation? Select all that apply
summary(loans)
# we can read that log.annual.inc, days.with.cr.line, revol.util, inq.last.6mths, delinq.2yrs and pub.rec are missing values.

#1.3.Which of the following is the best reason to fill in the missing values for these variables instead of removing observations with missing data? (Hint: you can use the subset() function to build a data frame with the observations missing at least one value. To test if a variable, for example pub.rec, is missing a value, use is.na(pub.rec).)
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
table(missing$not.fully.paid)
#Ans: We want to be able to predict risk for all borrowers, instead of just the ones with all data reported.

#1.4 What best describes the process we just used to handle missing values?
loansimputed=read.csv("loans_imputed.csv")
# install.packages("mice")
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
# Ans: We predicted missing variable values using the available independent variables for each observation.

#2.1 Which independent variables are significant in our model? (Significant variables have at least one star, or a Pr(>|z|) value less than 0.05.) Select all that apply.
library(caTools)
set.seed(144)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)
mod = glm(not.fully.paid~., data=train, family="binomial")
summary(mod)
# Ans:
#credit.policy ,purpose2 (credit card),purpose3 (debt consolidation),purpose6 (major purchase),
#purpose7 (small business),installment,log.annual.inc,fico,revol.bal,inq.last.6mths,
#pub.rec


#2.2 Consider two loan applications, which are identical other than the fact that the borrower in Application A has FICO credit score 700 while the borrower in Application B has FICO credit score 710.

# Let Logit(A) be the log odds of loan A not being paid back in full, according to our logistic regression model, and define Logit(B) similarly for loan B. What is the value of Logit(A) - Logit(B)?

#Because Application A is identical to Application B other than having a FICO score 10 lower, its predicted log odds differ by -0.009317 * -10 = 0.09317 from the predicted log odds of Application B.
#Now, let O(A) be the odds of loan A not being paid back in full, according to our logistic regression model, and define O(B) similarly for loan B. What is the value of O(A)/O(B)? (HINT: Use the mathematical rule that exp(A + B + C) = exp(A)*exp(B)*exp(C). Also, remember that exp() is the exponential function in R.)
exp(0.09317)
# Ans: 1.097648

#2.3 Predict the probability of the test set loans not being paid back in full (remember type="response" for the predict function). Store these predicted probabilities in a variable named predicted.risk and add it to your test set (we will use this variable in later parts of the problem). Compute the confusion matrix using a threshold of 0.5.
#What is the accuracy of the baseline model? Input the accuracy as a number between 0 and 1.
test$predicted.risk = predict(mod, newdata=test, type="response")
table(test$not.fully.paid, test$predicted.risk > 0.5)
# Ans:2413/2873=0.8399
# What is the accuracy of the baseline model? Input the accuracy as a number between 0 and 1.
# Ans: 2403/2873=0.8364


#Use the ROCR package to compute the test set AUC.
#The model has poor accuracy at the threshold 0.5. But despite the poor accuracy, we will see later how an investor can still leverage this logistic regression model to make profitable investments.

library(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

#3.1 In the previous problem, we built a logistic regression model that has an AUC significantly higher than the AUC of 0.5 that would be obtained by randomly ordering observations.
# However, LendingClub.com assigns the interest rate to a loan based on their estimate of that loan's risk. This variable, int.rate, is an independent variable in our dataset. In this part, we will investigate using the loan's interest rate as a "smart baseline" to order the loans according to risk.
# Using the training set, build a bivariate logistic regression model (aka a logistic regression model with a single independent variable) that predicts the dependent variable not.fully.paid using only the variable int.rate.
# The variable int.rate is highly significant in the bivariate model, but it is not significant at the 0.05 level in the model trained with all the independent variables. What is the most likely explanation for this difference?
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)
# Ans: int.rate is correlated with other risk-related variables, and therefore does not incrementally improve the model when those other variables are included. correct

#3.2 Make test set predictions for the bivariate model. What is the highest predicted probability of a loan not being paid in full on the testing set?
pred.bivariate = predict(bivariate, newdata=test, type="response")
summary(pred.bivariate)
# Ans: 0.4266
# With a logistic regression cutoff of 0.5, how many loans would be predicted as not being paid in full on the testing set?
# Ans: 0

# 3.3 What is the test set AUC of the bivariate model?
prediction.bivariate = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(prediction.bivariate, "auc")@y.values)
# Ans: 0.624

# 4.1 How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding of interest? Hint: remember to convert the percentage to a proportion before doing the math. Enter the number of dollars, without the $ sign.
10*exp(0.06*3)
# Ans: 11.97.

# 4.2 While the investment has value c * exp(rt) dollars after collecting interest, the investor had to pay $c for the investment. What is the profit to the investor if the investment is paid back in full?
# c * exp(rt) - c

# 4.3 Now, consider the case where the investor made a $c investment, but it was not paid back in full. Assume, conservatively, that no money was received from the borrower (often a lender will receive some but not all of the value of the loan, making this a pessimistic assumption of how much is received). What is the profit to the investor in this scenario?
# Ans: -c

#5.1 In the previous subproblem, we concluded that an investor who invested c dollars in a loan with interest rate r for t years makes c * (exp(rt) - 1) dollars of profit if the loan is paid back in full and -c dollars of profit if the loan is not paid back in full (pessimistically).
#In order to evaluate the quality of an investment strategy, we need to compute this profit for each loan in the test set. For this variable, we will assume a $1 investment (aka c=1). To create the variable, we first assign to the profit for a fully paid loan, exp(rt)-1, to every observation, and we then replace this value with -1 in the cases where the loan was not paid in full. All the loans in our dataset are 3-year loans, meaning t=3 in our calculations. Enter the following commands in your R console to create this new variable:
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
#What is the maximum profit of a $10 investment in any loan in the testing set (do not include the $ sign in your answer)?
summary(test$profit)
# Ans: 8.895

#6.1A simple investment strategy of equally investing in all the loans would yield profit $20.94 for a $100 investment. But this simple investment strategy does not leverage the prediction model we built earlier in this problem. As stated earlier, investors seek loans that balance reward with risk, in that they simultaneously have high interest rates and a low risk of not being paid back.
#To meet this objective, we will analyze an investment strategy in which the investor only purchases loans with a high interest rate (a rate of at least 15%), but amongst these loans selects the ones with the lowest predicted risk of not being paid back in full. We will model an investor who invests $1 in each of the most promising 100 loans.
#First, use the subset() function to build a data frame called highInterest consisting of the test set loans with an interest rate of at least 15%.
#What is the average profit of a $1 investment in one of these high-interest loans (do not include the $ sign in your answer)?
highInterest = subset(test, int.rate >= 0.15)
summary(highInterest$profit)
# Ans :0.2251.

#What proportion of the high-interest loans were not paid back in full?
table(highInterest$not.fully.paid)
# Ans:0.2517.

#6.2 Next, we will determine the 100th smallest predicted probability of not paying in full by sorting the predicted risks in increasing order and selecting the 100th element of this sorted list. Find the highest predicted risk that we will include by typing the following command into your R console:
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
#Use the subset() function to build a data frame called selectedLoans consisting of the high-interest loans with predicted risk not exceeding the cutoff we just computed. Check to make sure you have selected 100 loans for investment.
#What is the profit of the investor, who invested $1 in each of these 100 loans (do not include the $ sign in your answer)?
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
# Ans: 32.87361
# How many of 100 selected loans were not paid back in full?
table(selectedLoans$not.fully.paid)
#Ans: 18