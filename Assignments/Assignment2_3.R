# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment2_3.R </p>
# *
# * <p> Description: An R script for Assignment 2.3 Detecting Flu Epidemics via Search Engine Query Data  of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyright: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------

FluTrain = read.csv("FluTrain.csv")
# 1.1 which week corresponds to the highest percentage of ILI-related physician visits? Select the day of the month corresponding to the start of this week.
which.max(FluTrain$ILI)
 # 303
FluTrain$Week[303]
# Ans: 2009-10-18 - 2009-10-24

# 1.2 Let us now understand the data at an aggregate level. Plot the histogram of the dependent variable, ILI. What best describes the distribution of values of ILI?
hist(FluTrain$ILI)
# Ans:  Most of the ILI values are small, with a relatively small number of much larger values (in statistics, this sort of data is called "skew right").

# 1.3 Plot the natural logarithm of ILI versus Queries. What does the plot suggest?.
plot(log(FluTrain$ILI), FluTrain$Queries)
# Ans: There is a positive, linear relationship between log(ILI) and Queries.

# 2.1  Based on the plot we just made, it seems that a linear regression model could be a good modeling choice. Based on our understanding of the data from the previous subproblem, which model best describes our estimation problem?
#Ans: log(ILI) = intercept + coefficient x Queries, where the coefficient is positive

# 2.2 What is the training set R-squared value for FluTrend1 model (the "Multiple R-squared")1?
FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)
# Ans: 0.709

# 2.3 For a single variable linear regression model, there is a direct relationship between the R-squared and the correlation between the independent and the dependent variables. What is the relationship we infer from our problem?
Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
Correlation^2
# = 0.7090201
log(1/Correlation)
 # = 0.1719357
exp(-0.5*Correlation)
# = 0.6563792
# Ans: R-squared = Correlation^2 correct

FluTest = read.csv("FluTest.csv")
PredTest1 = predict(FluTrend1, newdata=FluTest)
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
# 3.1 What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012? (HINT: You can either just output FluTest$Week to find which element corresponds to March 11, 2012, or you can use the "which" function in R. To learn more about the which function, type ?which in your R console.)
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]
# Ans: 2.187383

# 3.2 What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012? Note that the relative error is calculated as
# (2.293422 - 2.187378)/2.293422 = 0.04624

# 3.3 What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the percentage of ILI-related physician visits, on the test set?
SSE = sum((PredTest1-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
# Ans: 0.7490645

# 4.1 How many values are missing in the new ILILag2 variable?
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)
# Ans: 2

# 4.2 Use the plot() function to plot the log of ILILag2 against the log of ILI. Which best describes the relationship between these two variables?
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
# Ans: This is a weak or no relationship between log(ILILag2) and log(ILI)

# 4.3 Which coefficients are significant at the p=0.05 level in this regression model? (Select all that apply.)
FluTrend2 = lm(log(ILI)~Queries+log(ILILag2), data=FluTrain)
summary(FluTrend2)
# Ans: 0.9063

# 4.4 On the basis of R-squared value and significance of coefficients, which statement is the most accurate?
# Ans: FluTrend2 is a stronger model than FluTrend1 on the training set.

# 5.1 Modify the code from the previous subproblem to add an ILILag2 variable to the FluTest data frame. How many missing values are there in this new variable?
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)
Ans: 2
