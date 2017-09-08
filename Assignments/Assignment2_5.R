# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment2_5.R </p>
# *
# * <p> Description: An R script for Assignment 2.5 Forecasting Elantra Sales (OPTIONAL) of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyright: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------


elantra = read.csv("elantra.csv")
ElantraTrain = subset(elantra,elantra$Year < 2013)
str(ElantraTrain)
ElantraTest = subset(elantra,elantra$Year > 2012)
str(ElantraTest)

# 1.1 How many observations are in the training set?
str(ElantraTrain)
# Ans: 36

# 2.1 Build a linear regression model to predict monthly Elantra sales using Unemployment, CPI_all, CPI_energy and Queries as the independent variables. Use all of the training set data to do this.
#What is the model R-squared? Note: In this problem, we will always be asking for the "Multiple R-Squared" of the model.
model1 =  lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all, data=ElantraTrain)
summary(model1)
# Ans: 0.4282

# 2.2 How many variables are significant, or have levels that are significant? Use 0.10 as your p-value cutoff.
summary(model1)
# Ans: 0

# 2.3 What is the coefficient of the Unemployment variable?
summary(model1)
# Ans: -3179.90

# 3.1 To incorporate the seasonal effect due to the month, build a new linear regression model that predicts monthly Elantra sales using Month as well as Unemployment, CPI_all, CPI_energy and Queries. Do not modify the training and testing data frames before building the model.What is the model R-Squared?
model2 =  lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data=ElantraTrain)
summary(model2)
# Ans: 0.4344

# 3.3 In the new model, given two monthly periods that are otherwise identical in Unemployment, CPI_all, CPI_energy and Queries, what is the absolute difference in predicted Elantra sales given that one period is in January and one is in March?
summary(model2)
# Ans = (coefficient of month)*(differnce between march and jan)  i.e.., coefficient of Month * (3-1)  3 for march and 1 for jan


# 4.1 Re-run the regression with the Month variable modeled as a factor variable. (Create a new variable that models the Month as a factor (using the as.factor function) instead of overwriting the current Month variable. We'll still use the numeric version of Month later in the problem.)What is the model R-Squared?

ElantraTrain$MonthFactor = as.factor(ElantraTrain$Month)
ElantraTest$MonthFactor = as.factor(ElantraTest$Month)
model3 =  lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthFactor, data=ElantraTrain)
summary(model3)
# Ans:

# 5.1 Which of the following variables is CPI_energy highly correlated with? Select all that apply. (Include only variables where the absolute value of the correlation exceeds 0.6.For the purpose of this question, treat Month as a numeric variable, not a factor variable.)

cor(ElantraTrain$CPI_energy,ElantraTrain$Month)
cor(ElantraTrain$CPI_energy,ElantraTrain$Unemployment)
cor(ElantraTrain$CPI_energy,ElantraTrain$Queries)
cor(ElantraTrain$CPI_energy,ElantraTrain$CPI_all)
# Ans: Unemployment correct, Queries correct, CPI_all correct

# 5.2 Which of the following variables is Queries highly correlated with? Again, compute the correlations on the training set. Select all that apply. (Include only variables where the absolute value of the correlation exceeds 0.6.For the purpose of this question, treat Month as a numeric variable, not a factor variable.)

cor(ElantraTrain$Queries,ElantraTrain$CPI_all)  #0.7536732
cor(ElantraTrain$Queries,ElantraTrain$Month)    #0.0158443
cor(ElantraTrain$Queries,ElantraTrain$Unemployment)  #-0.6411093
cor(ElantraTrain$Queries,ElantraTrain$CPI_energy)    #0.8328381
# Ans: Unemployment correct, CPI_energy correct, CPI_all correct

# 6.1  Let us now simplify our model (the model using the factor version of the Month variable). We will do this by iteratively removing variables, one at a time. Remove the variable with the highest p-value (i.e., the least statistically significant variable) from the model. Repeat this until there are no variables that are insignificant or variables for which all of the factor levels are insignificant. Use a threshold of 0.10 to determine whether a variable is significant.Which variables, and in what order, are removed by this process?

# if we consider model3 queries has more pvalue so first remove the queries from model
model4 =  lm(ElantraSales ~ Unemployment + CPI_energy + CPI_all + MonthFactor, data=ElantraTrain)
# Ans: Queries

# 6.2 Using the model from Problem 6.1, make predictions on the test set. What is the sum of squared errors of the model on the test set?
PredictTest = predict(model4, newdata = ElantraTest)
SSE = sum((PredictTest - ElantraTest$ElantraSales) ^ 2)
# Ans: 190757747

# 6.3  What would the baseline method predict for all observations in the test set?
#   Remember that the baseline method we use predicts the average outcome of all observations in the training set.
#The baseline is the mean of ElantraSales in the training set for every observation
mean(ElantraTrain$ElantraSales)
# Ans: 14462.25

# 6.4  What is the test set R-Squared?

SST = sum((mean(ElantraTrain$ElantraSales) - ElantraTest$ElantraSales)^2)
# Ans: 1-(SSE/SST) = 0.7280232

# 6.5 What is the largest absolute error that we make in our test set predictions?
max(abs(PredictTest - ElantraTest$ElantraSales))
# Ans: 7491.488

# 6.6 In which period (Month,Year pair) do we make the largest absolute error in our prediction?
which.max(abs(PredictTest - ElantraTest$ElantraSales))
# Ans: 03/2013



