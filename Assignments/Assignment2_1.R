# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment2_1.R </p>
# *
# * <p> Description: An R script for Assignment 2.1 Climate Change of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyright: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------

climate = read.csv("climate_change.csv")
# str(climate)
train = subset(climate,Year <=2006)
test = subset(climate,Year > 2006)
climateModel = lm(Temp ~ MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data = train)

# 1.1 Enter the model R2 (the "Multiple R-squared" value):
summary(climateModel)
# Ans: 0.75

# 1.2 Which variables are significant in the model?
# Ans: MEI, CO2, CFC.11, CFC.12, TSI, Aerosols

# 2.1 Which of the following is the simplest correct explanation for this contradiction?
# Ans:  All of the gas concentration variables reflect human development - N2O and CFC.11 are correlated with other variables in the data set. correct

# 2.2 Compute the correlations between all the variables in the training set. Which of the following independent variables is N2O highly correlated with (absolute correlation greater than 0.7)? Select all that apply.
cor(train$N2O,train)
# Ans: CO2, CH4, CFC.12
# Which of the following independent variables is CFC.11 highly correlated with? Select all that apply.
cor(train$CFC.11,train)
# Ans: CH4, CFC.12


# Enter the coefficient of N2O in this reduced model:
simplifiedModel = lm(Temp ~ MEI+TSI+Aerosols+N2O,data = train)
summary(simplifiedModel)
# Ans: 2.532e-02

# Enter the model R2:
# Ans: 0.7261

# 4 Enter the R2 value of the model produced by the step function:
finalClimateModel = step(climateModel)
summary(finalClimateModel)
# Ans: 0.7508
# Which of the following variable(s) were eliminated from the full model by the step function? Select all that apply.
# Ans: CH4


# 5 Enter the testing set R2:
predictTest = predict(finalClimateModel, newdata = test)
SSE = sum((test$Temp - predictTest)^2)
SST = sum((test$Temp - mean(train$Temp))^2)
1-SSE/SST
# Ans: 0.6286051