# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment2_4.R </p>
# *
# * <p> Description: An R script for Assignment 2.4 State Data  of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyright: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------

state = read.csv("statedata.csv")
str(state)
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)


# 1.1 In the R command you used to generate this plot, which variable name did you use as the first argument?
plot(statedata$x, statedata$y)
# Ans:  statedata$x

# 1.2 Using the tapply command, determine which region of the US (West, North Central, South, or Northeast)has the highest average high school graduation rate of all the states in the region:
tapply(statedata$HS.Grad,statedata$state.region,mean)
# Ans: West

# 1.3 Now, make a boxplot of the murder rate by region (for more information about creating boxplots in R, type ?boxplot in your console).Which region has the highest median murder rate?
boxplot(statedata$Murder ~ statedata$state.region)
# Ans: South

# 1.4 You should see that there is an outlier in the Northeast region of the boxplot you just generated. Which state does this correspond to?
area = subset(statedata,statedata$state.region == "Northeast")
boxplot(area$Murder ~ area$state.abb)
# Ans: New York

# 2.1 Build the model with all potential variables included (Population, Income, Illiteracy, Murder, HS.Grad, Frost, and Area). Note that you should use the variable "Area" in your model, NOT the variable "state.area".What is the coefficient for "Income" in your linear regression model?
model1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area , data = state)
summary(model1)
# Ans: -0.0000218

# 2.3 Now plot a graph of life expectancy vs. income using the command:
plot(statedata$Income, statedata$Life.Exp)
# Ans: Life expectancy is somewhat positively correlated with income.

# 3.1 You should be able to find a good model with only 4 independent variables, instead of the original 7. Which variables does this model contain?
model1 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost , data = state)
# Ans:  Population, Murder, Frost, HS.Grad

# 3.3 Which state do we predict to have the lowest life expectancy? (Hint: use the sort function)
sort(predict(model1))
statedata$state.name [1]
# Ans: Alabama

# Which state actually has the lowest life expectancy? (Hint: use the which.min function)
statedata$state.name [which.min(statedata$Life.Exp)]
# Ans: South Carolina

# 3.4 Which state do we predict to have the highest life expectancy?
sort(predict(model1))
statedata$state.name [47]
# Ans: Washington

# Which state actually has the highest life expectancy?
statedata$state.name [which.max(statedata$Life.Exp)]
# Ans: Hawaii

# 3.5 Take a look at the vector of residuals (the difference between the predicted and actual values).
# For which state do we make the smallest absolute error?
sort(abs(model1$residuals))
sort(abs(statedata$Life.Exp - predict(model1)))
# Ans: Indiana

# For which state do we make the largest absolute error?
# Ans: Hawaii