# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment2_2.R </p>
# *
# * <p> Description: An R script for Assignment 2.2 Reading Test Scores of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyright: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------

pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

# 1.1 How many students are there in the training set?
nrow(pisaTrain)
# Ans: 3663

# 1.2 Using tapply() on pisaTrain, what is the average reading test score of males?
tapply( pisaTrain$readingScore,pisaTrain$male,mean)
# Ans: 483.5325
# Or females?
# Ans: 512.9406

# 1.3 Which variables are missing data in at least one observation in the training set? Select all that apply.
summary(pisaTrain)
# Ans: All except (publicSchool,urban,readingScore)

# 1.4 How many observations are now in the training set?
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
nrow(pisaTrain)
# Ans: 2414
# How many observations are now in the testing set?
nrow(pisaTest)
# Ans: 990

# 2.1 Which of the following variables is an unordered factor with at least 3 levels? (Select all that apply.)
summary(pisaTrain$grade)
table(pisaTrain$male)
table(pisaTrain$raceeth)
# Ans:raceeth
# Which of the following variables is an ordered factor with at least 3 levels? (Select all that apply.)
# Ans: grade

# 3.1 What is the Multiple R-squared value of lmScore on the training set?
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(pisaTrain$readingScore ~ .,data = pisaTrain)
summary(lmScore)
# Ans: 0.3251

# 3.2 What is the training-set root-mean squared error (RMSE) of lmScore?
SSE = sum(lmScore$residuals^2)
sqrt(SSE/nrow(pisaTrain))
# Ans: 73.36555

# 3.3 Consider two students A and B. They have all variable values the same, except that student A is in grade 11 and student B is in grade 9. What is the predicted reading score of student A minus the predicted reading score of student B?
# Ans: A and B have a difference in grade of 2, it predicts that A has a reading score 2*29.54(coeffcient) = 59.09

# 3.5 Based on the significance codes, which variables are candidates for removal from the model? Select all that apply. (We'll assume that the factor variable raceeth should only be removed if none of its levels are significant.)
summary(lmScore)
# Ans: remove all unsignificant variable

# 4.1 What is the range between the maximum and minimum predicted reading score on the test set?
predTest = predict(lmScore,newdata=pisaTest)
summary(predTest)
# Ans: 637.7-353.2 = 284.5 (max - min)

# 4.2 What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE = sum((predTest - pisaTest$readingScore)^2)
# Ans: 5762082

# What is the root-mean squared error (RMSE) of lmScore on the testing set?
RMSE = sqrt(SSE/nrow(pisaTest))
# Ans: 76.29079

# 4.3 What is the predicted test score used in the baseline model? Remember to compute this value using the training set and not the test set.
baseline = mean(pisaTrain$readingScore)
# Ans:517.9629

# What is the sum of squared errors of the baseline model on the testing set? HINT: We call the sum of squared errors for the baseline model the total sum of squares (SST).
SST = sum((baseline-pisaTest$readingScore)^2)
# Ans: 7802354

# 4.4 What is the test-set R-squared value of lmScore?
1-SSE/SST
# Ans: 0.2614944