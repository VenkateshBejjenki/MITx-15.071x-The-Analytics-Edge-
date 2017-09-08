# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment3_4.R </p>
# *
# * <p> Description: An R script for Assignment 3.4 Predicting the Baseball World Series Champion of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyleft: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------

baseball = read.csv("baseball.csv")

# 1.1 How many team/year pairs are there in the whole dataset?
nrow(baseball)
# Ans: 1232


# 1.2 Though the dataset contains data from 1962 until 2012, we removed several years with shorter-than-usual seasons. Using the table() function, identify the total number of years included in this dataset.
table(baseball$Year)
length(table(baseball$Year))
# Ans: 47


#1.3 Because we're only analyzing teams that made the playoffs, use the subset() function to replace baseball with a data frame limited to teams that made the playoffs (so your subsetted data frame should still be called "baseball"). How many team/year pairs are included in the new dataset?
baseball = subset(baseball, Playoffs == 1)
nrow(baseball)
# Ans: 244


#1.4 Through the years, different numbers of teams have been invited to the playoffs. Which of the following has been the number of teams making the playoffs in some season? Select all that apply.
table(baseball$Year)
# Ans: 2,4,8,10

#2.1 It's much harder to win the World Series if there are 10 teams competing for the championship versus just two. Therefore, we will add the predictor variable NumCompetitors to the baseball data frame. NumCompetitors will contain the number of total teams making the playoffs in the year of a particular team/year pair. For instance, NumCompetitors should be 2 for the 1962 New York Yankees, but it should be 8 for the 1998 Boston Red Sox.
PlayoffTable = table(baseball$Year)
str(names(PlayoffTable))
#  Ans: Vector of years stored as strings (type chr)

#2.2 Given a vector of names, the table will return a vector of frequencies. Which function call returns the number of playoff teams in 1990 and 2001? (HINT: If you are not sure how these commands work, go ahead and try them out in your R console!)
PlayoffTable[c("1990", "2001")]
# 1990 2001
#   4    8

#2.3 Putting it all together, we want to look up the number of teams in the playoffs for each team/year pair in the dataset, and store it as a new variable named NumCompetitors in the baseball data frame. While of the following function calls accomplishes this? (HINT: Test out the functions if you are not sure what they do.)
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

#2.4 Add the NumCompetitors variable to your baseball data frame. How many playoff team/year pairs are there in our dataset from years where 8 teams were invited to the playoffs?
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
nrow( baseball$NumCompetitors )
table(baseball$NumCompetitors)
# Ans: 128

#3.1In this problem, we seek to predict whether a team won the World Series; in our dataset this is denoted with a RankPlayoffs value of 1. Add a variable named WorldSeries to the baseball data frame, by typing the following command in your R console:
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)

#WorldSeries takes value 1 if a team won the World Series in the indicated year and a 0 otherwise. How many observations do we have in our dataset where a team did NOT win the World Series?
table(baseball$WorldSeries)
# Ans: 197

#3.2 When we're not sure which of our variables are useful in predicting a particular outcome, it's often helpful to build bivariate models, which are models that predict the outcome using a single independent variable. Which of the following variables is a significant predictor of the WorldSeries variable in a bivariate logistic regression model? To determine significance, remember to look at the stars in the summary output of the model. We'll define an independent variable as significant if there is at least one star at the end of the coefficients row for that variable (this is equivalent to the probability column having a value smaller than 0.05). Note that you have to build 12 models to answer this question! Use the entire dataset baseball to build the models. (Select all that apply.)
summary(glm(WorldSeries~Year, data=baseball, family="binomial"))
# Ans: W and SLG variables were both nearly significant, with p = 0.0577 and 0.0504, respectively.


#4.1 In this section, we'll consider multivariate models that combine the variables we found to be significant in bivariate models. Build a model using all of the variables that you found to be significant in the bivariate models. How many variables are significant in the combined model?
LogModel = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data=baseball, family=binomial)
# Looking at summary(LogModel)
# Ans: 0

#4.2 Often, variables that were significant in bivariate models are no longer significant in multivariate analysis due to correlation between the variables. Which of the following variable pairs have a high degree of correlation (a correlation greater than 0.8 or less than -0.8)? Select all that apply.
cor(baseball$Year, baseball$RA)
# Ans: 0.4762422
cor(baseball[c(“Year”, “RA”, “RankSeason”, “NumCompetitors”)])
# Year/NumCompetitors


#4.3 Build all six of the two variable models listed in the previous problem. Together with the four bivariate models, you should have 10 different logistic regression models. Which model has the best AIC value (the minimum AIC value)?


Model1 = glm(WorldSeries ~ Year + RA, data=baseball, family=binomial)
Model2 = glm(WorldSeries ~ Year + RankSeason, data=baseball, family=binomial)
Model3 = glm(WorldSeries ~ Year + NumCompetitors, data=baseball, family=binomial)
Model4 = glm(WorldSeries ~ RA + RankSeason, data=baseball, family=binomial)
Model5 = glm(WorldSeries ~ RA + NumCompetitors, data=baseball, family=binomial)
Model6 = glm(WorldSeries ~ RankSeason + NumCompetitors, data=baseball, family=binomial)

# Ans: Numcompeitetors




