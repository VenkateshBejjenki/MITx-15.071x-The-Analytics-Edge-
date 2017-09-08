# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment3_1.R </p>
# *
# * <p> Description: An R script for Assignment 3.1 Popularity of Music Records of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyleft: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------

songs = read.csv("songs.csv")


# 1.1 How many observations (songs) are from the year 2010?
table(songs$year)
# Ans: 373

# 1.2 How many songs does the dataset include for which the artist name is "Michael Jackson"?
ml = subset(songs, artistname == "Michael Jackson")
nrow(ml)
# Ans: 18

# 1.3 Which of these songs by Michael Jackson made it to the Top 10? Select all that apply.
ml$songtitle
ml$Top10[1]
ml$Top10[4]
# Ans: You Rock My World, You Are Not Alone

# 1.4The variable corresponding to the estimated time signature (timesignature) is discrete, meaning that it only takes integer values (0, 1, 2, 3, . . . ).
# What are the values of this variable that occur in our dataset? Select all that apply.
table(songs$timesignature)
# Ans: 0,1,3,4,5,7
# Which timesignature value is the most frequent among songs in our dataset?
# Ans: 4

# 1.5 Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
which.max(songs$tempo)
# Ans: 6206
songs$songtitle[6206]
# Ans: Wanna Be Startin' Somethin'



# We wish to predict whether or not a song will make it to the Top 10. To do this, first use the subset function to split
# the data into a SongsTraining set "SongsTrain" consisting of all the observations up to and including 2009 song releases,
# and a testing set "SongsTest", consisting of the 2010 song releases.

# 2.1 How many observations (songs) are in the SongsTraining set?
SongsTrain = subset(songs,year<=2009)
SongsTest = subset(songs,year==2010)
str(SongsTrain)
# Ans:  7201

# 2.2 Creating our Prediction Model

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)
# Looking at the summary of your model, what is the value of the Akaike Information Criterion (AIC)?
# Ans: 4827.2

# 2.3 What does the model suggest?
# Ans: The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10

# 2.4 What does Model 1 suggest in terms of complexity?
# Ans: Mainstream listeners tend to prefer less complex songs

# 2.5 By inspecting the coefficient of the variable "loudness", what does Model 1 suggest?
# Ans: Mainstream listeners prefer songs with heavy instrumentation

# By inspecting the coefficient of the variable "energy", do we draw the same conclusions as above?
# Ans: No


# 3.1 What is the correlation between the variables "loudness" and "energy" in the SongsTraining set?
cor(SongsTrain$loudness,SongsTrain$energy)
# Ans: 0.7399067

# 3.2 Create Model 2, which is Model 1 without the independent variable "loudness". This can be done with the following command:

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)

# Look at the summary of SongsLog2, and inspect the coefficient of the variable "energy". What do you observe?
# Ans:  Model 2 suggests that songs with high energy levels tend to be more popular. This contradicts our observation in Model 1.


# 3.3 Now, create Model 3, which should be exactly like Model 1, but without the variable "energy".
# Look at the summary of Model 3 and inspect the coefficient of the variable "loudness". Remembering that higher loudness and energy
# both occur in songs with heavier instrumentation, do we make the same observation about the popularity of heavy instrumentation as
# we did with Model 2?
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
# Ans:  Yes

# 4.1 - Validating Our Model
# Make predictions on the test set using Model 3. What is the accuracy of Model 3 on the test set, using a threshold of 0.45?
# (Compute the accuracy as a number between 0 and 1.)
testpredict = predict(SongsLog3,newdata=SongsTest,type="response")
table(SongsTest$Top10,testpredict>=0.45)
# Ans:  (309+19)/(309+19+40+5)

# 4.2 What would the accuracy of the baseline model be on the test set? (Give your answer as a number between 0 and 1.)

table(SongsTest$Top10)
# Ans: (314)/(314+59)


# 4.3 - How many songs does Model 3 correctly predict as Top 10 hits in 2010 (remember that all songs in 2010 went into our test set),
# using a threshold of 0.45?
# Ans: 19
# How many non-hit songs does Model 3 predict will be Top 10 hits (again, looking at the test set), using a threshold of 0.45?
# Ans: 5

# 4.4 - Validating Our Model
# What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?
table(SongsTest$Top10,testpredict>=0.45)
# Ans: 19/(19+40)

# What is the specificity of Model 3 on the test set, using a threshold of 0.45?
# Ans: (309)/(309+5)


# 4.5 What conclusions can you make about our model? (Select all that apply.)
# Ans:  a)Model 3 favors specificity over sensitivity.
# b) Model 3 provides conservative predictions, and predicts that a song will make it to the Top 10 very rarely.
# So while it detects less than half of the Top 10 songs, we can be very confident in the songs that it does predict to be Top 10 hits.
