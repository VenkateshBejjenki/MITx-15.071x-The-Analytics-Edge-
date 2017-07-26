# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment1_1.R </p>
# *
# * <p> Description: An R script for Assignment 1.4 Internet Privacy Poll of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyright: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------


poll = read.csv("AnonymityPoll.csv")
# 1.1 How many people participated in the poll?
str(poll)
# Ans: 1002

# 1.2
# How many interviewees responded that they use a smartphone?
table(poll$Smartphone)
# Ans: 487
# How many interviewees responded that they don't use a smartphone?
# Ans: 472
# How many interviewees did not respond to the question, resulting in a missing value, or NA, in the summary() output?
# Ans: 1002-(487+472) = 43

# 1.3 Which of the following are states in the Midwest census region? (Select all that apply.)
table(poll$State, poll$Region)
# Ans: Kansas, Missouri, Ohio
# Which was the state in the South census region with the largest number of interviewees?
table(poll$State, poll$Region=="South")
# Ans: Texas

# 2.1 How many interviewees reported not having used the Internet and not having used a smartphone?
table(poll$Internet.Use, poll$Smartphone)
# Ans: 186
# How many interviewees reported having used the Internet and having used a smartphone?
# Ans: 470
# How many interviewees reported having used the Internet but not having used a smartphone?
# Ans: 285
# How many interviewees reported having used a smartphone but not having used the Internet?
# Ans: 17

# 2.2 How many interviewees have a missing value for their Internet use?
summary(poll)
# Ans: 1
# How many interviewees have a missing value for their smartphone use?
# Ans: 43

# 2.3 How many interviewees are in the new data frame?
limited = subset(poll,poll$Internet.Use==TRUE|poll$Smartphone==TRUE)
# Ans: 792

# 3.1 Which variables have missing values in the limited data frame? (Select all that apply.)
summary(limited)
# Ans: Smartphone, Age, Conservativeness, Worry.About.Info, Privacy.Importance, Anonymity.Possible, Tried.Masking.Identity correct ,Privacy.Laws.Effective

# 3.2 What is the average number of pieces of personal information on the Internet, according to the Info.On.Internet variable?
summary(limited$Info.On.Internet)
# Ans: 3.795

# 3.3 How many interviewees reported a value of 0 for Info.On.Internet?
table(limited$Info.On.Internet)
# Ans: 105
# How many interviewees reported the maximum value of 11 for Info.On.Internet?
# Ans: 8

# 3.4 What proportion of interviewees who answered the Worry.About.
# Info question worry about how much information is available about them on the Internet?
table(limited$Worry.About.Info)
# Ans: 386/(404+386) = 0.4886076

# 3.5 What proportion of interviewees who answered the Anonymity.
# Possible question think it is possible to be completely anonymous on the Internet?
table(limited$Anonymity.Possible)
# Ans: 278/(475+278) = 0.3691899

# 3.6 What proportion of interviewees who answered the Tried.Masking.Identity question have tried masking their identity on the Internet?
table(limited$Tried.Masking.Identity)
# Ans: 128/(656+128) = 0.1632653

# 3.7 What proportion of interviewees who answered the Privacy.Laws.Effective question find United States privacy laws effective?
table(limited$Privacy.Laws.Effective)
# Ans: 186/(541+186) = 0.2558459

# 4.1 Build a histogram of the age of interviewees. What is the best represented age group in the population?
jpeg("img1_4_4.1")
hist(limited$Age)
dev.off()
# Ans: People aged about 60 years old

# 4.2 What is the largest number of interviewees that have exactly the same value in their Age variable AND the same value in their Info.On.Internet variable?
max(table(limited$Age, limited$Info.On.Internet))
# Ans: 6

# 4.3 To avoid points covering each other up, we can use the jitter() function on the values we pass to the plot function.
# Experimenting with the command jitter(c(1, 2, 3)), what appears to be the functionality of the jitter command?
jitter(c(1, 2, 3))
jitter(c(1, 2, 3))
# Ans: jitter adds or subtracts a small amount of random noise to the values passed to it, and two runs will yield different results correct

# 4.4 Now, plot Age against Info.On.Internet with plot(jitter(limited$Age), jitter(limited$Info.On.Internet)).
# What relationship to you observe between Age and Info.On.Internet?
jpeg("img1_4_4.4")
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
dev.off()
# Ans: Older age seems moderately associated with a smaller value for Info.On.Internet

# 4.5 What is the average Info.On.Internet value for smartphone users?
tapply(limited$Info.On.Internet,limited$Smartphone,summary)
# Ans: 4.368
# What is the average Info.On.Internet value for non-smartphone users?
# Ans: 2.923

# 4.6 What proportion of smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
tapply(limited$Tried.Masking.Identity,limited$Smartphone,summary)
# Ans: 0.1925
# What proportion of non-smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
# Ans: 0.1174
