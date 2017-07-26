# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment1_1.R </p>
# *
# * <p> Description: An R script for Assignment 1.1 An Analytical Detective of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyright: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------


MVR = read.csv("mvtWeek1.csv")
# 1.1 How many rows of data (observations) are in this dataset?
nrow(MVR)
#Ans: 191641

#1.2 How many variables are in this dataset?
ls()
#Ans: 9

# 1.3 Using the "max" function, what is the maximum value of the variable "ID"?
MVR$ID[which.max(MVR$ID)]
#Ans: 9181151

# 1.4 What is the minimum value of the variable "Beat"?
MVR$Beat[which.min(MVR$Beat)]
#Ans: 111

# 1.5 How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
table(MVR$Arrest)
# Ans: 15536

# 1.6 How many observations have a LocationDescription value of ALLEY?
nrow(subset(MVR,MVR$LocationDescription =="ALLEY"))
# Ans: 2308

# 2.1 In what format are the entries in the variable Date?
MVR$Date[1]
# Ans: Month/Day/Year Hour:Minute

# 2.2 What is the month and year of the median date in our dataset? Enter your answer as "Month Year", without the quotes.
        # (Ex: if the answer was 2008-03-28, you would give the answer "March 2008", without the quotes.)
DateConvert = as.Date(strptime(MVR$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
# Ans: May 2006

# 2.3 In which month did the fewest motor vehicle thefts occur?
MVR$Month = months(DateConvert)
MVR$Weekday = weekdays(DateConvert)
MVR$Date = DateConvert
table(MVR$Month)
# Ans: February

# 2.4 On which weekday did the most motor vehicle thefts occur?
table(MVR$Weekday)
# Ans: Friday

# 2.5 Each observation in the dataset represents a motor vehicle theft, and the Arrest variable indicates whether an arrest was later made for this theft. Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(MVR$Arrest,MVR$Month)
# Ans: January

# 3.1 First, let's make a histogram of the variable Date. We'll add an extra argument, to specify the number of bars we want in our histogram. In your R console, type
jpeg("img1_1_3.1")
hist(MVR$Date, breaks=100)
dev.off()
# Looking at the histogram, answer the following questions.

# In general, does it look like crime increases or decreases from 2002 - 2012?
# Ans: Decreases

# In general, does it look like crime increases or decreases from 2005 - 2008?
# Ans: Decreases

#In general, does it look like crime increases or decreases from 2009 - 2011?
#Ans: Increases

# 3.2 Does it look like there were more crimes for which arrests were made in the first half of the time period or the second half of the time period?
jpeg("img1_1_3.2")
boxplot(MVR$Date ~ MVR$Arrest)
dev.off()
# Ans: First half

# 3.3 For what proportion of motor vehicle thefts in 2001 was an arrest made?
table(MVR$Arrest,MVR$Year)
# Ans: 2152/(2152+18517) = 0.1041173

# 3.4 For what proportion of motor vehicle thefts in 2007 was an arrest made?
table(MVR$Arrest,MVR$Year)
# Ans: 1212/(1212+13068) = 0.08487395

# 3.5 For what proportion of motor vehicle thefts in 2012 was an arrest made?
table(MVR$Arrest,MVR$Year)
# Ans: 550/(550+13542) = 0.03902924

#4.1 Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category? You should select 5 of the following options.
sort(table(MVR$LocationDescription))
# Ans:  GAS STATION, STREET, PARKING LOT/GARAGE(NON.RESID.) , ALLEY, DRIVEWAY - RESIDENTIAL


# 4.2 How many observations are in Top5?

Top5 = subset(MVR,MVR$LocationDescription=="STREET"
        |MVR$LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)"
        |MVR$LocationDescription=="ALLEY"
        |MVR$LocationDescription=="GAS STATION"
        |MVR$LocationDescription=="DRIVEWAY - RESIDENTIAL")

nrow(Top5)
# Ans: 177510

# 4.3 One of the locations has a much higher arrest rate than the other locations. Which is it? Please enter the text in exactly the same way as how it looks in the answer options for Problem 4.1
table(Top5$LocationDescription, Top5$Arrest)
# Ans: STREET

# 4.4 On which day of the week do the most motor vehicle thefts at gas stations happen?
table(Top5$LocationDescription, Top5$Weekday)
# Ans: Saturday

# 4.5 On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
table(Top5$LocationDescription, Top5$Weekday)
# Ans: Saturday

