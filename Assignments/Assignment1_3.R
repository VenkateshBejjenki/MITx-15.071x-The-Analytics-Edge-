# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment1_1.R </p>
# *
# * <p> Description: An R script for Assignment 1.3 Demographics and Employment in the United States of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyright: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------

CPS = read.csv("CPSData.csv")

# 1.1 How many interviewees are in the dataset?
str(CPS)
# Ans: 131302

# 1.2 Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? Please enter the name exactly how you see it
sort(table(CPS$Industry))
# Ans: Educational and health services

# 1.3 Which state has the fewest interviewees?
sort(table(CPS$State))
# Ans: New Mexico

# Which state has the largest number of interviewees?
# Ans: California

# 1.4 What proportion of interviewees are citizens of the United States?

table(CPS$Citizenship)
# native + naturalized = 116639+7073 = 123712
# native + naturalized + non - citizen = 116639+7073+7590 = 131302
# Ans: Therfore, US citizens(interviewees) = 123712 / 131302 = 0.9421943

# 1.5 The CPS differentiates between race
# (with possible values American Indian, Asian, Black, Pacific Islander, White, or Multiracial) and ethnicity.
# A number of interviewees are of Hispanic ethnicity, as captured by the Hispanic variable.
# For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?

table(CPS$Race,CPS$Hispanic)
# Ans: American Indian, Black, Multiracial, White

# 2.1 Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(CPS)
# Ans: MetroAreaCode, Married, Education, EmploymentStatus, Industry


# 2.2 Often when evaluating a new dataset, we try to identify if there is a pattern in the missing values in the dataset.
# We will try to determine if there is a pattern in the missing values of the Married variable.
# The function is.na(CPS$Married) returns a vector of TRUE/FALSE values for whether the Married variable is missing.
# We can see the breakdown of whether Married is missing based on the reported value of the
# Region variable with the function table(CPS$Region, is.na(CPS$Married)). Which is the most accurate:

table(CPS$Region,is.na(CPS$Married))
table(CPS$Sex,is.na(CPS$Married))
table(CPS$Age,is.na(CPS$Married))
table(CPS$Citizenship,is.na(CPS$Married))

# Ans: The Married variable being missing is related to the Age value for the interviewee. (As the table of Age & Married has 0 for either of them at every data value)

# 2.3 How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)?
# For this question, treat the District of Columbia as a state (even though it is not technically a state).
table(CPS$State, is.na(CPS$MetroAreaCode))
# Ans: 2
# How many states had all interviewees living in a metropolitan area? Again, treat the District of Columbia as a state.
# Ans: 3

# 2.4 Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))
# Midwest = (10674/(10674+20010))*100 = 34.78686
# Northeast = (5609/(5609+20330))*100 = 21.62381
# South = (9871/(9871+31631))*100 = 23.7844
# West  = (8084/(8084+25093))*100 = 24.36628
# Ans: Midwest

# 2.5 Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean))
# Ans: Wisconsin
# Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all interviewees were non-metropolitan?
# Ans: Montana 83.607908%

MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap =  read.csv("CountryCodes.csv")

# 3.1 How many observations (codes for metropolitan areas) are there in MetroAreaMap?
str(MetroAreaMap)
# Ans: 271

# How many observations (codes for countries) are there in CountryMap?
str(CountryMap)
# Ans: 149

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

# 3.2 Review the new version of the CPS data frame with the summary() and str() functions. What is the name of the variable that was added to the data frame by the merge() operation?
str(CPS)
# Ans: MetroArea

# How many interviewees have a missing value for the new metropolitan area variable? Note that all of these interviewees would have been removed from the merged data frame if we did not include the all.x=TRUE parameter.
summary(CPS)
# Ans: 34238

# 3.3 Which of the following metropolitan areas has the largest number of interviewees?
sort(table(CPS$MetroArea))
# Ans: Boston-Cambridge-Quincy, MA-NH

# 3.4 Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
# Hint: Use tapply() with mean, as in the previous subproblem. Calling sort() on the output of tapply() could also be helpful here.
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
# Ans: Laredo, TX

# 3.5 Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether an interviewee is Asian,
# determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race == "Asian",CPS$MetroArea,mean))
# Ans: 4

# 3.6 Determine which metropolitan area has the smallest proportion of interviewees who have received no high school diploma.
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))
# Ans: Iowa City, IA

# 4.1 What is the name of the variable added to the CPS data frame by this merge operation?
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
str(CPS)
# Ans: Country

# How many interviewees have a missing value for the new country of birth variable?
summary(CPS)
# Ans: 176

# 4.2 Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Country))
# Ans: Philippines - 839

# 4.3 What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA"
# metropolitan area have a country of birth that is not the United States?
# For this computation, don't include people from this metropolitan area who have a missing country of birth.
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country!="United States")
# Ans: 1668/(3736+1668) = 0.3086603

# 4.4 Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India?
sort(tapply(CPS$Country=="India",CPS$MetroArea,sum,na.rm=TRUE))
# Ans: New York-Northern New Jersey-Long Island, NY-NJ-PA (96)

# In Brazil?
sort(tapply(CPS$Country=="Brazil",CPS$MetroArea,sum,na.rm=TRUE))
# Ans: Boston-Cambridge-Quincy, MA-NH (18)

# In Somalia?
sort(tapply(CPS$Country=="Somalia",CPS$MetroArea,sum,na.rm=TRUE))
# Ans: Minneapolis-St Paul-Bloomington, MN-WI (17)


