# * ----------------------------------------------------------------------------------
# * <p> Title: Assignment1_1.R </p>
# *
# * <p> Description: An R script for Assignment 1.2 Stock Dynamics of MITx: 15.071x The Analytics Edge </p>
# *
# * <p> Copyright: Venkatesh Bejjenki © 2017 </p>
# *
# * @author Venkatesh Bejjenki
# * ----------------------------------------------------------------------------------


IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")


# 1.1 Our five datasets all have the same number of observations. How many observations are there in each data set?
str(IBM)
# Ans: 480

# 1.2 What is the earliest year in our datasets?
summary(IBM$Date)
# Ans: 1970

# 1.3 What is the latest year in our datasets?
summary(IBM$Date)
# Ans: 2009

# 1.4 What is the mean stock price of IBM over this time period?
summary(IBM$StockPrice)
# Ans: 144.38

# 1.5 What is the minimum stock price of General Electric (GE) over this time period?
summary(GE$StockPrice)
# Ans: 9.294

# 1.6 What is the maximum stock price of Coca-Cola over this time period?
summary(CocaCola$StockPrice)
# Ans: 146.58

# 1.8 What is the median stock price of Boeing over this time period?
summary(Boeing$StockPrice)
# Ans: 44.88

# 1.9 What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)
# Ans: 18.19414

# 2.1 Around what year did Coca-Cola has its highest stock price in this time period?
jpeg("img1_2_2.1")
plot(CocaCola$Date, CocaCola$StockPrice,type="l")
dev.off()
# Ans: 1973
# Around what year did Coca-Cola has its lowest stock price in this time period?
# Ans: 1980

# 2.2 In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more?
jpeg("img1_2_2.2")
plot(CocaCola$Date, CocaCola$StockPrice,type="l",col="red",lty=2)
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)
dev.off()
# Ans: Procter and Gamble

# 2.3 Around 1983, the stock for one of these companies (Coca-Cola or Procter and Gamble) was going up, while the other was going down. Which one was going up?
# Ans: Coco_Cola

# In the time period shown in the plot, which stock generally has lower values?
# Ans: Coco_Cola

# 3.1 Which stock fell the most right after the technology bubble burst in March 2000?

jpeg("img1_2_3.1")
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432],IBM$StockPrice[301:432],col="blue")
lines(GE$Date[301:432],GE$StockPrice[301:432],col="green")
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432],col="purple")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],col="orange")
# Ans:GE

# 3.2 Which stock reaches the highest value in the time period 1995-2005?
#Ans: IBM

# 3.3 In October of 1997, there was a global stock market crash that was caused by an economic crisis in Asia. Comparing September 1997 to November 1997, which companies saw a decreasing trend in their stock price? (Select all that apply.)
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)
dev.off()
# Ans: Procter and Gamble & Boeing

# 3.4 In the last two years of this time period (2004 and 2005) which stock seems to be performing the best, in terms of increasing stock price?
# Ans: Boeing

# 4.1 For IBM, compare the monthly averages to the overall average stock price. In which months has IBM historically had a higher stock price (on average)? Select all that apply.
summary(IBM$StockPrice)
# =>mean =144.38
tapply(IBM$StockPrice, months(IBM$Date),mean)
# Ans: January, February, March, April, May

# 4.2 General Electric and Coca-Cola both have their highest average stock price in the same month. Which month is this?
tapply(GE$StockPrice, months(GE$Date),mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date),mean)
# Ans: April

# 4.3 For the months of December and January, every company's average stock is higher in one month and lower in the other. In which month are the stock prices lower?
# Ans: December
