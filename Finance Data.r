library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(dplyr)
library(ggplot2)

sensex=getSymbols("^BSESN",auto.assign = FALSE)

# How to get Yahoo Finance Symbol?
# https://finance.yahoo.com/

head(sensex)
tail(sensex)

head(Ad(sensex))
#functions like Op, Cl, Ad, Vo, Hi, Lo

#xts file
class(sensex)
head(index(sensex))
head(coredata(sensex))
head(data.frame(coredata(sensex)))

# xts = index + coredata 
# Why is xts better than normal time series data in R? 
# We can create an xts object by using xts(x,order.by) function. 
# x is the coredata and order.by is the index. 

candleChart(sensex) 
lineChart(sensex) 
barChart(sensex)

candleChart(sensex['2023'])

barChart(sensex['2023-05'])

barplot(sensex['2023-05']); abline(h=mean(sensex['2023-05']), col="red", lwd=2)


candleChart(sensex['2023/01-2023/04'])

addSMA(n=20)
addSMA(n=200)

#Check details
#https://www.quantmod.com/examples/charting/

#compute returns
# Two ways of computing returns: Arithmetic return and log return (continuous return)

# We can use the ROC function from TTR Package to compute return. 
args(ROC)

sensex_ret_ROC = ROC(Ad(sensex),type = 'discrete')
# We do not need to get the coredata first, as ROC can directly use xts data. 

head(sensex_ret_ROC) 

# Missing data is there. 
sensex_ret_ROC=sensex_ret_ROC[-1,]

head(sensex_ret_ROC)

# We can also use the Return.calculate() function from PerformanceAnalytics package to compute returns. 
head(Return.calculate(Ad(sensex),method='log'))

# How do we handle missing data?
sum(is.na(sensex))

# What to do with missing data?
# 1. Delete, 2. Extrapolate, 3. Retain 

sensex_missing_delete = na.exclude(sensex)

sensex_missing_extrapolate = na.approx(sensex)

dim(sensex_missing_delete)

dim(sensex_missing_extrapolate)

sensex_locf = na.locf(sensex)

# Let's perform a simple exercise. Let's find out if there is a day-of-week effect in India. 
head(sensex_ret_ROC)
sensex_df=data.frame(date=index(sensex_ret_ROC),return=coredata(sensex_ret_ROC))
head(sensex_df)

names(sensex_df)=c("date","returns")

sensex_df$dow=weekdays(sensex_df$date,abbreviate = F)

summary = sensex_df %>% 
  group_by(dow) %>% 
  summarise(mean_ret=mean(returns,na.rm=T)) 

summary=subset(summary,mean_ret!=0)

ggplot(summary,aes(x=dow,y=mean_ret))+
  geom_col(fill='blue')

# Homework: Find out if there is a month effect

#Obtaining foreign currency data

euro=getSymbols("USD/EUR",src='oanda',auto.assign = F)

# Visit https://www1.oanda.com/currency/converter/ to get the symbols
head(euro)
tail(euro)

inr=getSymbols("USD/INR",src='oanda',auto.assign = F)
head(inr)
tail(inr)
chartSeries(inr)

us_unemp = getSymbols('UNRATE',src='FRED',auto.assign = F)
head(us_unemp)
tail(us_unemp)

library(riingo)  # Data from Tiingo.com

ringo_secret = "a51df28500c24def8b3809ab1a91b40183241acd"
ringo_secret = "Put Your API Key Here"

riingo_set_token(ringo_secret)

bitcoin = riingo_crypto_prices("btcusd")
head(bitcoin,2)

library(alphavantager) #Data from https://www.alphavantage.co/support/#support
alpha_secret = "U81OO3W3WD8PRJ1B"

av_api_key(alpha_secret)
aapl = av_get('AAPL','TIME_SERIES_INTRADAY',interval='5min' )
head(aapl)

# Get google trends data
library(gtrendsR)

data("categories")

categories
# A data frame with 1782 rows and 2 variables
# id for "all categories" is 0. 

data("countries")
head(countries[countries$name=="INDIA",],1)

df = gtrends(keyword=c("GDP","Nifty"),geo="IN",time="now 7-d")

str(df) # list
head(df$interest_over_time)


unemp = gtrends("jobs",geo="IN",time="today+5-y")

str(unemp)

unemp$interest_by_region


# The following codes are not working as of now. May work in future.
library(rtweet)

library(httpuv)

auth_setup_default()

sensex_twitter = search_tweets("Sensex",n=1000)

head(sensex_twitter)

sensex_twitter$text[1]

