#Anomaly Detection - Assignment 1 - Stock EDA

library(quantmod)

getSymbols('TLRY')
head(TLRY)
TLRY <- rownames_to_column(data.frame(getSymbols("TLRY", auto.assign = FALSE)))
TLRY

TLRY2 <- rownames_to_column(data.frame(getSymbols("TLRY", auto.assign = FALSE)))

Stocks <- function(){
  require(quantmod)
  tilray <- rownames_to_column(data.frame(getSymbols("TLRY", auto.assign = FALSE)))
}

#From chris, applying sma into the dataset
TLRY_EMA10 <- EMA(TLRY$TLRY.Close, n=10)

head(TLRY)

ted2 <- df1 %>% group_by(sno,ratings_type) %>% 
  summarise(count_rating_type = sum(count)) %>% spread(ratings_type,count_rating_type) %>% ungroup() %>%
  inner_join(ted1,by = "sno")

occupation_by_rank <- df2 %>% group_by(occupation1) %>% summarise(n = n_distinct(sno)) %>% arrange(desc(n))

getSymbols('TSLA')
head(TSLA)
tail(TSLA)

chartSeries(TLRY)

#Adding percentage change
library(dplyr)
training_set2 <- training_set2%>%group_by(neighbourhood_cleansed)%>%mutate(neighclean=round(mean(price),0))%>%ungroup()

TLRY <- TLRY %>% mutate(chg=((TLRY.Close-TLRY.Open)/TLRY.Open))

str(TLRY)


#From the Medium article

# Custom R function as Data.
Stocks <- function(){
  require(quantmod)
  tilray <- rownames_to_column(data.frame(getSymbols("TLRY", auto.assign = FALSE)))
}

# Set libPaths.
.libPaths("/Users/kannishida/.exploratory/R/3.3")

# Load required packages.
library(geosphere)
#install.packages('geosphere')
library(zipcode)
#install.packages('zipcode')
library(rvest)
library(lubridate)
library(tidyr)
library(urltools)
#install.packages('urltools')
library(stringr)
library(broom)
library(RcppRoll)
#install.packages('tibble')
library(tibble)
#install.packages('dplyr')
#install.packages('zoo')
library(zoo)
library(dplyr)
library(quantmod)
#install.packages('Rtools')

# Data Analysis Steps
tail(TLRY)

TLRY <- TLRY %>%
  gather(measure, value, -rowname, na.rm=TRUE) %>%
  separate(measure, into=c("symbol", "measure"), sep="\\.") %>%
  spread(measure,value) %>%
  rename(date = rowname) %>%
  mutate(date = ymd(date)) %>%
  filter(date >= today() - years(2)) %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(growth = Adjusted - first(Adjusted), growth_percent = (Adjusted - first(Adjusted))/first(Adjusted)*100) %>%
  mutate(base_price = Adjusted[date  == "2018-07-19"], growth_from_base = Adjusted-base_price,
         growth_percent_from_base=growth_from_base/base_price*100) %>%
  mutate(moving_average = roll_mean(Adjusted, 50,align="right", fill=0)) %>%
  mutate(diff = Adjusted - lag(Adjusted), percent_diff = diff /lag(Adjusted) * 100) %>%
  filter(percent_diff > 10 | percent_diff < -10)

str(TLRY)
TLRY$symbol

#Growth from beggining
TLRY2 %>%
  mutate(growth = TLRY.Adjusted - first(TLRY.Adjusted), growth_percent = (TLRY.Adjusted - first(TLRY.Adjusted))/first(TLRY.Adjusted)*100)

#Growth per day
TLRY2 %>%
  mutate(pdchg = (TLRY.Adjusted - TLRY.Open)/TLRY.Open*100)

#Number of extreme days:
TLRY2 %>%
  mutate(prchg = TLRY.Adjusted - lag(TLRY.Adjusted), prcchg = prchg / lag(TLRY.Adjusted) * 100)%>%
  filter(!between(prchg, -10, 10))


