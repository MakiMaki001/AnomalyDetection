setwd('C:\\Users\\marko\\OneDrive\\Documents\\Columbia\\Anomaly Detection - Summer 2019')

#Assignment 3

The Office of Management and Enterprise Services in the State of Oklahoma has made its [purchase credit card transactions](https://catalog.data.gov/dataset/purchase-card-pcard-fiscal-year-2014) available. This dataset contains information on purchases made through the purchase card programs administered by the state and higher education institutions. 

In this assignment, you will complete the following tasks.

- Create new features and conduct exploratory data analysis. Your exploratory data analysis should be as exhaustive as possible. 
- Each feature or discussion is a new lead. Structure your EDA for different leads with sub-sections. Each sub-section will cover the following:
  - Lead: Write what you are going to do in two to three sentences.
- Analysis: your EDA
- Conclusion: What is the business insight? How can this feature help prediction? Write a short conclusion in the end of each sub-section. 

-  Submit in the HTML format.  

library(dplyr)
#install.packages('DataExplorer')
library(DataExplorer)
#install.packages('xda')
library(xda)
library(ggplot2)
#install.packages('plotly')
library(plotly)
library(lubridate)
library(anytime)
library(DT)

#Variables created: 
#Time Difference categorical
#Time difference numeric
#Full name
#Average purchase per transaction
#Grouping of metchant and vendor categories

ccard <- read.csv("purchase_credit_card.csv")
str(ccard)
summary(ccard)
colnames(ccard)
getwd()

#From class file: 

# Count of agencies
# Spent by agency
# Count by merchant.Category.Code

colnames(ccard)<-c('Year_Month', 'Agency_Number', 'Agency_Name', 'Cardholder_Last_Name',
                   'Cardholder_First_Initial', 'Description', 'Amount', 'Vendor', 'Transaction_Date',
                   'Posted_Date', 'Merchant_Category')
nrow(ccard)
head(ccard)

##### Simple Bar Plots 

#By Agency Names
stat_by_agency <- ccard %>% group_by(Agency_Name) %>%
  summarise(Count = n(),
            Amount = sum(Amount/1000),
            mean = mean(Amount/1000),
            min = min(Amount/1000),
            max = max(Amount/1000)
  ) %>%
  arrange(desc(Amount/1000)) %>% ungroup() 

stat_by_agency

stat_by_agency1 <- stat_by_agency %>%
  mutate(row = rep(1:nrow(stat_by_agency)),
         Agency_Name_ind = paste(row,Agency_Name,sep="_"),
         percent = Amount/sum(Amount)) %>%
  select(Agency_Name_ind,Count, Amount, percent,mean, min, max)

head(stat_by_agency)
agency_temp <-stat_by_agency[1:10,]
agency_temp1 <- stat_by_agency1[1:10,]

barplot(agency_temp$Amount,names.arg=agency_temp$Agency_Name,
        main="Amount by agency name",las=2       )

barplot(agency_temp1$Count,names.arg=agency_temp1$Agency_Name_ind,
        main="Count by agency name",las=2       )

#By Merchant Category
stat_by_mc <- ccard %>% group_by(Merchant_Category) %>%
  summarise(Count = n(),
            Amount = sum(Amount/1000),
            mean = mean(Amount/1000),
            min = min(Amount/1000),
            max = max(Amount/1000)
  ) %>%
  arrange(desc(Amount)) %>% ungroup() 

stat_by_mc

stat_by_mc1 <- stat_by_mc %>%
  mutate(row = rep(1:nrow(stat_by_mc)),
         Merchant_Cat_ind = paste(row,Merchant_Category,sep="_"),
         percent = Amount/sum(Amount)) %>%
        dplyr::arrange(Count)%>%
  select(Merchant_Cat_ind,Count, Amount, percent,mean, min, max)

head(stat_by_mc)
summary(stat_by_mc)
stat_by_mc

#Chart count
mc_temp <-stat_by_mc[1:10,]
mc_temp1 <-stat_by_mc[1:10,]

barplot(mc_temp1$Count,names.arg= mc_temp1$Merchant_Category,
        main="Count by Merchant Category",las=2       )

#Chart amount
barplot(mc_temp$Amount,names.arg=mc_temp$Merchant_Category,
        main="Amount by Merchant Category",las=2       )

#Create Timestamp
time_by_agency <- ccard %>% group_by(Agency_Name) %>%
  mutate(Transaction_Date=as.Date(Transaction_Date,format="%m/%d/%Y %H:%M")) %>%
  arrange(Agency_Name,Transaction_Date) %>%
  mutate(time = Transaction_Date-lag(Transaction_Date)) 
time_by_agency

time_by_agency$time

time_by_agency[,c("Agency_Number","Agency_Name", "Transaction_Date","Vendor", 
                  "time")]

#Frequency
time_by_agency %>% filter(Agency_Number ==26500) %>% 
  group_by(Vendor,Merchant_Category)

time_by_agency

#End of class file

#From class: 

#One visualization which I should use is variable importance plot (look up)

str(ccard)
levels(ccard$Agency_Name)

########Applying RFM Analysis to the dataset: 

####Putting all of them together: 

df_RFM <- ccard %>% 
  group_by(Agency_Name, Transaction_Date) %>% 
  summarise(
              frequency =n_distinct(Transaction_Date), monetary= sum(Amount))
df_RFM

#####Recency

#Simplify transaction date value - since all transactions at 12:00 will remove those. 
ccard$Transaction_Date <- anydate(ccard$Transaction_Date)
summary(ccard$Transaction_Date)
max(ccard$Transaction_Date)
min(ccard$Transaction_Date)

stat_by_time_count <- ccard %>% group_by(Transaction_Date) %>%
  summarise(Count = n(),
            Amount = sum(Amount/1000)
  ) %>%
  arrange(desc(Amount)) %>% ungroup() 
stat_by_time_count

str(ccar)

barplot(stat_by_time_count$Count,names.arg=stat_by_time_count$Transaction_Date,
        main="Count of Transactions through time",las=2       )

barplot(stat_by_time_count$Amount,names.arg=stat_by_time_count$Transaction_Date,
        main="Amount through time",las=2       )

##Two anomalies
#8/21 and 7/19 in 2013
max821 <- ccard[ccard$Transaction_Date == '2013-08-21',]
ma719 <- ccard[ccard$Transaction_Date == '2013-07-19',]

levels(ccard$Agency_Name)

max821 %>% arrange(desc(Amount))
ma719 %>% arrange(desc(Amount))

levels(ccard$Transaction_Date)
ccard$Transaction_Date
#Categorized Transaction Column that did work
#One variable- Categorized transactions based on the quartile of the last transaction dates (4- most recent)
r_score <- ifelse(ccard$Transaction_Date < '2013-09-25', 1, 
                  ifelse(ccard$Transaction_Date > '2013-09-26' & 
                           ccard$Transaction_Date <"2014-01-06", 2, 
                         ifelse(ccard$Transaction_Date > '2014-01-07' & 
                                  ccard$Transaction_Date < "2014-04-02", 3, 4)))
ccard <- cbind(ccard, r_score)

#Getting the difference in time
ccard <- ccard %>% 
  mutate(TD = difftime('2014-06-30', ccard$Transaction_Date, units="days")) #Last date in the dataset (6/30) and subtracting from date
summary(ccard$TD)

####Frequency 

##From class file frequency
head(ccard$Agency_Number)
time_by_agency %>% filter(Agency_Number ==26500) %>% 
  group_by(Vendor,Merchant_Category)

time_by_agency

time_by_agency %>% filter(Agency_Number ==1000) %>% 
  group_by(Vendor,Merchant_Category)

time_by_agency %>% group_by(Agency_Number) %>% 
  group_by(Vendor,Merchant_Category)


### Frequency by Merchant Category

head(dplyr::arrange(ccard, Agency_Name, Merchant_Category))
head(dplyr::arrange(ccard, Merchant_Category))

summary(stat_by_mc)

stat_by_mc %>% dplyr::arrange(desc(Amount), Count)
stat_by_mc %>% dplyr::arrange(desc(Amount),desc(Count))
summary(stat_by_mc$count)

#Chart
stat_by_mc_count <- stat_by_mc %>% dplyr::arrange(desc(Count))
stat_by_mc_count
stat_by_mc_count_temp <- stat_by_mc_count[1:5,]
ggplot(stat_by_mc_count_temp,aes(x = Merchant_Category,y = Count,colour = max)) + 
  geom_bar(stat="identity", fill="blue") + 
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

stat_by_mc_amount <- stat_by_mc %>% dplyr::arrange(desc(Amount), Count)
stat_by_mc_amount_temp <- stat_by_mc_amount[1:5,]
ggplot(stat_by_mc_amount_temp,aes(x = Merchant_Category,y = Amount,colour = max)) + 
  geom_point(stat="identity") + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

stat_by_mc_amount
stat_by_mc_count

#Seeing merchant category by quarter of transaction date
str(ccard)
ccard1 <- ccard %>%
  select(Agency_Name, Vendor, Transaction_Date, Merchant_Category, Amount, r_score)

ccard1 %>% filter(r_score ==1) %>% 
  group_by(Merchant_Category, Vendor, Agency_Name)%>%
  arrange(desc(Amount/1000))

ccard1 %>% filter(r_score ==2) %>% 
  group_by(Merchant_Category, Vendor, Agency_Name)%>%
  arrange(desc(Amount/1000))

ccard1 %>% filter(r_score ==3) %>% 
  group_by(Merchant_Category, Vendor, Agency_Name)%>%
  arrange(desc(Amount/1000))

ccard1 %>% filter(r_score ==4) %>% 
  group_by(Merchant_Category, Vendor, Agency_Name)%>%
  arrange(desc(Amount/1000))

#Need to do an f_lookup to align count number to the merchant category- all merchant categories should have the same number
stat_by_mc
f_score <- ifelse(ccard$Transaction_Date < '2013-09-25', 1, 
                  ifelse(ccard$Transaction_Date > '2013-09-26' & 
                           ccard$Transaction_Date <"2014-01-06", 2, 
                         ifelse(ccard$Transaction_Date > '2014-01-07' & 
                                  ccard$Transaction_Date < "2014-04-02", 3, 4)))

MC <- as.numeric(" ")
ccard2 <- cbind(ccard,MC)
lookupdf <- aggregate(MC~ Merchant_Category,ccard,count)
scoringData2$neighclean <- lookup_e(scoringData2$neighbourhood_cleansed,lookupdf[,c(1:2)])

### Frequncy by Name 
levels(ccard$Cardholder_Last_Name)

#Combining last name with first initial
stat_by_lastname <- ccard %>% group_by(Cardholder_Last_Name) %>%
  summarise(Count = n(),
            Amount = sum(ccard$Amount/1000),
            mean = mean(ccard$Amount/1000),
            min = min(ccard$Amount/1000),
            max = max(ccard$Amount/1000)
  ) %>%
  arrange(desc(Count))%>% ungroup()

lastname_temp <-stat_by_lastname[1:10,]
ggplot(lastname_temp,aes(x = Cardholder_Last_Name,y = Count)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

Full_Name <-paste0(as.character(ccard$Cardholder_First_Initial)," ", as.character(ccard$Cardholder_Last_Name))

ccard <- cbind(ccard, Full_Name)

stat_by_name <- ccard %>% group_by(Full_Name) %>%
  summarise(Count = n(),
            Amount = sum(ccard$Amount/1000),
            mean = mean(ccard$Amount/1000),
            min = min(ccard$Amount/1000),
            max = max(ccard$Amount/1000)
  ) %>%
  arrange(desc(Count))%>% ungroup()

stat_by_name
name_temp <-stat_by_name[1:10,]
ggplot(name_temp,aes(x = Full_Name,y = Count)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

stat_by_name1 <- stat_by_name %>%
  mutate(row = rep(1:nrow(stat_by_name)),
         Name = paste(row,Full_Name,sep="_"),
         percent = Amount/sum(Amount)) %>%
  select(Name, Count, Amount, percent,mean, min, max)

name_temp1 <-stat_by_name1[1:15,]
barplot(name_temp1$Count,names.arg=name_temp1$Name,
        main="Count by Name",las=2       )

ggplot(name_temp1,aes(x = Name,y = Count)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

#Looking at name per quarter of date purchased
ccard2 <- ccard %>%
  select(Full_Name, Vendor, Merchant_Category, Agency_Name, Amount, r_score)

ccard2 %>% filter(r_score ==1) %>% 
  group_by(Full_Name)%>%
  arrange(desc(Amount/1000))

ccard2 %>% filter(r_score ==2) %>% 
  group_by(Full_Name)%>%
  arrange(desc(Amount/1000))

ccard2 %>% filter(r_score ==3) %>% 
  group_by(Full_Name)%>%
  arrange(desc(Amount/1000))

ccard2 %>% filter(r_score ==4) %>% 
  group_by(Full_Name)%>%
  arrange(desc(Amount/1000))

###Monetary- Vendor
stat_by_vendor <- ccard %>% group_by(Vendor) %>%
  summarise(Count = n(),
            Amount = sum(Amount/1000),
            mean = mean(Amount/1000),
            min = min(Amount/1000),
            max = max(Amount/1000)
  ) %>%
  arrange(desc(Amount)) %>% ungroup() 
stat_by_vendor

#Chart of top 5 Vendors 
vendor_temp1<- stat_by_vendor[1:5,]
ggplot(vendor_temp1,aes(x = Vendor,y = Amount)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))


stat_by_vendor2 <- ccard %>% group_by(Vendor) %>%
  summarise(Count = n(),
            Amount = sum(Amount/1000)
  ) %>%
  arrange(desc(Count)) %>% ungroup()
stat_by_vendor2
vendor_temp2<- stat_by_vendor2[1:5,]
ggplot(vendor_temp2,aes(x = Vendor,y = Count)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

#Adding variable amount per transactions (Not counted in 1000's)
stat_by_vendor3 <- ccard %>% group_by(Vendor) %>%
  summarise(Count = n(),
            Amount = sum(Amount)
  ) %>%
  mutate(avg_Purchase = Amount/Count)%>%
  arrange(desc(avg_Purchase)) %>% ungroup()

stat_by_vendor3
vendor_temp3<- stat_by_vendor3[1:5,]
ggplot(vendor_temp3,aes(x = Vendor,y = avg_Purchase)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

#Grouping by Vendor and Merhcant
stat_by_vendor4 <- ccard %>% group_by(Vendor, Agency_Name) %>%
  summarise(Count = n(),
            Amount = sum(Amount)
  ) %>%
  mutate(avg_Purchase = Amount/Count)%>% ungroup()
stat_by_vendor4

#Vendor and Merchant by count  
stat_by_vendor4_Count <- stat_by_vendor4 %>% arrange(desc(Count))
stat_by_vendor4_Count
count_temp <- stat_by_vendor4_Count[1:5,]
ggplot(count_temp,aes(x = Vendor,y = Count, fill = Agency_Name)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  ggtitle('Count by Vendors With Merchant Grouping')

#Vendor and Merchant by Amount
stat_by_vendor4_Amount <- stat_by_vendor4 %>% arrange(desc(Amount))
stat_by_vendor4_Amount
amount_temp <- stat_by_vendor4_Amount[1:5,]
ggplot(amount_temp,aes(x = Vendor,y = Amount, fill = Agency_Name)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  ggtitle('Amount by Vendors with Merchant Grouping')

#Average amount by Vendor and Merchant Grouping
stat_by_vendor4_AvgPur <- stat_by_vendor4 %>% arrange(desc(avg_Purchase))
stat_by_vendor4_AvgPur
avgpur_temp <- stat_by_vendor4_AvgPur[1:5,]
ggplot(avgpur_temp,aes(x = Vendor, y = avg_Purchase, fill = Agency_Name)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  ggtitle('Average per purchase by Vendors with Merchant Grouping')

##Put monetary amount into quartiles and score them

#Do an absolute chart of amount per agency 
str(ccard)

###Putting all RFM Together

#Could also use library(lubridate) instead of anydate

df_RFM <- ccard %>% 
  group_by(Agency_Name) %>% 
  summarise(recency = 
              frequency =n_distinct(Transaction_Date), monetary= sum(Amount))

##Charts

ggplot(ccard, aes(x = Transaction_Date, y = Amount, Fill = Merchant_Category)) + 
  geom_point()

#####Assignment 3 Redo
#Feedback from professor on assignment 3: 
#- Good reading summary 
#- An agency buys from various merchant categories. It will be more insightful to compare at an agency and merchant category level. 
#For example, assume an agency usually buys gasoline every other day and office supplies every month. 
#The purchase frequency of gasoline and office supplies together is not as meaningful as the purchase frequency of gasoline, 
#and the frequency of office supplies separately. 
#- Let's try to understand the actor of fraud - it is an agency. If you compare across vendors, can it really detect which agency is a fraudster? 
#- You did look at (Merchant_Category, Vendor, Agency_Name) level. Can it really give insights to agency fraud?

str(ccard)
library(anytime)

colnames(ccard)<-c('Year_Month', 'Agency_Number', 'Agency_Name', 'Cardholder_Last_Name',
                   'Cardholder_First_Initial', 'Description', 'Amount', 'Vendor', 'Transaction_Date',
                   'Posted_Date', 'Merchant_Category')

#Adding Agency and Merchang Category as a Variable in initial dataset
ccard <- ccard %>% mutate(AgencyMerch = paste(Agency_Name, 
                                    Merchant_Category, sep="| & |"))

#By Agency Names
stat_by_agentmerch <- ccard %>% mutate(AgencyMerch = paste(Agency_Name, 
                                  Merchant_Category, sep="| & |")) %>% 
        group_by(AgencyMerch) %>%
         summarise(Count = n(),
                   Amt = sum(Amount/1000),
                   mean = mean(Amount/1000),
                   min = min(Amount/1000),
                   max = max(Amount/1000)
         ) %>%
         arrange(desc(Amt)) %>% ungroup() 
stat_by_agentmerch

#Apply binning to the time factor and get the frequency of the types of transcations

ccard$Transaction_Date <- anydate(ccard$Transaction_Date)
summary(ccard$Transaction_Date)
max(ccard$Transaction_Date)
min(ccard$Transaction_Date)

stat_by_time_count <- ccard %>% group_by(Transaction_Date) %>%
  summarise(Count = n(),
            Amount = sum(Amount/1000)
  ) %>%
  arrange(Transaction_Date) %>% ungroup() 

diff_time_ag = Transaction_Date-lag(Transaction_Date)

barplot(stat_by_time_count$Count,names.arg=stat_by_time_count$Transaction_Date,
        main="Count of Transactions through time",las=2       )

barplot(stat_by_time_count$Amount,names.arg=stat_by_time_count$Transaction_Date,
        main="Amount through time",las=2       )

#Adding differences in dates to identify ranges in purchases made

ccard<- ccard %>% mutate(TransDateMonth=Transaction_Date-30,
                         TransDateBiMonth=Transaction_Date-15,
                         TransDateWeek=Transaction_Date-7)
rm(ccard$AgencyMerchSet)
#Adding columns showing count of transactions by Monthly, BiMohthly, and Weekly 
AgencyMerchSet <- ccard[,c('Agency_Number', 'Amount','Transaction_Date','TransDateMonth',
                           'TransDateWeek','AgencyMerch')]

AgencyMerch_Count <- AgencyMerchSet %>% dplyr::group_by(Agency_Number, Amount, Transaction_Date,
                                              TransDateMonth, TransDateWeek, AgencyMerch) %>%
                                        count()
dim(AgencyMerch_Count)

AgencyMerch_Count <- AgencyMerch_Count %>% dplyr::group_by(AgencyMerch) %>%
                    mutate(Month_Purch=sum(AgencyMerchSet$Agency_Number & Transaction_Date > AgencyMerchSet$Transaction_Date
                                            & TransDateMonth<AgencyMerchSet$Transaction_Date),
                           Week_Purch=sum(AgencyMerchSet$Agency_Number & Transaction_Date > AgencyMerchSet$Transaction_Date
                                            & TransDateWeek<AgencyMerchSet$Transaction_Date),
                           SumAmt = sum(Amount))
AgencyMerch_Count
summary(AgencyMerch_Count)

#Adding to main dataset
ccard<- left_join(ccard, AgencyMerch_Count, by = c("AgencyMerch"))

#Adding columns showing count of transactions by Monthly, BiMohthly, and Weekly 
AgencyMerchSet2 <- train[,c('Agency_Number', 'Amount','Transaction_Date','TransDateMonth',
                           'TransDateWeek','AgencyMerch')]

AgencyMerch_Count2 <- AgencyMerchSet2 %>% dplyr::group_by(Agency_Number, Amount, Transaction_Date,
                                                        TransDateMonth, TransDateWeek, AgencyMerch) %>%
                                                        count()
dim(AgencyMerch_Count)

AgencyMerch_Count2 <- AgencyMerch_Count2 %>% dplyr::group_by(AgencyMerch) %>%
          mutate(Month_Purch=sum(AgencyMerchSet$Agency_Number & Transaction_Date > AgencyMerchSet$Transaction_Date
                         & TransDateMonth<AgencyMerchSet$Transaction_Date),
         Week_Purch=sum(AgencyMerchSet$Agency_Number & Transaction_Date > AgencyMerchSet$Transaction_Date
                        & TransDateWeek<AgencyMerchSet$Transaction_Date),
         SumAmt = sum(Amount))

#Sum amount of AgencyMerch
SumAmt_Count <- AgencyMerchSet %>% dplyr::group_by(AgencyMerch) %>%
         mutate(SumAmt = sum(Amount))

SumAmt_Count

levels(ccard$Merchant_Category)
levels(ccard$Agency_Name)

#Adding to main dataset- too big so we have to reduce the dataset
#ccard<- left_join(ccard, AgencyMerch_Count, by = c("AgencyMerch"))
library(caTools)
split = sample.split(ccard, SplitRatio = 0.1)
train = subset(ccard, split == TRUE)
test = subset(ccard, split == FALSE)

train <- left_join(train, AgencyMerch_Count, by = c("AgencyMerch"))

str(train)
train <- train[,-16]
train <- train[,-17]
train <- train[,-18]
train <- train[,-19]

#Highest 

head(train %>% dplyr::arrange(desc(SumAmt)))

#Removing Variables for Computational space
str(ccard)

library(DT)
datatable(
    AgencyMerch_Count,
    rownames = FALSE, 
    colnames=c("Month_Purch","Week_Purch","SumAmt"),
    options = list(columnDefs=list(list(className = 'dt-center', targets = 0:3)))
  )

summary(AgencyMerch_Count)
##Combining above set of variables to the main dataset: too big of a vector
#ccard<- left_join(ccard, AgencyMerch_Count, by = c("AgencyMerch"))

AgencyMerch_Count %>% dplyr::arrange(desc(SumAmt))

#Highest amount per month 
AgencyMerch_Count %>% dplyr::summarise(Count = n(), 
                                       AmtPerMonth = sum(SumAmt/Month_Purch)) %>%
                      dplyr::arrange(desc(AmtPerMonth))

#Lowest amount per month values
AgencyMerch_Count %>% dplyr::summarise(Count = n(), 
                                       AmtPerMonth = sum(SumAmt/Month_Purch)) %>%
                      dplyr::arrange(AmtPerMonth)

#Highest count of Agency Merch per Month and Week
AgencyMerch_Count %>% dplyr::summarise(Count = n(), 
                                       AmtPerMonth = sum(SumAmt/Month_Purch),
                                       AmtPerWeek = sum(SumAmt/Week_Purch)) %>%
                  dplyr::arrange(desc(Count))


#Setting the lag on the dataset: 

#####I was lazy and did not end up doing the redo of the assignment but what we should have done: 

#1. Added AAgencyMerch category and used it with frequency to determine irregularities
#2 Get lagged difference in time and do analysis using figures to determine tendencies in purchases 
#3. Find a way to bin frequency to merchagency category and then identify times where frequencies are out of the norm
