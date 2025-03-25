#Assignment 4- Industry Case - Healthcare Fraud Detection

#install.packages("zipcode")
library(dplyr)
#library(DataExplorer)
#library(xda)
library(ggplot2)
library(data.table)
library(zipcode)
suppressPackageStartupMessages(library(tidyverse))
library(stringr)
library(ggthemes)
#install.packages('ggmap')
#install.packages('DT')
#install.packages('usmap')
#install.packages('rlang')
#install.packages('maps')
suppressPackageStartupMessages(library(maps))
#install.packages('woeBinning')
library(woeBinning)
library(corrplot)
library(DT)

data(zipcode)
nrow(zipcode)
zipcode$city <- as.factor(zipcode$city)
zipcode$state <- as.factor(zipcode$state)
map('usa')
levels(zipcode$city)
levels(zipcode$state)

setwd('C:\\Users\\marko\\OneDrive\\Documents\\Columbia\\Anomaly Detection - Summer 2019')
payment <- read.csv("inpatientCharges.csv")
str(payment)

?clean.zipcodes
?formatCurrency
str(payment)
str(payment)

head(payment$Average.Total.Payments)

# Convert the average to numeric
p1 <- strsplit(x = as.character(payment$Average.Covered.Charges),split = "$",fixed = T)
payment$Average.Covered.Charges <- as.numeric(sapply(p1,"[[",2))
p1 <- strsplit(x = as.character(payment$Average.Total.Payments),split = "$",fixed = T)
payment$Average.Total.Payments <- as.numeric(sapply(p1,"[[",2))
p1 <- strsplit(x = as.character(payment$Average.Medicare.Payments),split = "$",fixed = T)
payment$Average.Medicare.Payments <- as.numeric(sapply(p1,"[[",2))
rm("p1")

summary(payment[,c('Total.Discharges','Average.Total.Payments','Average.Medicare.Payments','Total.Discharges')])

dim(payment)

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
} #Function to scale the data 

##Splitting of data
#install.packages('caTools')
library(caTools)
split = sample.split(payment, SplitRatio = 0.5)
train = subset(payment, split == TRUE)
test = subset(payment, split == FALSE)

###Variables: 

##DRG Definition: Classification system that groups similar clinical conditions (diagnoses) 
#and the procedures furnished by the hospital during the stay.
levels(payment$DRG.Definition)
drg <- payment %>% group_by(DRG.Definition) %>%
  summarise(Count = n()) %>% 
  arrange(desc(Count))

datatable(
  drg,
  rownames = FALSE, 
  colnames=c("DRG.Definition", 'Count'),
  options = list(columnDefs=list(list(className = 'dt-center', targets = 0:2)))
)

##Total Discharges: The number of discharges billed by all providers for inpatient hospital services.
max(payment$Total.Discharges)
min(payment$Total.Discharges)
summary(payment$Total.Discharges)

##Average Covered Charges: The average charge of all provider's services covered by Medicare for 
#discharges in the DRG. These will vary from hospital to hospital because of differences in hospital charge structures.
levels(payment$Average.Covered.Charges)
payment$Average.Covered.Charges

##Average Total Payments: The average total payments to all providers for the DRG including the MS-DRG amount, teaching,  
#disproportionate share, capital, and outlier payments for all cases. Also included in average total payments are 
#co-payment and deductible amounts that the patient is responsible for and any additional payments by third parties for 
#coordination of benefits.
levels(payment$Average.Total.Payments)

##Average Medicare Payments: The average amount that Medicare pays to the provider for Medicare's share of the MS-DRG. 
#Medicare payment amounts include the MS-DRG amount, teaching,  disproportionate share, capital, and outlier payments 
#for all cases. Medicare payments DO NOT include beneficiary co-payments and deductible amounts nor any additional 
#payments from third parties for coordination of benefits.
levels(payment$Average.Medicare.Payments[c(1:10,),])

str(payment)
levels(payment$DRG.Definition)
tail(payment$DRG.Definition)
V2

#Class file
payment %>% arrange(Provider.Id, DRG.Definition) 
head(payment)

#Average Covered charges and Provider State
payment <- as.data.table(payment)
train <- as.data.table(train)
av1 <- payment[,Average.Covered.Charges,by=Provider.State]
av1
ggplot(data = av1,mapping = aes(y = Provider.State,x = Average.Covered.Charges,colour=Average.Covered.Charges))+
  geom_point()

av1train <- train[,Average.Covered.Charges, by=Provider.State]
av1train
ggplot(data = av1train,mapping = aes(y = Provider.State,x = Average.Covered.Charges,colour=Average.Covered.Charges))+
  geom_point()


#Average Total Payments and Provider State
av1 <- payment[,Average.Total.Payments,by=Provider.State]
av1

ggplot(data = av1,mapping = aes(y = Provider.State,x = Average.Total.Payments,colour=Average.Total.Payments))+
  geom_point()

V1 <- payment %>% # aggregate procedures for each hospital
  group_by(Provider.Id, Provider.Zip.Code, Provider.Name) %>% # keep zip & name
  summarise(procSum = sum(Total.Discharges) ) 
str(V1)
V1
str(payment)
levels(payment$Provider.Name)

# merge aggregated hospital data with zipcode, copy lat+lon for each hospital
V2 <- merge(V1,zipcode, by.x= "Provider.Zip.Code", by.y= "zip")
V2

g <- list( 
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  subunitwidth = 1,
  countrywidth = 1
)
g
str(g)
g
plot(g)
V1 <- payment %>% # aggregate procedures for each hospital
  group_by(Provider.Id, Provider.Zip.Code, Provider.Name) %>% # keep zip & name
  summarise(procSum = sum(Total.Discharges) ) 

V1

V2 <- merge(V1,zipcode, by.x= "Provider.Zip.Code", by.y= "zip")
V2
head(V2)

str(payment)

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
} #Function to scale the data 

#End of class file

##Number of providers per state
ProviderperState <- payment %>% group_by(Provider.Name, Provider.State) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
ProviderperState

datatable(
  ProviderperState,
  rownames = FALSE, 
  colnames=c("Provider.Name", "Provider.State", "Count"),
  options = list(columnDefs=list(list(className = 'dt-center', targets = 0:3)))
)

ProviderZip <- payment %>% mutate(StateZip = paste(Provider.State, 
                             Provider.Zip.Code, sep="_")) %>% 
  group_by(Provider.Name, StateZip) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

datatable(
  ProviderZip,
  rownames = FALSE, 
  colnames=c("Provider.Name", "StateZip", "Count"),
  options = list(columnDefs=list(list(className = 'dt-center', targets = 0:3)))
)


ProviderperState2 <- payment %>% group_by(Provider.Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

datatable(
  ProviderperState2,
  rownames = FALSE, 
  colnames=c("Provider.Name", "Count"),
  options = list(columnDefs=list(list(className = 'dt-center', targets = 0:2)))
)

##################Average Total Payments - Average Medicare - Average Covered 

diff1 <- payment %>% 
  dplyr::mutate(OutPocket = Average.Medicare.Payments - Average.Covered.Charges)

diff <- payment$Average.Covered.Charges - payment$Average.Medicare.Payments
summary(diff)

TotMedDiff <- payment$Average.Total.Payments - payment$Average.Medicare.Payments
TotCovDiff <- payment$Average.Total.Payments - payment$Average.Covered.Charges
summary(TotMedDiff)
summary(TotCovDiff)

payment <- cbind(payment, TotMedDiff, TotCovDiff)

#Correlation between three payments: 
num <- dplyr::select(payment, Average.Total.Payments, Average.Medicare.Payments, Average.Covered.Charges, Total.Discharges)
corrplot(corr = stats::cor(num))

###########################Average Total Payments - State and DRG scaled
#Added standard deviation as well as payment, coverage, medicare, discharge, and SD of total payments. 

#Making Scaled dataset
Scaled <- payment %>% mutate(DRGState = paste(Provider.State, 
                                               DRG.Definition, sep="_"),                    
                                   ScTotPay = scale_this(Average.Total.Payments),
                                   ScTotCov = scale_this(Average.Covered.Charges),
                                   ScTotMed = scale_this(Average.Medicare.Payments),
                                   ScTotDis = scale_this(Total.Discharges))
str(Scaled)
Scaled

ScaledSum <- Scaled%>% dplyr::group_by(DRGState) %>%
                  dplyr::summarise(Count = n(),
                  ScTotPay = sum(ScTotPay),
                  ScTotCov = sum(ScTotCov),
                  ScTotMed = sum(ScTotMed),
                  ScTotDis = sum(ScTotDis),
                  SD = sd(ScTotPay)) 
ScaledSum$SD[is.na(ScaledSum$SD)] <- 0

summary(ScaledSum$ScTotPay)
summary(ScaledSum$ScTotCov)
summary(ScaledSum$ScTotMed)
summary(ScaledSum$ScTotDis)

levels(payment$Hospital.Referral.Region.Description)

#Total Payment correlated with higher Medicare payment- correlation between the two was 66%
ScaledSum %>% arrange(desc(ScTotPay)) %>% top_n(10, ScTotPay) #6/10 highest total payment in California, rest are from texas. This is due to size clearly. 
ScaledSum %>% arrange(ScTotPay) %>% top_n(10, -ScTotPay) #5/10 lowest Total Payment in California (same DRG also lowest Total Medicare Payment). Rest are Florida and Texas
ScaledSum %>% arrange(desc(ScTotCov)) %>% top_n(10, ScTotCov)#8/10 lowest Total Coverage in California
ScaledSum %>% arrange(ScTotCov) %>% top_n(10, -ScTotCov)#5/10 lowest Total Coverage in Illinois (1 in Indiana). Bottom 3 are cardiac related
ScaledSum %>% arrange(desc(ScTotMed)) %>% top_n(10, ScTotMed) #6/10 scaled highest medicare cost in California (Texas other 4)
ScaledSum %>% arrange(ScTotMed) %>% top_n(10, -ScTotMed) #8/10 scaled total medicare lowest in California 
ScaledSum %>% arrange(desc(ScTotDis)) %>% top_n(10, ScTotDis) #6/10 DRG titles are major around relatively populous states
ScaledSum %>% arrange(ScTotDis) %>% top_n(10, -ScTotDis) #10/10 lowest scaled discharges in California

map <- ScaledSum %>% arrange(ScTotCov)
map <- map[1:5,]

summary(ScaledSum)
  
ggplot(data=map, aes(x=reorder(DRGState, -ScTotCov), y=ScTotCov, fill = SD)) +
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("                                     Scaled least amount of Average Total Coverege Charges") +
  ylab('Scaled numeric value of Coverage Payment') +
  xlab('State and DRG Procedure Tag') + 
  theme(axis.text.x  = element_text(angle=30, vjust=0.5, size=8))

#Scaled by provider name
Scaled2 <- payment %>% mutate(DRGState = paste(Provider.State, 
                                               DRG.Definition, sep="_"),                    
                              ScTotPay = scale_this(Average.Total.Payments),
                              ScTotCov = scale_this(Average.Covered.Charges),
                              ScTotMed = scale_this(Average.Medicare.Payments),
                              ScTotDis = scale_this(Total.Discharges)) %>% 
  group_by(Provider.Name, DRGState)

summary(Scaled2)

Scaled2 %>% arrange(desc(ScTotPay)) %>% select(Provider.Name, DRGState, ScTotPay)
Scaled2 %>% arrange(desc(ScTotCov)) %>% select(Provider.Name, DRGState, ScTotCov)
Scaled2 %>% arrange(desc(ScTotMed)) %>% select(Provider.Name, DRGState, ScTotMed)

ScaledSum2 <- payment %>% mutate(DRGState = paste(Provider.State, 
                                                  DRG.Definition, sep="_")) %>%
  dplyr::group_by(DRGState, Provider.Name) %>%
  dplyr::summarise(Count = n(),
                   TotPay = sd(sum(Average.Total.Payments)),
                   TotCov = sd(sum(Average.Covered.Charges)),
                   TotMed = sd(sum(Average.Medicare.Payments)),
                   TotDis = sd(sum(Total.Discharges))) 

ScaledSum2[is.na(ScaledSum2)] <- 0

summary(ScaledSum2)
anyNA(ScaledSum)
summary(ScaledSum2)
ScaledSum2$TotCov

ScaledSum2 %>% arrange(SD)

#Trying interactive groupig format

formatCurrency(
  datatable(
    ScaledSum,
    rownames = FALSE, 
    colnames=c("DRGState", "Count","ScTotPay", "ScTotCov", "ScTotMed", 'ScTotDis', 'SD'),
    options = list(columnDefs=list(list(className = 'dt-center', targets = 0:4)))
  ),
  c("ScTotPay","ScTotMed", "SD")
)

##Total Payment largest Standard deviation DRG: 
# VT_391 ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W MCC but only 2 of them
##Above 7
#AR_418 - LAPAROSCOPIC CHOLECYSTECTOMY W/O C.D.E. W CC 
#NV_482 - HIP & FEMUR PROCEDURES EXCEPT MAJOR JOINT W/O CC/MCC
##Above 230
#CA_872 - SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W/O MCC 

Totpay3
summary(Totpay3$Count)

dim(payment)
str(payment)

#############################Total Payments by state
TotPayST <- payment %>% dplyr::group_by(Provider.State) %>%
            dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments/1000),
                   TotCov = sum(Average.Covered.Charges/1000),
                   TotMed = sum(Average.Medicare.Payments/1000),
                   TotDis = sum(Total.Discharges),
                   SD = sd(Average.Total.Payments),
                   PayPerDis = sum((Average.Total.Payments/Total.Discharges)/1000)) %>%
  dplyr::arrange(desc(SD))
TotPayST

summary(TotPayST$PayPerDis)

ggplot(data=TotPayST, aes(x=reorder(Provider.State, TotPay),y=TotPay)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("                                     Average Total Payment by State") +
  ylab('Total Average Payment per State in Thousands') +
  xlab('State')

#Top 10 total payment
map1 <- TotPayST[1:10,]
ggplot(data=map1, aes(x=reorder(Provider.State, TotPay),y=TotPay, fill = SD)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("                                     Total Payment per State with SD") +
  ylab('Total Average Payment per state in thousands') +
  xlab('State')

#Paymet per discharge per state
ggplot(data=TotPayST, aes(x=reorder(Provider.State, PayPerDis),y=PayPerDis)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("                                     Average Total Payment per Discharge per State") +
  ylab('Total Average Payment per Discharge per State in thousands') +
  xlab('State')

#Payment minus Coverage per state
ggplot(data=TotPayST, aes(x=reorder(Provider.State, -(TotPay-TotCov)),y=TotPay - TotCov)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("                                     Total Payment minus Average Coverage per State") +
  ylab('Total Average Payment per Discharge per State in thousands') +
  xlab('State')


######################With State, DRG, and Zicode
Totpay <- payment %>% mutate(DRGState = paste(Provider.State, 
                                              DRG.Definition, sep="_")) %>%
  dplyr::group_by(DRGState, Provider.Zip.Code) %>%
  dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments),
                   TotCov = sum(Average.Covered.Charges),
                   TotMed = sum(Average.Medicare.Payments),
                   TotDis = sum(Total.Discharges),
                   SD = sd(Average.Total.Payments)) %>%
  dplyr::arrange(desc(SD))
Totpay$SD[is.na(Totpay$SD)] <- 0

Totpay

#With State, DRG, and Zicode
Totpay <- payment %>% mutate(DRGState = paste(Provider.State, 
                                              DRG.Definition, sep="_")) %>%
  dplyr::group_by(DRGState) %>%
  dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments/1000),
                   TotCov = sum(Average.Covered.Charges/1000),
                   TotMed = sum(Average.Medicare.Payments/1000),
                   TotDis = sum(Total.Discharges),
                   SD = sd(Average.Total.Payments)) %>%
  dplyr::arrange(desc(SD))
Totpay$SD[is.na(Totpay$SD)] <- 0
Totpay

#The highest SD has small counts so will filter for above 7 counts
map <- Totpay %>% filter(Count > 7)
map
map <- map[1:5,]
map
ggplot(data=map, aes(x=reorder(DRGState, -SD), y= SD, fill = TotPay)) +
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("                   Standard Deviation of Payments for Procedures done more than 7 times") +
  ylab('Standard Deviation of Total Average Payments') +
  xlab('State and DRG Procedure') + 
  theme(axis.text.x  = element_text(angle=35, vjust=0.5, size=8))
map

#################Heatmap
#install.packages('devtools')
library(devtools)
#install_github('UrbanInstitute/urbnmapr')
library(tidyverse)
library(urbnmapr)
#install.packages('scales')
library(scales)

#Basic map of US
ggplot(urbnmapr::states, aes(x = long, y = lat, group = group))+
  geom_polygon(fill = 'grey', color = 'white') +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

TotPayST_Merge <- TotPayST %>% select(Provider.State, SD) %>% 
  merge(states, by.x= "Provider.State", by.y= "state_abbv")

TotPayST_Merge2 <- TotPayST %>% select(Provider.State, SD) %>% 
  merge(zipcode, by.x= "Provider.State", by.y= "state")


#From Paulinas file - Success
#CREATING MAP
library(DT)
library(usmap)
TotPayST_Merge2 <- as.data.frame(TotPayST_Merge2[,c('Provider.State','SD')]) 
colnames(TotPayST_Merge2) <- c('state', 'Standard Dev')

map<-plot_usmap(data=TotPayST_Merge2,values = 'Standard Dev')

map+
  scale_fill_continuous(name="SD of Total Payments",low="white",high="blue")+
  ggtitle("Standard deviation of Total Payments accross the state")+
  theme(plot.title = element_text(hjust = 0.5))


TotPayST_Merge2

#Total payments- States, zip and drg

Totpay3 <- payment %>% mutate(DRGState = paste(Provider.State, 
                                               DRG.Definition, sep="_")) %>%
  dplyr::group_by(DRGState, Provider.Zip.Code) %>%
  dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments),
                   TotCov = sum(Average.Covered.Charges),
                   TotMed = sum(Average.Medicare.Payments),
                   TotDis = sum(Total.Discharges),
                   SD = sd(Average.Total.Payments)) %>%
  dplyr::arrange(desc(SD))
Totpay2$SD[is.na(Totpay3$SD)] <- 0

#Map Zipcodes in us with largest SD in total payment per zipcode 
map('usa')
selected <- zipcode[zipcode$zip %in% c( "75701", "08901", "04240","72401", "75093"),]
points( selected$longitude, selected$latitude, pch= 20, cex= 2 )
text( selected$longitude, selected$latitude, selected$zip, pos=3, cex= .7)

#Making set with differences from Total Payments
TotPayDiff <- payment %>% mutate(DRGState = paste(Provider.State, 
                                                  DRG.Definition, sep="_"))%>%
                    dplyr::group_by(DRGState) %>%
                    dplyr::summarise(Count = n(),
                   Med = (sum(Average.Total.Payments)-sum(Average.Medicare.Payments))/1000,
                   Cov = (sum(Average.Total.Payments)- sum(Average.Covered.Charges))/1000,
                   PayPerDis = (sum(Average.Total.Payments)/sum(Total.Discharges)))
summary(TotPayDiff$Med)
summary(TotPayDiff$Cov)
summary(TotPayDiff$PayPerDis)

TotPayDiff %>% dplyr::arrange(desc(Med)) #Shows the highest instances of payments being higher than medicare payment
TotPayDiff %>% dplyr::arrange(Med) #Shows the largest difference between average payment and average medicare payment
TotPayDiff %>% dplyr::arrange(desc(Cov)) #Shws procedures per state that had payments higher than the coverage charges
TotPayDiff %>% dplyr::arrange(Cov) #Shows instances where coverage payments were much higher than total payments

str(payment)

#Average Coverage
TotCov <- payment %>% dplyr::group_by(DRG.Definition, Provider.State) %>%
  dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments),
                   TotCov = sum(Average.Covered.Charges),
                   TotMed = sum(Average.Medicare.Payments),
                   TotDis = sum(Total.Discharges)) %>%
  dplyr::arrange(desc(TotCov))
TotCov

#Average Medicare
TotMed <- payment %>% dplyr::group_by(DRG.Definition, Provider.State) %>%
  dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments),
                   TotCov = sum(Average.Covered.Charges),
                   TotMed = sum(Average.Medicare.Payments),
                   TotDis = sum(Total.Discharges)) %>%
  dplyr::arrange(desc(TotMed))
TotMed

#Total Dishcarge
summary(payment$Total.Discharges)
payment[payment$Total.Discharges == 3383.00,]
payment[payment$Total.Discharges == 11.00,]

TotDis <- payment %>% dplyr::group_by(DRG.Definition) %>%
  dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments),
                   TotCov = sum(Average.Covered.Charges),
                   TotMed = sum(Average.Medicare.Payments),
                   TotDis = sum(Total.Discharges,),
                   SD = sd(Average.Total.Payments),
                   DisDiv = Average.Total.Payments/Total.Discharges) %>%
  dplyr::arrange(desc(TotDis))

#Relationshp of discharge to total payment
ggplot(payment, aes(x=Total.Discharges, y = payment$Average.Total.Payments)) +
  geom_point()

#Major discharge outlier was Major Joint replacement in New York 70th street 
max(payment$Total.Discharges)
payment[payment$Total.Discharges == 3383,]

ggplot(payment, aes(x=Average.Medicare.Payments, y = payment$Average.Total.Payments, fill = Provider.State)) +
  geom_point()

ggplot(Totpay, aes(x=TotMed, y = TotPay, fill = DRGState)) + 
  geom_point()

#Total payment minus Medicare spending
TotPaymMed <- payment %>% dplyr::group_by(DRG.Definition, Provider.State) %>%
  dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments),
                   TotCov = sum(Average.Covered.Charges),
                   TotMed = sum(Average.Medicare.Payments),
                   TotDis = sum(Total.Discharges),
                   PayMed = (sum(Average.Total.Payments)-sum(Average.Medicare.Payments))/1000) %>%
  dplyr::arrange(desc(PayMed))
TotPaymMed

##Mapping top 5 medicare payments
#Overall
TotPaymMed1 <- payment %>% dplyr::group_by(DRG.Definition) %>%
  dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments),
                   TotCov = sum(Average.Covered.Charges),
                   TotMed = sum(Average.Medicare.Payments/1000),
                   TotDis = sum(Total.Discharges),
                   PayMed = (sum(Average.Total.Payments)-sum(Average.Medicare.Payments))/1000) %>%
  dplyr::arrange(desc(PayMed))

TotPaymMed1
summary(TotPaymMed1$PayMed)

map <- TotPaymMed1[1:10,]
ggplot(map, aes(x = reorder(DRG.Definition, -TotMed), y = TotMed, group=1)) +
  geom_line(size = 1) + 
  geom_point(size=2, fill=map$Count) +
  ylab("Total Medicare payments in thousands") + xlab("Top 10 Medicare payments by DRG") +
  theme(axis.text.x  = element_text(angle=30, vjust=0.5, size=8))

#Most medicare payments by state With state
TotPaymMed2 <- payment %>% mutate(DRGState = paste(Provider.State, 
                                                   DRG.Definition, sep="_")) %>% 
                    dplyr::group_by(DRGState) %>%
                    dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments),
                   TotCov = sum(Average.Covered.Charges),
                   TotMed = sum(Average.Medicare.Payments/1000),
                   TotDis = sum(Total.Discharges),
                   PayMed = (sum(Average.Total.Payments)-sum(Average.Medicare.Payments))/1000) %>%
  dplyr::arrange(desc(TotMed)) %>%
  top_n(10, TotMed)
TotPaymMed2

ggplot(TotPaymMed2, aes(x = reorder(DRGState, -TotMed), y = TotMed, group=1)) +
  geom_line(size = 1) + 
  geom_point(size=2, fill=TotPaymMed2$Count) +
  ylab("Total Medicare payments in thousands") + xlab("Top 10 Medicare payments by DRG and State") +
  theme(axis.text.x  = element_text(angle=30, vjust=0.5, size=8))

TotPaymMed2 <- payment %>% mutate(DRGState = paste(Provider.State, 
                                                   DRG.Definition, sep="_")) %>% 
  dplyr::group_by(DRGState) %>%
  dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments),
                   TotCov = sum(Average.Covered.Charges),
                   TotMed = sum(Average.Medicare.Payments/1000),
                   TotDis = sum(Total.Discharges),
                   PayMed = (sum(Average.Total.Payments)-sum(Average.Medicare.Payments))/1000) %>%
  dplyr::arrange(desc(PayMed))
TotPaymMed2

#Total Scaled payments to referral refion
Scaled1 <- payment %>% mutate(DRGState = paste(Provider.State, 
                                              DRG.Definition, sep="_"),                    
                             ScCovMedDiff = scale_this(Average.Medicare.Payments - Average.Covered.Charges),
                             ScPayCovDiff = scale_this(Average.Total.Payments - Average.Covered.Charges),
                             ScPayMedDiff = scale_this(Average.Total.Payments - Average.Medicare.Payments),
                             ScPayperDis = scale_this(Average.Total.Payments/Total.Discharges))


summary(Scaled1$ScCovMedDiff)
Scaled1Sum <- Scaled1 %>% dplyr::group_by(DRGState, Hospital.Referral.Region.Description) %>%
  dplyr::summarise(Count = n(),
                   ScCovMedDiff = sum(ScCovMedDiff),
                   ScPayCovDiff = sum(ScPayCovDiff),
                   ScPayMedDiff = sum(ScPayMedDiff),
                   ScPayperDis = sum(ScPayperDis)) 

Scaled1Sum %>% dplyr::arrange(desc(ScCovMedDiff)) %>% top_n(10, -ScCovMedDiff) #9/10 CovMedicare diff in Boston area of MA
Scaled1Sum %>% dplyr::arrange(ScCovMedDiff) %>% top_n(10,ScCovMedDiff) #9/10 on california
Scaled1Sum %>% dplyr::arrange(desc(ScPayCovDiff)) %>% top_n(10,-ScPayCovDiff) #7/10 in MA, rest from GA
Scaled1Sum %>% dplyr::arrange(ScPayCovDiff) %>% top_n(10,ScPayCovDiff) #5/10 in GA Rest in less populous states
Scaled1Sum %>% dplyr::arrange(desc(ScPayMedDiff)) %>% top_n(10,-ScPayMedDiff) #4/10 in AL, 4/10 in GA
Scaled1Sum %>% dplyr::arrange(ScPayMedDiff) %>% top_n(10, ScPayMedDiff) #All 10 states in the south
map$DRGState

map$Hospital.Referral.Region.Description
map <- Scaled1Sum %>% dplyr::arrange(desc(ScPayMedDiff)) %>% top_n(10,-ScPayMedDiff)
map <- map[1:5,]
map$ScPayMedDiff

ggplot(data=map, aes(x=reorder(Hospital.Referral.Region.Description, -ScPayMedDiff), y= ScPayMedDiff, fill = DRGState)) +
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("             Total Paymeyent and Medicare Difference") +
  ylab('Scaled Total Payment to Medicare Difference') +
  xlab('Region') + 
  theme(axis.text.x  = element_text(angle=35, vjust=0.5, size=8))



#Taking into account provider

#Total Pay calculation based on provider and DRG+State
TotPayProvider <- payment %>% mutate(DRGState = paste(Provider.State, 
                                                DRG.Definition, sep="_")) %>%
                  dplyr::group_by(DRGState, Provider.Name) %>%
                  dplyr::summarise(Count = n(),
                   TotPay = sum(Average.Total.Payments/1000),
                   TotCov = sum(Average.Covered.Charges/1000),
                   TotMed = sum(Average.Medicare.Payments/1000),
                   TotDis = sum(Total.Discharges),
                   SD = sd(Average.Total.Payments),
                   PayPerDis = sum((Average.Total.Payments/Total.Discharges)/1000))
TotPayProvider$SD[is.na(TotPayProvider$SD)] <- 0

summary(TotPayProvider)


#Analysis of top items for each mutated variable
TotPayProvider %>% dplyr::arrange(desc(TotPay)) #6/10 Good Samaritan in California, other 4 are st marys in illinois
TotPayProvider %>% dplyr::arrange(desc(TotCov)) #7/10 in california- good samaritan, st. joseph, st. mary medical
TotPayProvider %>% dplyr::arrange(desc(TotMed)) #6/10 of states in St. Marys in Illinois, other 4 Good Samaritan in California
TotPayProvider %>% dplyr::arrange(desc(TotDis)) #4/10 Major procedures. All of them have one count but discharges in the thousands. NY major joint replacement majr outlier (3383)
TotPayProvider %>% dplyr::arrange(desc(SD)) #3/10 (including the top) is in Ohio- good samaritan. california and washington states have multiple
TotPayProvider %>% dplyr::arrange(desc(PayPerDis)) #6/10 californtia. Top one is in Ohio- good samaritan
TotPayProvider %>% dplyr::arrange(PayPerDis) #Top one is in california for psychosis

#Scaled Provider on Provider and DRG+State
ScaledProvider <- payment %>% mutate(DRGState = paste(Provider.State, 
                                              DRG.Definition, sep="_"),                    
                             ScTotPay = scale_this(Average.Total.Payments),
                             ScTotCov = scale_this(Average.Covered.Charges),
                             ScTotMed = scale_this(Average.Medicare.Payments),
                             ScTotDis = scale_this(Total.Discharges))
str(Scaled)

ScaledProvSum <- ScaledProvider %>% dplyr::group_by(DRGState, Provider.Name) %>%
  dplyr::summarise(Count = n(),
                   ScTotPay = sum(ScTotPay),
                   ScTotCov = sum(ScTotCov),
                   ScTotMed = sum(ScTotMed),
                   ScTotDis = sum(ScTotDis),
                   SD = sd(Average.Total.Payments)) 
ScaledProvSum$SD[is.na(ScaledProvSum$SD)] <- 0

summary(ScaledProvSum)

#Analysis of top Provider and state mutated from the scaled variables
ScaledProvSum %>% dplyr::arrange(desc(ScTotPay)) %>% top_n(10, ScTotPay) #6/10 from california. 2 and 3 are IL. First one (CA) is major outlier with 4.99
ScaledProvSum %>% dplyr::arrange(desc(ScTotCov)) %>% top_n(10, ScTotCov) #8/10 in california. 1st is in illinois. 
ScaledProvSum %>% dplyr::arrange(desc(ScTotMed)) %>% top_n(10, ScTotMed) #4/10 top ones are in illinis. next 4 are california
ScaledProvSum %>% dplyr::arrange(desc(ScTotDis)) %>% top_n(10, ScTotDis) #Top in Ny. Wide ranging of states but 5/10 major surgeries

########Feedback from the professor for EDA Report
##############################You can try to create ratio variables of provider to DRGstate statistics.

######Variables that will be used for Final EDA Report: 
##Final DB
FinalDB <- payment %>% mutate(DRGState = paste(Provider.State, 
                                                DRG.Definition, sep="_"),
                               MedPerDis = (Average.Medicare.Payments/Total.Discharges)) #Medicare payments per discharge

FinalDB <- select(FinalDB, Provider.Id, Provider.Name, Provider.Zip.Code, Average.Covered.Charges,
                  Average.Total.Payments, Average.Medicare.Payments, Total.Discharges, 
                  DRGState, MedPerDis) 

#install.packages('caret')
library(caTools)
library(plyr)
split = sample.split(payment, SplitRatio = 0.5)
train = subset(payment, split == TRUE)
test = subset(payment, split == FALSE)

train <- train %>% mutate(DRGState = paste(Provider.State, 
                                             DRG.Definition, sep="_"))

train <- select(train, Provider.Id, Provider.Name, Provider.Zip.Code, Average.Covered.Charges,
                Average.Total.Payments, Average.Medicare.Payments, Total.Discharges, 
                DRGState) 

str(train)
rm(train)
#########1. Sd of Total Payments per Provider per state

Scaled <- payment %>% mutate(DRGState = paste(Provider.State, 
                                              DRG.Definition, sep="_"),                    
                             ScTotPay = scale_this(Average.Total.Payments),
                             ScTotCov = scale_this(Average.Covered.Charges),
                             ScTotMed = scale_this(Average.Medicare.Payments),
                             ScTotDis = scale_this(Total.Discharges))

Scaled %>% select(DRGState, ScTotPay, ScTotMed, ScTotCov, Provider.Name)%>% group_by(DRGState, Provider.Name) %>% 
  arrange(desc(ScTotPay))

ScaledSum <- Scaled %>% dplyr::group_by(DRGState) %>%
  dplyr::summarise(Count = n(),
                   ScTotPay = sum(ScTotPay),
                   ScTotCov = sum(ScTotCov),
                   ScTotMed = sum(ScTotMed),
                   ScTotDis = sum(ScTotDis),
                   SD = sd(ScTotPay)) 
ScaledSum$SD[is.na(ScaledSum$SD)] <- 0

#Adding to Final DB
FinalDB <- FinalDB %>% mutate(                    
                             ScTotPay = scale_this(Average.Total.Payments),
                             ScTotCov = scale_this(Average.Covered.Charges),
                             ScTotMed = scale_this(Average.Medicare.Payments),
                             ScTotDis = scale_this(Total.Discharges))

train <- train %>% mutate(                    
  ScTotPay = scale_this(Average.Total.Payments),
  ScTotCov = scale_this(Average.Covered.Charges),
  ScTotMed = scale_this(Average.Medicare.Payments),
  ScTotDis = scale_this(Total.Discharges))

str(train)

#########2. Total Payments minus Medicare Payments - scaled
TotPaymMed1 <- payment %>% mutate(DRGState = paste(Provider.State, 
                                                   DRG.Definition, sep="_")) %>% 
                                    dplyr::group_by(DRGState, Provider.Name) %>%
                  dplyr::summarise(Count = n(),
                   PayMedDiff = (sum(Average.Total.Payments)-sum(Average.Medicare.Payments))/1000,
                   CovMedDiff = (sum(Average.Covered.Charges)-sum(Average.Medicare.Payments))/1000,
                   CovTotDiff = (sum(Average.Covered.Charges)-sum(Average.Total.Payments))/1000)
summary(TotPaymMed1)
summary(TotPaymMed1$Count)

TotPaymMed1 %>% filter(Count > 2)

TotPaymMed1 %>% arrange(CovMedDiff) 

TotPaymMed1Sc <- TotPaymMed1 %>% mutate(PayMedDiffSc = scale_this(PayMedDiff),
                                        CovMedDiffSc = scale_this(CovMedDiff),
                                        CovTotDiffSc = scale_this(CovTotDiff)) %>% ungroup()
TotPaymMed1Sc$PayMedDiffSc[is.na(TotPaymMed1Sc$PayMedDiffSc)] <- 0
TotPaymMed1Sc$CovMedDiffSc[is.na(TotPaymMed1Sc$CovMedDiffSc)] <- 0
TotPaymMed1Sc$CovTotDiffSc[is.na(TotPaymMed1Sc$CovTotDiffSc)] <- 0

summary(TotPaymMed1Sc)

#Top 10 total payment minus medicare biggest differences where Medicare Payments were larger
map2 <- TotPaymMed1 %>% arrange(PayMedDiff)
map2 <- map2[1:10,]
ggplot(data=map2, aes(x=reorder(Provider.Name, PayMedDiff),y=PayMedDiff, fill = DRGState)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("                                     Total Payments minus Medicare payments") +
  ylab('Difference between total Payment and Medicare scaled to thousands') +
  xlab('State') + 
  theme(legend.position="bottom")

#Top 10 differences where Covered Payments were higher than Medicare Payments
map3 <- TotPaymMed1 %>% arrange(desc(CovMedDiff))
map3 <- map3[1:10,]
ggplot(data=map3, aes(x=reorder(Provider.Name, CovMedDiff),y=CovMedDiff, fill = DRGState)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("                       Top 10 Total Covered Payments higher than Medicare Payments") +
  ylab('Difference between total Payment and Medicare scaled to thousands (div by 1000)') +
  xlab('State') + 
  theme(legend.position="bottom")

#Top 10 differences where Covered Payments were higher than Total Payment
map4 <- TotPaymMed1 %>% arrange(desc(CovTotDiff))
map4 <- map4[1:10,]
ggplot(data=map3, aes(x=reorder(Provider.Name, CovTotDiff),y=CovTotDiff, fill = DRGState)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("                       Top 10 Total Covered Payments higher than Total Average Payments") +
  ylab('Difference between covered payments and total payments (div by 1000)') +
  xlab('State') + 
  theme(legend.position="bottom")

#Just applying the difference between the two- sign of fraud if more spent on medicare than coverage
#(not sure why there is a discrepancy when alligning the two)
CovMedDiff <- payment %>% mutate(DRGState = paste(Provider.State, 
                                                   DRG.Definition, sep="_")) %>%
  group_by(DRGState, Provider.Name) %>%
  summarise(Count = n(),
            Coverage = sum(Average.Covered.Charges),
            Medicare = sum(Average.Medicare.Payments),
            Diff = Coverage - Medicare,
            CovMedDiff = (sum(Average.Covered.Charges)-sum(Average.Medicare.Payments))) %>%
  arrange(CovMedDiff)
CovMedDiff

#Adding to FinalDB
summary(FinalDB$PayMedDiffSc)

FinalDB <- FinalDB %>% left_join(TotPaymMed1Sc, by = "DRGState")
summary(FinalDB)

rm(FinalDB)

#Left_join would consistently add over 3 millin rows
train <- train %>% left_join(TotPaymMed1Sc, by = "DRGState")
summary(train)

################Join function worked (Check if you still have to do the subgroupings with the train sample)
train <- train %>% join(TotPaymMed1Sc, by='DRGState', type="left", match="first")
str(TotPaymMed1Sc)
str(train)

########3. Fraction of total payments per numeric variables
#DRG
PercTot <- payment %>% mutate(DRGState = paste(Provider.State, 
                                                 DRG.Definition, sep="_")) %>%
  group_by(Provider.Name, DRGState) %>%
  summarise(Count = n(),
            TotalPayments = sum(Average.Total.Payments),
            MedicarePayments = sum(Average.Medicare.Payments),
            CoveredCharges = sum(Average.Covered.Charges),
            Discharges = sum(Total.Discharges),
            MedPerc = round(MedicarePayments/TotalPayments,2),
            CovPerc = round(CoveredCharges/TotalPayments,2),
            DisPerc = round(Discharges/TotalPayments,2),
            MedPerDis = round(TotalPayments/Discharges,2)) %>% 
  ungroup()


PercTot %>% arrange(desc(MedPerc))

max(PercTot$MedPerc)
summary(PercTot)
str(PercTot)
nrow(PercTot)
PercTot %>% filter(Count > 2)

PercTot %>% arrange(desc(MedPerc))
PercTot %>% arrange(MedPerc)
PercTot %>% arrange(MedPerDis) %>% select(MedPerDis, DRGState, Provider.Name, Count)
PercTot %>% arrange(desc(MedPerDis)) %>% select(MedPerDis, DRGState, Provider.Name, Count)
PercTot %>% select(MedPerc, Count, DRGState) %>% filter(Count > 2)

#Top Average Medicare to Total Payment providers by Procedure and State (Above 1 Quartile Total Payments)
map5 <- PercTot %>% arrange(desc(MedPerc))
map5 <- map5[1:10,]
ggplot(data=map5, aes(x=reorder(Provider.Name, MedPerc),y=MedPerc, fill = DRGState)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("                       Top 10 Total Medicare Payment over Total Payment") +
  ylab('Fraction of Total Medicare Payment to Total Payment') +
  xlab('State') + 
  theme(legend.position="bottom")

summary(PercTot$MedPerc)

#Scaling the precentages
PercTotSc <- PercTot %>% mutate(MedPerc = scale_this(MedPerc),
                            CovPerc = scale_this(CovPerc),
                            DisPerc = scale_this(DisPerc),
                            MedPerDis = scale_this(MedPerDis))
summary(PercTotSc)
map6 <- PercTotSc %>% arrange(desc(MedPerc))
map6 <- map6[1:10,]
ggplot(data=map6, aes(x=reorder(Provider.Name, MedPerc),y=MedPerc, fill = DRGState)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("                       Top 10 Total Medicare Payment over Total Payment") +
  ylab('Fraction of Total Medicare Payment to Total Payment') +
  xlab('State') + 
  theme(legend.position="bottom")

####Adding to final DB
FinalDB <- FinalDB %>% left_join(PercTot, by='DRGState')

#left_join added too many variables
str(train)
str(PercTot)
train <- train[,-3]
train <- train %>% left_join(PercTotSc, by='DRGState', type="left", match="first")

###############join
library(plyr)
train <- train %>% join(PercTot, by = 'DRGState', type="left", match="first")
str(train)

######5. Percentage of medicare payment per provider or other. 
mutate(Percentage = paste(round((Medicare_Payment/sum(Medicare_Payment))*100,2),"%",sep = ""))

#####Dimension Reduction Techniques
#FinalDB
summary(FinalDB)

#####PCA

##Attempt 1
str(train)
train <- train[,-3]
train <- train[,-12]

trainPCADB <- train %>% select(Provider.Id, ScTotPay, ScTotCov, ScTotMed, ScTotDis,
                             PayMedDiffSc, CovMedDiffSc, CovTotDiffSc, MedPerc, DisPerc, CovPerc,
                             MedPerDis)

#install.packages('rAverage')
library(rAverage)
outlier.replace(trainPCADB, whichModel = NULL, alpha = .025, value = NULL)

str(trainPCADB)
anyNA(trainPCA)
summary(trainPCA)
library(corrplot)
corrplot(corr = cor(trainPCA))

trainPCA <- prcomp(trainPCADB,
                   center = TRUE,
                   scale. = TRUE) 

print(trainPCA)
summary(trainPCA)
plot(trainPCA)

plot(trainPCA, type = 'barplot')

summary(trainPCA$x)

trainPCA[,1:2]

TrainPCAOut <- trainPCA$x
summary(TrainPCAOut)

TrainPCAOut %>% filter(num < 100)

Filters

library(devtools)
#install_github("vqv/ggbiplot")
#Next, you can call ggbiplot on your PCA:
  
library(ggbiplot)

ggbiplot(trainPCA, choices = 1:2)
ggbiplot(trainPCA)

screeplot(trainPCA, type = 'line', main = 'Scree Plot')

pcaCharts(trainPCA)

trainPCA$rotation 

#install.packages('NormalizeMets')
#install.packages('limma')

##Attempt 2 
trainPCA2DB <- train %>% select(Provider.Id, ScTotPay, ScTotMed,PayMedDiffSc, CovMedDiffSc, 
                              CovTotDiffSc, MedPerc, DisPerc, CovPerc, MedPerDis)

trainPCA2 <- prcomp(trainPCA2,
                   center = TRUE,
                   scale. = TRUE) 

print(trainPCA2)
summary(trainPCA2)
plot(trainPCA2)

trainPCA2 %>% arrange_(desc(x))

ggbiplot(trainPCA2)

ggbiplot(trainPCA2, ellipse=TRUE)

plot(pc$x[, 1], pc$x[, 2], col = trainPCA2DB, main = "PCA", xlab = "PC1", ylab = "PC2")

plot(trainPCA2$x[,1],trainPCA2$x[,2], main = 'PCA', xlab = 'PC1', ylab = 'PC2')

summary(trainPCA2$x)

screeplot(trainPCA2, type = 'line', main = 'Scree Plot')

biplot(trainPCA2)

trainPCA2x <- as.data.frame(trainPCA2$x)

trainPCA2x <- trainPCA2x %>% filter(trainPCA2x$PC1 < 15, trainPCA2x$PC2 < 15)
summary(trainPCA2x)

plot(trainPCA2x$PC1,trainPCA2x$PC2, main = 'PCA', xlab = 'PC1', ylab = 'PC2')
ggbiplot(trainPCA)

variance.explained = prop.table(trainPCA2$sdev)
plot(variance.explained, main = 'Variable Importance')

#install.packages('car')
car::outlierTest(trainPCA2$x)

summary(trainPCA2)

max(trainPCA2$x)

#Attempt 3 
summary(trainPCA2$x)
print(trainPCA2)

trainPCA3DB <- train %>% select(Provider.Id, ScTotMed,PayMedDiffSc, 
                                CovTotDiffSc, MedPerc, DisPerc, CovPerc, MedPerDis)

trainPCA3 <- prcomp(trainPCA3DB,
                    center = TRUE,
                    scale. = TRUE) 
print(trainPCA3)
summary(trainPCA3)
summary(trainPCA3$x)

plot(trainPCA3$x[,1],trainPCA2$x[,2], main = 'PCA', xlab = 'PC1', ylab = 'PC2')

##SVD
library(dplyr)
str(trainSVD)
trainSVD <- train %>% select(Provider.Id, ScTotPay, ScTotCov, ScTotMed, ScTotDis,
                             PayMedDiffSc, CovMedDiffSc, CovTotDiffSc, MedPerc, DisPerc, CovPerc,
                             MedPerDis)

trainSVD <- svd(trainSVD)

?svd

print(trainSVD)
summary(trainSVD)
plot(trainSVD)

trainSVD$d

#Variable Importance
variance.explained = prop.table(trainSVD$d^2)
variance.explained
plot(variance.explained)

####Kmeans

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20) #This means that R will try 20 different random starting assignments and then select the one with the lowest within cluster variation.
irisCluster

#####DBScan
#install.packages('fpc')
library(fpc)
modl <- fpc::dbscan(trainPCA2DB, eps = 50, MinPts = 10)
trainPCA2DBdbscan <- trainPCA2$x[,1:2]
plot(trainPCA2DBdbscan, main = "Healthcare Variables breakdown", type="p", col=modl$cluster, frame = FALSE)

#From Paulina- making interactive table
formatCurrency(
  datatable(
    Charges_vs_Payment,
    rownames = FALSE, 
    colnames=c("Provider ID","Provider State","Charges Covered","Medicare Payment","Charges Not Covered","% Not Covered"),
    options = list(columnDefs=list(list(className = 'dt-center', targets = 0:5)))
  ),
  c("Covered_Charges","Medicare_Payment","Not_Covered")
)

#Rerunning dimensionality reduction

#install.packages('caret')
library(caTools)
library(plyr)
split = sample.split(payment, SplitRatio = 0.5)
train = subset(payment, split == TRUE)
test = subset(payment, split == FALSE)

train <- train %>% mutate(DRGState = paste(Provider.State, 
                                           DRG.Definition, sep="_"))

train <- select(train, Provider.Id, Provider.Name, Provider.Zip.Code, Average.Covered.Charges,
                Average.Total.Payments, Average.Medicare.Payments, Total.Discharges, 
                DRGState) 

#Scaled Absolute numeric values
train <- train %>% mutate(                    
  ScTotPay = scale_this(Average.Total.Payments),
  ScTotCov = scale_this(Average.Covered.Charges),
  ScTotMed = scale_this(Average.Medicare.Payments),
  ScTotDis = scale_this(Total.Discharges))

#Adding values from DIfferences Table
train <- train %>% join(TotPaymMed1Sc, by='DRGState', type="left", match="first")

#Adding scaled values from the percentages variables
train <- train[,-3]
train <- train %>% left_join(PercTotSc, by='DRGState', type="left", match="first")

##Dimensionality Reduction


##PCA 1
trainPCADB <- train %>% select(Provider.Id, ScTotPay, ScTotCov, ScTotMed, ScTotDis,
                               PayMedDiffSc, CovMedDiffSc, CovTotDiffSc, MedPerc, DisPerc, CovPerc,
                               MedPerDis)

trainPCA <- prcomp(trainPCADB,
                   center = TRUE,
                   scale. = TRUE) 

print(trainPCA)
summary(trainPCA)
screeplot(trainPCA, type = 'line', main = 'Scree Plot')

ggbiplot(trainPCA)

biplot(trainPCA2)

trainPCA$scale

trainPCAx <- as.data.frame(trainPCA2$x)

trainPCAx <- trainPCA2x %>% filter(trainPCA2x$PC1 < 15, trainPCA2x$PC2 < 15)
summary(trainPCA2x)

plot(trainPCAx$PC1,trainPCAx$PC2, main = 'PCA', col = 'red' ,xlab = 'PC1', ylab = 'PC2')

?plot

plot(trainPCAx$PC1,trainPCA2x$PC2, main = 'PCA', xlab = trainPCAx, ylab = 'PC2')
ggbiplot(trainPCA)

variance.explained = prop.table(trainPCA$center)
plot(variance.explained, main = 'Center')


variance.explained = prop.table(trainPCA$sdev)
plot(variance.explained, main = 'Variable Importance', type = 'barplot')

trainPCA$center

trainPCA2x$PC1

#qqplot
library(car)
qqPlot(trainPCA$x[,2],pch = 20, col = c(rep("red", 33), rep("blue", 99)))     

identify(qqnorm(trainPCA$x[,2],pch = 20, col = c(rep("red", 33), rep("blue", 99))))

##PCA 2
trainPCA2DB <- train %>% select(Provider.Id, ScTotPay, ScTotMed,PayMedDiffSc, CovMedDiffSc, 
                                CovTotDiffSc, MedPerc, DisPerc, CovPerc, MedPerDis)

trainPCA2 <- prcomp(trainPCA2DB,
                    center = TRUE,
                    scale. = TRUE)

summary(trainPCA2)
screeplot(trainPCA2, type = 'line', main = 'Scree Plot')

variance.explained = prop.table(trainPCA2$sdev)
plot(variance.explained, main = 'Variable Importance', type = 'barplot')

#PCA 3
summary(trainPCA2$x)
print(trainPCA2)

trainPCA3DB <- train %>% select(Provider.Id, ScTotMed,PayMedDiffSc, 
                                CovTotDiffSc, MedPerc, DisPerc, CovPerc, MedPerDis)

trainPCA3 <- prcomp(trainPCA3DB,
                    center = TRUE,
                    scale. = TRUE) 

summary(trainPCA)
screeplot(trainPCA3, type = 'line', main = 'Scree Plot')

variance.explained = prop.table(trainPCA3$sdev)
plot(variance.explained, main = 'Variable Importance', type = 'barplot')