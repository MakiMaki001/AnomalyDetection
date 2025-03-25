#Assignment 2 - Probability of default
setwd('C:\\Users\\marko\\OneDrive\\Documents\\Columbia\\Anomaly Detection - Summer 2019')
getwd()

#install.packages('AnomalyDetection')
#install.packages('Rtools')
#install.packages('digest')
library(Rtools)
library(dplyr)
#install.packages('ggplot2')
library(ggplot2)
#install.packages("gridExtra")
library(digest)
#install.packages('gridExtra')
library(gridExtra)
library(rpart)
library(corrplot)

data <- read.csv('XYZloan_default.csv')
head(data)
str(data)

str(data)
summary(data)
data[13:31]


#From Module 2- Probability of Default Notes
dim(data)

#Sections of features
#AP: Application
#CD: Call Detail
#TD: Credit Center
#CR: Credit Bureau
#PA: Call Detail 
#MB: Mobile Info

## 75% of the sample size
smp_size <- floor(0.50 * nrow(data))

set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]
dim(train)

str(train)

#Number of NA's on TD columns
colSums(is.na(train[27:72]))/nrow(train)

summary(colSums(is.na(train))/nrow(train))

#Removing columns with high number of NA's
train <- train[,-(42:74)]
train <- train[-(29:33)]

data[,colSums(is.na(data) < nrow(data))]

#Seperating types of variables
#AP
ap <- train[4:12]
ap <- cbind(ap, train$loan_default)
ap$AP006 <- as.numeric(ap$AP006)
ap$AP005 <- as.numeric(ap$AP005)

#TD
td <- train[13:41]
td <- cbind(td, train$loan_default)
td[is.na(td)] <- 0
anyNA(td)

#CR
cr <- train[37:57]
cr <- cbind(cr, train$loan_default)
anyNA(cr)

#PA
pa <- train[58:156]
pa <- cbind(pa, train$loan_default)
pa[is.na(pa)] <- 0
anyNA(pa)

#Cd
cd <- train[157:334]
cd <- cbind(cd, train$loan_default)
cd[is.na(cd)] <- 0
anyNA(cd)

#MB
mb <- train[335:341]
mb <- cbind(mb, train$loan_default)
mb[is.na(mb)] <- 0
anyNA(mb)

corrplot(corr = cor(mb))

#### Plot barcharts for a categorical variable (AP006- OS_Type)

df1 <- train %>%
  group_by(AP006) %>%
  summarise(X_counts = n(),
            Y_counts = sum(loan_default)) %>%
  mutate( Y_by_X_pcnt = Y_counts/X_counts,
          X_pcnt = X_counts/sum(X_counts))
df1

theme_set(theme_bw())

train %>%
  group_by(AP006) %>%
  summarise(X_counts = n(),
            Y_counts = sum(loan_default))

# Draw plot
p1 <- ggplot(df1, aes(x=AP006, y=X_pcnt, fill=AP006)) + 
  geom_bar(stat="identity", width=0.8) + 
  labs(title="One-way frequency barchart", 
       subtitle="AP006", 
       caption="source: data") + 
  coord_cartesian(ylim=c(0,1)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
p1

# Draw plot
p2 <- ggplot(df1, aes(x=AP006, y=Y_by_X_pcnt, fill=AP006)) + 
  geom_bar(stat="identity", width=0.8) + 
  labs(title="% of Y by X", 
       subtitle="AP006", 
       caption="source: data") + 
  coord_cartesian(ylim=c(0,1)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

require(gridExtra)
grid.arrange(p1, p2, nrow=2,heights=c(1, 1))

#Dealing with a continuous variable (AP001- YR_Age)
# Compute the frequency of AP001
df1 <- train %>%
  group_by(AP001) %>%
  summarise(X_counts = n(),
            Y_counts = sum(loan_default)) %>%
  mutate( Y_by_X_pcnt = Y_counts/X_counts,
          X_pcnt = X_counts/sum(X_counts))
df1

theme_set(theme_bw())

train %>%
  group_by(AP001) %>%
  summarise(X_counts = n(),
            Y_counts = sum(loan_default))

# Draw plot
p1 <- ggplot(df1, aes(x=AP001, y=X_pcnt, fill=AP001)) + 
  geom_bar(stat="identity", width=0.8) + 
  labs(title="One-way frequency barchart", 
       subtitle="AP001", 
       caption="source: data") + 
  coord_cartesian(ylim=c(0,1)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
p1

# Draw plot
p2 <- ggplot(df1, aes(x=AP001, y=Y_by_X_pcnt, fill=AP001)) + 
  geom_bar(stat="identity", width=0.8) + 
  labs(title="% of Y by X", 
       subtitle="AP001", 
       caption="source: data") + 
  coord_cartesian(ylim=c(0,1)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

require(gridExtra)
grid.arrange(p1, p2, nrow=2,heights=c(1, 1))


#### Plot barcharts for a categorical variable (AP003- CD Education)
max(train$AP003)
min(train$AP003)

df3 <- train %>%
  group_by(AP003) %>%
  summarise(X_counts = n(),
            Y_counts = sum(loan_default)) %>%
  mutate( Y_by_X_pcnt = Y_counts/X_counts,
          X_pcnt = X_counts/sum(X_counts))
df3

theme_set(theme_bw())
head(train$AP001)

# Draw plot
p1 <- ggplot(df3, aes(x=AP003, y=X_pcnt, fill=AP003)) + 
  geom_bar(stat="identity", width=0.8) + 
  labs(title="One-way frequency barchart", 
       subtitle="AP003", 
       caption="source: data") + 
  coord_cartesian(ylim=c(0,1)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
p1

# Draw plot
p2 <- ggplot(df3, aes(x=AP003, y=Y_by_X_pcnt, fill=AP003)) + 
  geom_bar(stat="identity", width=0.8) + 
  labs(title="% of Y by X", 
       subtitle="AP003", 
       caption="source: data") + 
  coord_cartesian(ylim=c(0,1)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
p2

require(gridExtra)
grid.arrange(p1, p2, nrow=2,heights=c(1, 1))

#### Plot barcharts for a categorical variable (AP007- Level_Appl_City)
max(train$AP007)
min(train$AP007)
summary(train$AP007)

df4 <- train %>%
  group_by(AP007) %>%
  summarise(X_counts = n(),
            Y_counts = sum(loan_default)) %>%
  mutate( Y_by_X_pcnt = Y_counts/X_counts,
          X_pcnt = X_counts/sum(X_counts))
df4

theme_set(theme_bw())
head(train$AP001)

# Draw plot
p1 <- ggplot(df4, aes(x=AP007, y=X_pcnt, fill=AP007)) + 
  geom_bar(stat="identity", width=0.8) + 
  labs(title="One-way frequency barchart", 
       subtitle="AP007", 
       caption="source: data") + 
  coord_cartesian(ylim=c(0,1)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
p1

# Draw plot
p2 <- ggplot(df4, aes(x=AP007, y=Y_by_X_pcnt, fill=AP007)) + 
  geom_bar(stat="identity", width=0.8) + 
  labs(title="% of Y by X", 
       subtitle="AP007", 
       caption="source: data") + 
  coord_cartesian(ylim=c(0,1)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
p2

require(gridExtra)
grid.arrange(p1, p2, nrow=2,heights=c(1, 1))

#Working with AP variable
str(ap)
ap$AP006 <- as.numeric(ap$AP006)
ap$AP005 <- as.numeric(ap$AP005)

corrplot(corr = cor(ap))
pairs(ap)

#Lasso Model
dataset2$market <- as.factor(dataset2$market)
levels(dataset2$market)
str(dataset2)

library(glmnet)
x = model.matrix(price ~ .,data=datasetBest)
y = dataset8$price

lassoModel = cv.glmnet(x,y, alpha=1) # Note default for alpha is 1 which corresponds to Lasso
lassoModel

lassoModel$bestlambda
sapply

coef(lassoModel)
x = model.matrix(price ~ .,data=training_set2)
cor(dataset2$price, dataset2$maximum_nights)

#install.packages('glmnet')
library(glmnet)
x = model.matrix(loan_default ~ .,data=data)
y = data$loan_default

#AP: Application
#CD: Call Detail
#TD: Credit Center
#CR: Credit Bureau
#PA: Call Detail 
#MB: Mobile Info

#Selecting important variables from the AP parameters: #AP: Application
str(ap)
library(glmnetUtils)
x <- model.matrix(ap$`train$loan_default`~.,data = ap)
y <- ap$`train$loan_default`

aplasso <- cv.glmnet(x,y, alpha = 1)
aplasso$lambda.1se
plot(coef(aplasso))
coef(aplasso)

apbest <- as.data.frame(cbind(ap$AP001,ap$AP002,ap$AP003,ap$AP004,ap$AP005,ap$AP006,
                ap$AP007,ap$AP008))
str(apbest)
#Failed attempt at getting a for loop involved int my analysis
for (i in 1:nrow(ap)) {
  if (cor(ap)>.01){
    i^2 else {i}}

#Selecting important variables with TD: #TD: Credit Center 
x <- model.matrix(td$`train$loan_default`~., data = td)
y <- td$`train$loan_default`
tdlasso <- cv.glmnet(x,y,alpha=1)
plot(coef(tdlasso))
tdbes <- as.data.frame(cbind(td$TD005, td$TD009, td$TD013))

ncol(td)

#CD: Call Detail
str(cd)
desc(cor(cd))
x <- model.matrix(cd$`train$loan_default`~.,data = cd)
y <- cd$`train$loan_default`
cdlasso <- cv.glmnet(x,y,alpha=1)
plot(coef(cdlasso))
coef(cdlasso)
cdbes <- as.data.frame(cbind(cd$CD123))
head(cdbes)

ncol(cd)

#CR: Credit Bureau
str(cr)
cor(cr)
x <- model.matrix(cr$`train$loan_default`~.,data = cr)
y <- cr$`train$loan_default`

crlasso <- cv.glmnet(x,y,alpha=1)
plot(coef(crlasso))
coef(crlasso)
list(desc(coef(crlasso)))

crbes <- as.data.frame(cbind(cr$CR007, cr$CR008, cr$CR015, cr$CR016, cr$CR017,
               cr$CR019))
ncol(cr)
head(crbes)
head(cr)
#PA: Call Detail
str(pa)
summary(pa)
cor(pa)
x <- model.matrix(pa$`train$loan_default`~.,data = pa)
y <- pa$`train$loan_default`

palasso <- cv.glmnet(x,y,alpha=1)
coef(palasso)
#No significant variables

#MB: Mobile Info
str(mb)
summary(mb$MB007)
mb$MB007 <- as.numeric(mb$MB007)
cor(mb)
x <- model.matrix(mb$`train$loan_default`~., data = mb)
y <- mb$`train$loan_default`

mblasso <- cv.glmnet(x,y,alpha=1)
plot(coef(mblasso))
corrplot(corr = cor(mb))
#Lasso model 0'ed out all the featurs for mb

ggplot(data, aes(data$MB007, y = count(data$loan_default))) +
  geom_bar()

data %>% 
  group_by(data$MB007) %>%
  

plot.data = summarise(group_by(data$MB007))
colnames(plot.data) = c("x","y")
plot.data$x = as.character(plot.data$x)

mb1 <- data %>%
  group_by(data$MB007) %>%
  summarise(X_counts = n(),
            Y_counts = sum(loan_default)) %>%
  mutate( Y_by_X_pcnt = Y_counts/X_counts,
          X_pcnt = X_counts/sum(X_counts))
desc(mb1$X_counts)
levels(data$MB007)
data %>%
  group_by(data$AP007) %>%
  summarise(X_counts = n(),
            Y_counts = sum(loan_default))

mb1 %>%
  group_by(mb1$`data$MB007`)%>%
  desc(mb1$X_counts)
mb1

mb1 <- mb1 %>% 
  filter(mb1$X_counts > 2000) %>%
  arrange(desc(mb1$X_counts))

mb1
ggplot(mb1, aes(x = X_counts, y = data$MB007))+
  geom_bar()

str(data$MB007)
levels(data$MB007)
# Draw plot
p1 <- ggplot(mb1, aes(x=data$MB007, y=X_pcnt, fill=data$MB007)) + 
  geom_bar(stat="identity", width=0.8) + 
  labs(title="One-way frequency barchart", 
       subtitle="MB007", 
       caption="source: data")
p1

plot.data = summarise(group_by(train[,c("MB007","loan_default","title")],yy),count = n(),sum(views))
train$

#install.packages('glmnetUtils')

#install.packages('corrplot')
library(corrplot)          
corrplot(corr = cor(td))

#Combining

bestft <- cbind(apbest, crbes, tdbes, cdbes, train$loan_default)
corrplot(corr = cor(bestft))
str(bestft)

apsq <- apbest^2
crsq <- crbes^2
apcrsq <- cbind(apsq, crsq)

bestft2 <- cbind(apsq, crsq, tdbes, cdbes, train$loan_default)
corrplot(corr = cor(bestft2))

rm(bestft)

st

ncol(bestft)

#Writing for loops for features: 

#have to do lapply (or list apply)
#lapply(varlist, function name)
#Group_by should have group_by_ when within a function

