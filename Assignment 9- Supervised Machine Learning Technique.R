setwd('C:\\Users\\marko\\OneDrive\\Documents\\Columbia\\Anomaly Detection - Summer 2019')

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
library(h2o)
library(mice)
library(Hmisc)
library(anytime)
#install.packages('purr')
library(purrr)
library(tibble)
library(h2o)

data <- read.csv('XYZloan_default.csv')
data2 <- read.csv('XYZloan_default.csv')

nrow(data$loan_default==1)
summary(data$loan_default)
15488/64512
#Seperating and removing response variable from the dataset
# Specify the response variable
str(data)
response <- "loan_default"
# Make the response variable a categorical variable
data[[response]] <- as.factor(data[[response]])  

str(response)

#Remove date variable since anydate was causing too many NA's
str(data)
data <- data[,-8]

#Apps 
data$AP006
data$AP006 <- as.numeric(data$AP006)

#Tanslation for apps
#1 - Android
#2 - api 
#3 - h5
#4 - ios

#Sections of features
#AP: Application
#CD: Call Detail
#TD: Credit Center
#CR: Credit Bureau
#PA: Call Detail 
#MB: Mobile Info

#Finding which columns have NA's
colnames(data)[colSums(is.na(data)) > 0]

#Simple way to impute 0 for all NA values
data[is.na(data)] <- 0

#Create predictor variable
## Exclude the variable 'Time' 
predictors <- setdiff(names(data), c(response)) 

#Capping and Flooring
fun <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

datacapfl <- fun(data)

str(data)

def <- data[,3]
data <- data[,-3]

#Split the data with H20
h2o.init()
h2odata <- as.h2o(data)

splits <- h2o.splitFrame(
  data = h2odata, 
  ratios = c(0.3,0.3),   # the ratios should sum up to to be less than 1.0. 
  destination_frames = c("train", "valid", "test"), seed = 1234
)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]

#Set the ratios between target = 0 and target = 1 
#to be 1:1, 1:2, 1:3, and 1:4. You will get four model populations.

#Running individual random forest

##The parameters that allow for the model to allow for under/oversampling

#sample_rate	
#Row sample rate per tree (from 0.0 to 1.0) Defaults to 0.6320000291.

#sample_rate_per_class	
#A list of row sample rates per class (relative fraction for each class, 
#from 0.0 to 1.0), for each tree

#Create predictor variable
predictors <- colnames(data[1:377])

#1 based on the basic training model- so 1 to 1
rf_model <- h2o.randomForest(        
  training_frame = train,       
  validation_frame = valid,     
  x=predictors,                       
  y='loan_default',                         
  model_id = "rf_model",      
  ntrees = 2000, #2000 is recommended                
  max_depth = 30, #30 is recommended               
  stopping_rounds = 2,          
  stopping_tolerance = 1e-2,    
  score_each_iteration = T,     
  seed=1234)                 

str(train)

rf_model
?h2o.randomForest
## Get the AUC on the validation set
h2o.auc(h2o.performance(rf_model, newdata = valid)) 

Rf_predictions<-h2o.predict(object = rf_model,newdata = valid)
Rf_predictions

h2o.performance(rf_model)

#First chart
plot(h2o.performance(rf_model), type='roc')



##2 based on the basic training model- so 1 to 2
rate_per_class_list <- c(.5, .5)
rf_model2 <- h2o.randomForest(        
  training_frame = train,       
  validation_frame = valid,     
  x=predictors,                       
  y='loan_default',                         
  model_id = "rf_model2",      
  ntrees = 2000, #2000 is recommended                
  max_depth = 30, #30 is recommended               
  stopping_rounds = 2,          
  stopping_tolerance = 1e-2,    
  score_each_iteration = T,     
  seed=1234,
  sample_rate_per_class = rate_per_class_list)

## Get the AUC on the validation set
h2o.auc(h2o.performance(rf_model2, newdata = valid)) 

Rf_predictions<-h2o.predict(object = rf_model2,newdata = valid)
Rf_predictions

#2nd chart
plot(h2o.performance(rf_model2), type='roc')

h2o.performance(rf_model2)

##3 based on the basic training model- so 1 to 3
rate_per_class_list <- c(.33, .67)
rf_model3 <- h2o.randomForest(        
  training_frame = train,       
  validation_frame = valid,     
  x=predictors,                       
  y='loan_default',                         
  model_id = "rf_model3",      
  ntrees = 200, #2000 is recommended                
  max_depth = 10, #30 is recommended               
  stopping_rounds = 2,          
  stopping_tolerance = 1e-2,    
  score_each_iteration = T,     
  seed=1234,
  sample_rate_per_class = rate_per_class_list)

## Get the AUC on the validation set
h2o.auc(h2o.performance(rf_model3, newdata = valid)) 

h2o.auc(h2o.performance(rf_model3, newdata = test)) 

?plot

Rf_predictions<-h2o.predict(object = rf_model3,newdata = valid)
Rf_predictions

#3rd chart
plot(h2o.performance(rf_model3), type='roc')

h2o.performance(rf_model3)

##4 based on the basic training model- so 1 to 4
rate_per_class_list <- c(.25, .75)
rf_model4 <- h2o.randomForest(        
  training_frame = train,       
  validation_frame = valid,     
  x=predictors,                       
  y='loan_default',                         
  model_id = "rf_model2",      
  ntrees = 2000, #2000 is recommended                
  max_depth = 30, #30 is recommended               
  stopping_rounds = 2,          
  stopping_tolerance = 1e-2,    
  score_each_iteration = T,     
  seed=1234,
  sample_rate_per_class = rate_per_class_list)

## Get the AUC on the validation set
h2o.auc(h2o.performance(rf_model4, newdata = valid)) 

Rf_predictions<-h2o.predict(object = rf_model4,newdata = valid)
Rf_predictions

#4th chart- .25, .75 sample
plot(h2o.performance(rf_model4), type='roc')
h2o.performance(rf_model4)

#5th chart- .1 .9 sample
rate_per_class_list <- c(.1, .9)
rf_model5 <- h2o.randomForest(        
  training_frame = train,       
  validation_frame = valid,     
  x=predictors,                       
  y='loan_default',                         
  model_id = "rf_model2",      
  ntrees = 2000,                 
  max_depth = 30,                
  stopping_rounds = 2,          
  stopping_tolerance = 1e-2,    
  score_each_iteration = T,     
  seed=1234,
  sample_rate_per_class = rate_per_class_list)

## Get the AUC on the validation set
h2o.auc(h2o.performance(rf_model5, newdata = valid)) 

h2o.performance(rf_model5)

Rf_predictions<-h2o.predict(object = rf_model5,newdata = valid)

#5th chart- sample
plot(h2o.performance(rf_model5), type='roc')

#6th Model- .9 .1 sample
rate_per_class_list <- c(.9, .1)
rf_model6 <- h2o.randomForest(        
  training_frame = train,       
  validation_frame = valid,     
  x=predictors,                       
  y='loan_default',                         
  model_id = "rf_model6",      
  ntrees = 2000,                 
  max_depth = 30,                
  stopping_rounds = 2,          
  stopping_tolerance = 1e-2,    
  score_each_iteration = T,     
  seed=1234,
  sample_rate_per_class = rate_per_class_list)

## Get the AUC on the validation set
h2o.auc(h2o.performance(rf_model6, newdata = valid)) 

Rf_predictions<-h2o.predict(object = rf_model6,newdata = valid)

h2o.performance(rf_model6)

#.9, .1- sample
plot(h2o.performance(rf_model6), type='roc')

#7th model- .1 .9 sample a
rate_per_class_list <- c(.1, .9)
rf_model7 <- h2o.randomForest(        
  training_frame = train,       
  validation_frame = valid,     
  x=predictors,                       
  y='loan_default',                         
  model_id = "rf_model6",      
  ntrees = 150,                 
  max_depth = 5,                
  stopping_rounds = 2,          
  stopping_tolerance = 1e-2,    
  score_each_iteration = T,     
  seed=1234,
  sample_rate_per_class = rate_per_class_list)

## Get the AUC on the validation set
h2o.auc(h2o.performance(rf_model7, newdata = valid)) 
##AUC on test set
h2o.auc(h2o.performance(rf_model7, newdata = test)) 

Rf_predictions<-h2o.predict(object = rf_model7,newdata = valid)

h2o.performance(rf_model7)

#.1, .9- sample
plot(h2o.performance(rf_model7), type='roc')

#Using predictors from the first assignment
h2o.init()
data2 <- select(data, loan_default, AP001,AP002,AP003,AP004,AP006, AP007, 
                AP008, CD123, TD005, TD009, TD013, CR007, 
                CR008, CR015, CR016, CR017, CR019) 

str(data)

predictors2 <- setdiff(names(data2), c(response)) 

h2odata2 <- as.h2o(data2)

splits2 <- h2o.splitFrame(
  data = h2odata2, 
  ratios = c(0.3,0.3),   # the ratios should sum up to to be less than 1.0. 
  destination_frames = c("train", "valid", "test"), seed = 1234
)
train2 <- splits2[[1]]
valid2 <- splits2[[2]]
test2  <- splits2[[3]]

#Putting it on the best performing model with all variables
rate_per_class_list <- c(.33, .67)
rf_model8 <- h2o.randomForest(        
  training_frame = train2,       
  validation_frame = valid2,     
  x=predictors2,                       
  y='loan_default',                         
  model_id = "rf_model2",      
  ntrees = 200, #2000 is recommended                
  max_depth = 10, #30 is recommended               
  stopping_rounds = 2,          
  stopping_tolerance = 1e-2,    
  score_each_iteration = T,     
  seed=1234,
  sample_rate_per_class = rate_per_class_list)

str(data2)

## Get the AUC on the validation set
h2o.auc(h2o.performance(rf_model8, newdata = valid)) 
## ## Get the AUC on the test set
h2o.auc(h2o.performance(rf_model8, newdata = test))

Rf_predictions<-h2o.predict(object = rf_model8,newdata = valid)
Rf_predictions

#3rd chart
plot(h2o.performance(rf_model8), type='roc')

h2o.performance(rf_model8)

##Running grid search
#Doing sample # try using the `sample_rate_per_class` parameter:
# downsample the Class 2, and leave the rest the same
rate_per_class_list = c(1, .4, 1, 1, 1, 1, 1)
sample_factors <- c(1, 0.5, .33, .25)
hyper_params = list(ntrees = seq(50, 150, 500), 
                     max_depth=seq(2, 5, 12), 
                    class_sampling_factors = list(c(.33, 0.66), c(.1, 0.9)))

grid <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian"),
  algorithm="randomForest",
  grid_id="rf_grid",
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  seed = 1234,                                                             
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  score_tree_interval = 10,
  balance_classes =  TRUE)

## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("rf_grid", sort_by="auc", decreasing = TRUE)    
sortedGrid

sortedGrid@hyper_names

for (i in 1:10) {
  topModels <- h2o.getModel(sortedGrid@model_ids[[i]])
  print(h2o.auc(h2o.performance(topModels, valid = TRUE)))
}

best_model <- h2o.getModel(sortedGrid@model_ids[[1]])
summary(best_model)

scoring_history <- as.data.frame(best_model@model$scoring_history)
#plot(scoring_history$number_of_trees, scoring_history$training_MSE, type="p") #training mse
#points(scoring_history$number_of_trees, scoring_history$validation_MSE, type="l") #validation mse

## get the actual number of trees
ntrees <- best_model@model$model_summary$number_of_trees


# Calculate performance measures at threshold that maximizes precision
my.pred = h2o.predict(best_model,test)
head(my.pred)
my.perf = h2o.performance(best_model, test)
str(my.perf) 
head(my.perf@metrics)

#Plotting ROC
tpr=as.data.frame(h2o.tpr(my.perf))
fpr=as.data.frame(h2o.fpr(my.perf))
ROC_out<-merge(tpr,fpr,by='threshold')
head(ROC_out)

h2o.F1(my.perf)
precision=as.data.frame(h2o.precision(my.perf))
recall=as.data.frame(h2o.recall(my.perf))
PR_out<-merge(precision,recall,by='threshold')
head(PR_out)

ggplot(PR_out, aes(x = tpr, y = precision)) +
  theme_bw() +
  geom_line() +
  ggtitle("Precision-Recall")


#Running grid search
## sort the grid models by decreasing AUC
predictors <- colnames(data[1:377])
sample_factors <- c(1, 0.5, .33, .25)
hyper_params = list(ntrees = seq(50,500,100), 
                    max_depth=seq(2,12,3),
                    list(c(1),
                         c(.5,.5), c(.20,.80)))

#2
predictors <- colnames(data[1:377])
sample_factors <- c(1, 0.5, .33, .25)
hyper_params = list(ntrees = seq(50,500,100))

grid <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian"),
  algorithm="randomForest",
  grid_id="rf_grid",# Below are is the same as h2o.gbm()
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  seed = 1234,                                                             
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  score_tree_interval = 10,
  balance_classes =  TRUE)

?h2o.grid

#Stack overflow article detailing plotting of 4 models: 
#https://stackoverflow.com/questions/44034944/how-to-directly-plot-roc-of-h2o-model-object-in-r

#Plotting different sampling approaches
list(rf_model,rf_model2,rf_model3,rf_model4) %>% 
  # map a function to each element in the list
  map(function(x) x %>% h2o.performance(valid=T) %>% 
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>% 
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')] %>% 
        # add (0,0) and (1,1) for the start and end point of ROC curve
        add_row(tpr=0,fpr=0,.before=T) %>% 
        add_row(tpr=0,fpr=0,.before=F)) %>% 
  # add a column of model name for future grouping in ggplot2
  map2(c('1 to 1 Sampling','1 to 2 Sampling','1 to 3 Sampling','1 to 4 Sampling'),
       function(x,y) x %>% add_column(model=y)) %>% 
  # reduce four data.frame to one
  reduce(rbind) %>% 
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 4,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Four Models')

  
#Plotting the 4 extreme models
list(rf_model5, rf_model6, rf_model7, rf_model8) %>% 
  # map a function to each element in the list
  map(function(x) x %>% h2o.performance(valid=T) %>% 
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>% 
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')] %>% 
        # add (0,0) and (1,1) for the start and end point of ROC curve
        add_row(tpr=0,fpr=0,.before=T) %>% 
        add_row(tpr=0,fpr=0,.before=F)) %>% 
  # add a column of model name for future grouping in ggplot2
  map2(c('.9, .1 Sampling', '.1, .9 Sampling', 'Best Ntrees Grid and 5 Depth',
         'Assignment 1 variable'),
       function(x,y) x %>% add_column(model=y)) %>% 
  # reduce four data.frame to one
  reduce(rbind) %>% 
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 4,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Four Models')
