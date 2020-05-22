#-----------------------------------------------------------------------------------------------------------------------------------
#                           INSTALLING PACKAGES AND DOWNLOADING DATASET
#------------------------------------------------------------------------------------------------------------------------------------


if(!require(dplyr)) install.packages("dplyr",repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2",repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret",repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages('data.table',repos = 'http://cran.us.r-project.org')
if(!require(tidyr)) install.packages('tidyr',repos = 'http://cran.us.r-project.org')


library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(caret)
library(data.table)


data <- read.csv("AB_NYC_2019.csv")

#-----------------------------------------------------------------------------------------------------------------------------
#=============================ANALYSIS AND DATA EXPLORATION====================================================
#---------------------------------------------------------------------------------------------------------------------------------

#Analyzing the structure of the Dataset
str(data)

#Summarizing the dataset
summary(data)

#Density distribution of the price
data%>%ggplot(aes(price))+geom_density()

#Density distribution of logarithm of price
data%>%ggplot(aes(log(price)))+geom_density()

#Distribution of prices VS Neighbourhood groups
data %>% group_by(neighbourhood_group) %>%filter(price<1250)%>%ggplot(aes(x=neighbourhood_group,y=price))+geom_boxplot()

#Distribution of price VS Number of reviews
data%>%filter(price<2000)%>%ggplot(aes(x=number_of_reviews,y=price))+geom_point(size=1,alpha = 0.2)

#Distribution of prices VS reviews per month
data%>%filter(price<2000 & reviews_per_month<20)%>%ggplot(aes(x=reviews_per_month,y=price))+geom_point(size=1,alpha = 0.2)

#Distribution of neighbourhood group with respect to latitudes and longitudes
data%>% ggplot(aes(x=latitude,y=longitude,color=neighbourhood_group))+geom_point(size=1,alpha = 0.2)

#Distribution of price with latitude and longitude
data%>% filter(price<1250)%>%ggplot(aes(x=latitude,y=longitude,color=price))+geom_point()

#Distribution of price VS minimum heights
data%>% ggplot(aes(x=minimum_nights,y=price))+geom_point(size=1,alpha = 0.2)

#Distribution of price VS availability_365
data%>%ggplot(aes(x=availability_365,y=price))+geom_point(size=1,alpha = 0.2)



#Extracting year from last_review
last_review_date<-as.Date(data$last_review)
f_last_review_year = factor(format(last_review_date,'%Y'))
data<-data%>%mutate(Annual=f_last_review_year)

#Distribution of Prices VS Last review year
data%>%filter(price<5000)%>%ggplot(aes(x=Annual,y=price))+geom_point(size=1,alpha = 0.2)

#Distribution of Prices VS host_listings_count
data%>%ggplot(aes(x=calculated_host_listings_count,y=price))+geom_point(size=1,alpha = 0.2)

#Distribution of median price of neighbourhoods
data%>%group_by(neighbourhood)%>%summarize(nbg_median=median(price),Rank=rank(-nbg_median))%>%filter(nbg_median>150)%>%ggplot(aes(x=neighbourhood, y=nbg_median))+geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Distribution of price VS Room Type
data%>%filter(price<1000)%>%ggplot(aes(room_type,price))+geom_boxplot()

#======================FILLING NA'S OF THE DATASET===============================================

#Evaluating the average year by rounding of the mean to the nearest integer
avg_year<-round(mean(as.numeric(paste(data$Annual[!is.na(data$Annual)]))))

#Assigning the missing points of dataset with the calculated avg_year
data$Annual[is.na(data$Annual)]=factor(avg_year)

#Assigning the datasets with missing reviews_per_month with the median of the present values
data$reviews_per_month[is.na(data$reviews_per_month)]=median((data$reviews_per_month[!is.na(data$reviews_per_month)]))


#===================DATAFRAME BUILDING AND SPLITTING DATA===========================================

#CREATING THE DATAFRAME 'M' WHICH WILL BE UTILIZED FURTHER
M <-data.frame(data$price,data$latitude,data$longitude,data$neighbourhood_group,data$neighbourhood,data$room_type,data$number_of_reviews,data$calculated_host_listings_count,data$minimum_nights,data$reviews_per_month,data$Annual)


#PARTIONING THE DATAFRAME 'M' INTO validation_set AND train IN 0.2:0.8 RATIO
t1_index_ <- createDataPartition(y = M$data.price, times = 1, p = 0.2, list = FALSE)
train <- M[-t1_index_,]
validation_set <- M[t1_index_,]

#PARTIONING THE train INTO train_set AND test_set IN 0.75:0.25 RATIO
t2_index_ <- createDataPartition(y = train$data.price, times = 1, p = 0.25, list = FALSE)
train_set <- M[-t2_index_,]
test_set <- M[t2_index_,]
#THUS FINAL RATION OF SPLITIING train_set:test_set:validation_set = 0.6:0.2:0.2

#Setting parameter train_control to ten 10 cross validations
train_control<- trainControl(method="cv", number=10)



#===================MODEL BUILDING==================================================================

#--------------------------------------------------------------------------------------------------
#                                 MODEL 1: SIMPLE LINEAR REGRESSION
#-------------------------------------------------------------------------------------------------------------

#TRAINING THE MODEL ON TRAIN_SET
MODEL_1 <- train(data.price ~ ., data = train_set,
                 method = "lm")

#DISPLAYING SUMMARY OF THE MODEL
summary(MODEL_1)

#PREDICTING PRICE FOR TEST_SET
MODEL_1_predict<-predict(linear_model,test_set)

#CALCULATING RMSE FOR THE TEST_SET PREDICTION
RMSE(MODEL_1_predict,test_set$data.price)

#PLOTTING THE GRAPHS DEPICTING THE FIT OF THE MODEL
plot(MODEL_1$finalModel)

#STORING THE OBTAINED RMSE ERROR IN RMSE_results DATAFRAME
RMSE_results <- data_frame(method = "SIMPLE LINEAR REGRESSION MODEL", RMSE = RMSE(Model_1_predict,test_set$data.price))

#----------------------------------------------------------------------------------------------------------
#                 MODEL 2: LINEAR REGRESSION MODEL WITH LOGARIHTHMIC SCALING OF PRICE
#--------------------------------------------------------------------------------------------------------------
  
  
#TRAINING THE MODEL ON TRAIN_SET
MODEL_2 <- train(log(1+data.price) ~ ., data = train_set,
                   method = "lm")

#DISPLAYING SUMMARY OF THE MODEL
summary(MODEL_2)

#PREDICTING PRICE FOR TEST_SET
MODEL_2_predict<-predict(MODEL_2,test_set)

#CALCULATING RMSE FOR THE TEST_SET PREDICTION
RMSE(exp(MODEL_2_predict)+1,test_set$data.price)

#PLOTTING THE GRAPHS DEPICTING THE FIT OF THE MODEL
plot(MODEL_2$finalModel)

#BINDING THE OBTAINED RMSE ERROR WITH RMSE_results DATAFRAME
RMSE_results <- bind_rows(RMSE_results,data_frame(method="LINEAR REGRESSION MODEL WITH LOGARIHTHMIC SCALING OF PRICE",RMSE = RMSE(exp(Model_2_predict)-1,test_set$data.price) ))

#-------------------------------------------------------------------------------------------------------
#                             MODEL 3: XGBOOST LINEAR MODEL
#------------------------------------------------------------------------------------------------------

#TRAINING THE MODEL ON TRAIN_SET
MODEL_3 <- train(log(1+data.price) ~ ., data = train_set,
                 method = "xgbLinear",tuneGrid = expand.grid(nrounds=50,lambda=c(.01,.1),alpha =c(.01, .1),eta = .3))

#DISPLAYING SUMMARY OF THE MODEL
summary(MODEL_3)

#PREDICTING PRICE FOR TEST_SET
MODEL_3_predict<-predict(MODEL_3,test_set)

#CALCULATING RMSE FOR THE TEST_SET PREDICTION
RMSE(exp(MODEL_3_predict)-1,test_set$data.price)

#BINDING THE OBTAINED RMSE ERROR WITH RMSE_results DATAFRAME
RMSE_results <- bind_rows(RMSE_results,data_frame(method="XGBOOST LINEAR MODEL",RMSE = RMSE(exp(Model_3_predict)-1,test_set$data.price) ))

----------------------------------------------------------------------------------------------------------------------------------------
 
#DISPLAYING THE RESULTS OF THE ABOVE MODEL
RMSE_results %>% knitr::kable()


#----------------------------------------------------------------------------------------------------------------------------
#                          VALIDATION OF THE MODEL  
#--------------------------------------------------------------------------------------------------------------------------------

#XGBTree IS THE MODEL WHICH PREDICTS THE LEAST RMSE FOR THE TRAININ DATA

#PREDICTING PRICE FOR TEST_SET BY USING MODEL_3
VALIDATION_predict<-predict(MODEL_3,validation_set)

#CALCULATING RMSE FOR THE VALIDATION_SET
VALIDATION_RMSE<-RMSE(exp(VALIDATION_predict)-1,validation_set$data.price)

#DISPLAYING THE VALIDATION SET RMSE 
tibble(method = "VALIDATION_SET RMSE USING MODEL 3", RMSE = VALIDATION_RMSE) %>% knitr::kable()


#THE HIGH RMSE VALUES ARE DUE TO THE OUTLIERS PRESENT IN THE FORM OF HIGH PRICE HOUSES

#THUS THIS MAKES IT NECESSARY TO CALCULATE AN RMSE WITHOUT TAKING INTO CONSIDERATION THE OUTLIERS

#Calculating the RMSE for the validation data_Set for the prices lying below 0.9
index<-validation_set$data.price < quantile(validation_set$data.price,0.9)
VALIDATION_RMSE_<-RMSE(exp(VALIDATION_predict[index])-1,validation_set$data.price[index])

#RMSE FOR THE VALIDATIONS_SET NOT CONTAINING THE OUTLIERS
tibble(method = "VALIDATION_SET RMSE AFTER FILTERING OUT OUTLIERS", RMSE = VALIDATION_RMSE_) %>% knitr::kable()

#THUS IT CAN BE SEEN DUE TO A LOWER RMSE VALUE THAT THE OUTLIERS PRESENT EARLIER HAD INCREASED THE RMSE VALUE, REMOVING THEM REDUCES RMSE VALUE



