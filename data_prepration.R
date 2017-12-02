#Set working directory
setwd("~/Cars/Final/Github")
#load data
library(readr)
df_car_sample = read_csv("VehicleEmissions2015_CABQ-en-us.prepared.sample10000.csv")
summary(df_car_sample)

library(dplyr)
library(caret)
library(tidyr)

#Replace 0 with NA
df_car_sample$GVWR =  df_car_sample$GVWR %>% replace(.==0, NA)
df_car_sample$ODOMETER = df_car_sample$ODOMETER %>% replace(.==0, NA)

#Remove rows having NA in Oddometer as only 0.41 percentage values are missing 
df_car_sample = df_car_sample %>% drop_na(ODOMETER)

#Calculate new feature AGE using model year as age is better feature compared to model year 
#at the same time giving details about vehicles age
this_year = format(Sys.Date(), '%Y')
this_year = as.integer(this_year)
df_car_sample$AGE = this_year - df_car_sample$MODEL_YEAR

#Impute missing value present in feature GVWR
library(mice)
df_imputed_s = mice(df_car_sample, m = 5, method = "pmm", seed = 200)
df_imp = mice::complete(df_imputed_s,4)


#Recode Initial result to binary
library(plyr)
df_imp$INITIAL_RESULT = revalue(df_imp$INITIAL_RESULT, c("P"=1))
df_imp$INITIAL_RESULT = revalue(df_imp$INITIAL_RESULT, c("F"=0))

#Drop unwanted featrures
#Model year as AGE is new variable derived from model year 
#Final result is not required since we are intreseted to predict vehicle failure in advance intial result is the target feature
#Inspection year as the data is from year 2015 and hecne this is common values for all the rows for this feature 
df_work = subset(df_imp, select = -c(MODEL_YEAR,FINAL_RESULT,INSPECTION_YEAR))
#re-arrange features in dataset
df_work = df_work[c(1,2,3,4,5,6,7,8,9,10,11,13,12)]
summary(df_work)

#Checking distribution of final result variable
barplot(prop.table(table(df_work$INITIAL_RESULT)),
        col = rainbow(2),
        ylim = c(0,1),
        xlab = "0 (Fail) and 1(PASS) classes",
        main = "INITAL RESULT Distribution")


#get the varibale type
sapply(df_work, class)

#Checeking correlation between variables
library(corrplot)
corrplot(cor(df_work[c( 1, 3, 4, 5, 6, 7, 10,12)]), method = "pie")

#Create dummy variables for categorical variables 
library(dummies)
df_car_dummy = dummy.data.frame(df_work, names = c("INSPECTION_MONTH","MAKE","TRANS_TYPE","FUEL_TYPE","DUAL_EXHAUST", "CYL"), sep = "_")
names(df_car_dummy) = gsub("\\-", "_", names(df_car_dummy))

#Normalize data 
library(caret)
preObj <- preProcess(df_car_dummy[, -65], method=c("center", "scale"))
df_car = predict(preObj, df_car_dummy)


#Splitting dataset into test and training set
library(caTools)
set.seed(123)
split = sample.split(df_car$INITIAL_RESULT, SplitRatio = 0.8)
training_set = subset(df_car, split == TRUE)
test_set = subset(df_car, split == FALSE)

saveRDS(training_set, file = "training_set.rds")
saveRDS(test_set, file = "test_set.rds")

#Prepare dataset with balanced INITIAL_RESULT class distribution 
library(ROSE)
under = ovun.sample(INITIAL_RESULT ~.,
                    data = training_set,
                    method = "under",
                    N = 1494)$data
#Checking distribution of initial result variable after balancing
barplot(prop.table(table(under$INITIAL_RESULT)),
        col = rainbow(2),
        ylim = c(0,1),
        xlab = "0 (Fail) and 1(PASS) classes",
        main = "INITAL RESULT Distribution")
saveRDS(under, file = "under.rds")
