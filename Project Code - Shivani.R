# ----------Step 1: Import all the required libraries ----------
# We will add to this as and when required
library(ggplot2)
#---- for model building----
#install.packages("gbm")
library (gbm)

# ------------------- Step 1 ends here --------------------------


# ------------------- Step 2: Gather the data -----------------

### Data is provided as .csv file and already split into Test and Train.
### The training set is comprised of the first 19 days of each month, while the test set is the 20th to the end of the month.
### Let's import the data
bike_train <- read.csv("C:/Users/siyuz/Desktop/CSP 571/Project/data/train.csv")
bike_test <- read.csv("C:/Users/siyuz/Desktop/CSP 571/Project/data/test.csv")
data2 <- read.csv("C:/Users/siyuz/Desktop/CSP 571/Project/data/data2.csv")
bike = bike_train #(for now)
head(bike, 10)

# ------------------- Step 2 ends here --------------------------



# ------------------- Step 3: Data Cleaning -----------------

    # 3b. Complete Data Perform missing value analysis and Impute if needed

  # Although we have already seen above that there are no null values in the dataset. 
  # Lets try other way to confirm
  # Checking nulls
      is.null(bike)
  # what we can infer:
  # ->There are no null values in the dataset.

    # 3c. Correct Data: Check for any invalid data points

    # 3d. Create Derived Attributes - Feature Extraction
bike$date=as.Date(substr(bike$datetime,1,10))
      bike$year = as.factor(year(bike$datetime))
      bike$month = as.factor(month(bike$datetime))
      bike$hour = as.factor(hour(bike$datetime))
      bike$wkday = as.factor(wday(bike$datetime))
      bike = bike[-c(1)]
      bike_test = bike_test[-c(1)]
      
      head(bike, 5)
      head(bike_test, 5)
# ------------------- Step 3 ends here --------------------------

# ------------- Step 4: Exploratory Data Analysis -----------

    # 4b(2) Explore categorical features
   
      # i. Check distribution of categorical variables --> pie chart
        # simple pie -->> pie(table(bike$season), main="season")

ggplot(bike, aes(x=" ",fill=year))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = " Year")+ theme_void()

ggplot(bike, aes(x=" ",fill=month))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = " Month")+theme_void()
bike$season = factor(bike$season)
ggplot(bike, aes(x=" ",fill=season))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = " Season")+theme_void()
bike$holiday = factor(bike$holiday)
ggplot(bike, aes(x=" ",fill=holiday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = " Holiday")+theme_void()

ggplot(bike, aes(x=" ",fill=wkday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = " Weekday")+theme_void()
bike$workingday = factor(bike$workingday)
ggplot(bike, aes(x=" ",fill=workingday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = " Workingday")+theme_void()
bike$weather = factor(bike$weather)
ggplot(bike, aes(x=" ",fill=weather))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = " Weather")+theme_void()

      # ii. Check how individual categorical features affects the target variable

ggplot(bike, aes(x=season, y=count, fill=year)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
    ) + labs(title="Histogram for Seasons") +  labs(x="Season", y="Count")

ggplot(bike, aes(x=year, y=count, fill=year)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) + 
  labs(title="Histogram for year") +  labs(x="year", y="Count")

ggplot(bike, aes(x=month, y=count, fill=month)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) + 
  labs(title="Histogram for month") +  labs(x="month", y="Count")

ggplot(bike, aes(x=holiday, y=count, fill=holiday)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) +   labs(title="Histogram for holiday") +labs(x="holiday", y="Count")

ggplot(bike, aes(x=wkday, y=count, fill=wkday)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) +    labs(title="Histogram for weekday") +labs(x="weekday", y="Count")

ggplot(bike, aes(x=workingday, y=count, fill=workingday)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) +    labs(title="Histogram for working day") +labs(x="working day", y="Count")

ggplot(bike, aes(x=weather, y=count, fill=weather)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) +  labs(title="Histogram for weather") +labs(x="weather", y="Count")


      # iii. Explore trends over time ---- exploring some more pairplots

ggplot(bike, aes(x=season, y=count, group=year, color=year)) + 
  stat_summary(
    fun.y=mean, 
    geom='line'
  ) + 
  stat_summary(
    fun.y=mean, 
    geom='point'
  ) + 
  labs(title="Average Count by Month Across Season") +
  labs(x="Season", y="Count")


ggplot(bike, aes(x=bike$hour, y=count, group=season, color=season)) + 
  stat_summary(
    fun.y=mean, 
    geom='line'
  ) + 
  stat_summary(
    fun.y=mean, 
    geom='point'
  )+ 
  labs(title="Average Count By Hour Of The Day Across Season") +
  labs(x="Hour of the Day", y="Count")

ggplot(bike, aes(x=bike$date, y=count, group=bike$date, color=year)) + 
  stat_summary(
    fun.y=mean, 
    geom='line'
  ) + 
  stat_summary(
    fun.y=mean, 
    geom='point'
  )+ 
  labs(title="Average Count By Hour Of The Day Across Weekdays") +
  labs(x="Hour of the Day", y="Count")

# 4c. Drop some variables from the dataset based on the analysis so far 
# drop temp, casual, registered and date
bike_subset = bike[-c(5,9:10, 12)] 
head(bike_subset,5)

#----------Part 5 : Model Builing starts here ----------------------
    # 5a. Split data into test and train set
    # 5b. Linear Regression
    # 5c. Random Forest
    # 5d. Gradient Boosting

# 5a. Split data into test and train set
sample_size = floor(0.8 * nrow(bike))
set.seed(1)
train_index = sample(nrow(bike), size = sample_size)
train <- bike[train_index, ]
test <- bike[-train_index, ] 

# 5d. Gradient Boosting
  #install.packages("gbm")
library (gbm)
gbmtree=4000
iDepth = 3
set.seed(1) 

# Predict Casual Counts
CasualData <- subset(train, select = -c(count, registered, date))
gbm.Casual <- gbm(log1p(casual)~.,data=CasualData,distribution= "gaussian",n.trees=gbmtree,interaction.depth=iDepth)


# Predict Registered Counts
RegisteredData <- subset(train, select = -c(count, casual, date))
gbm.Registered <- gbm(log1p(registered)~.,data=RegisteredData,distribution= "gaussian",
                      n.trees=gbmtree,interaction.depth=iDepth)

summary(gbm.Casual)
## summary(gbm.Casual)
#                 var    rel.inf
#hour             hour 56.9760312
#temp             temp 13.6712902
#month           month  9.7003849
#humidity     humidity  3.9624909
#wkday           wkday  3.8721591
#workingday workingday  3.8606379
#atemp           atemp  3.5983376
#weather       weather  1.7951507
#year             year  1.0807385
#windspeed   windspeed  1.0745978
#holiday       holiday  0.2119541
#season         season  0.1962272

summary(gbm.Registered)
## summary(gbm.Registered)
#var     rel.inf
#hour             hour 73.99447438
#month           month  6.26129638
#workingday workingday  4.55590636
#wkday           wkday  3.85967539
#year             year  3.60929105
#humidity     humidity  2.39512679
#temp             temp  1.99088129
#weather       weather  1.64512315
#atemp           atemp  1.01003766
#windspeed   windspeed  0.39311638
#holiday       holiday  0.22416857
#season         season  0.06090261

##Inference - gbm Casual: season, holiday, weather are not much significant here.
##Inference - gbm Registered: holiday, windspeed are not much significant here.

gbm.CasualFinal <- gbm(log1p(casual) ~ hour + workingday  + temp + month  +  wkday + humidity + year + windspeed, 
                               data=CasualData, distribution= "gaussian",n.trees=gbmtree,interaction.depth=iDepth)
gbm.RegisteredFinal <- gbm(log1p(registered) ~ hour + year + workingday + month + wkday + humidity + temp + weather  + season, 
                           data=RegisteredData, distribution= "gaussian",n.trees=gbmtree,interaction.depth=iDepth)


# Prediction on train data
  # Prediction on train data - casual users

gbm.PredTrainCasual <- predict(gbm.Casual, train, n.trees=gbmtree)
gbm.PredTrainCasualFinal <- predict(gbm.CasualFinal, train, n.trees=gbmtree)

# Prediction on train data - Registered users
gbm.PredTrainRegistered <- predict(gbm.Registered, train, n.trees=gbmtree)
gbm.PredTrainRegisteredFinal <- predict(gbm.RegisteredFinal, train, n.trees=gbmtree)

# Sum up Casual and Registered to get Total Count
gbm.PredTrainCount <- round(exp(gbm.PredTrainCasual) - 1, 0) + round(exp(gbm.PredTrainRegistered) - 1, 0)
gbm.PredTrainCountFinal <- round(exp(gbm.PredTrainCasualFinal) - 1, 0) + round(exp(gbm.PredTrainRegisteredFinal) - 1, 0)

# Calculate Train RMSLE
gbm.rf_train_rmsle_full <- rmsle(train$count, gbm.PredTrainCount)
gbm.rf_train_rmsle2_reduced <- rmsle(train$count, gbm.PredTrainCountFinal)

# Prediction on test data
# Prediction on test data - casual users
gbm.PredTestCasual = predict(gbm.Casual, test, n.trees=gbmtree)
gbm.PredTestCasualFinal = predict(gbm.CasualFinal, test, n.trees=gbmtree)

# Prediction on test data - registered users
gbm.PredTestRegistered = predict(gbm.Registered, test, n.trees=gbmtree)
gbm.PredTestRegisteredFinal = predict(gbm.RegisteredFinal, test, n.trees=gbmtree)

# Sum up Casual and Registered to get Total Count
gbm.PredTestCount = round(exp(gbm.PredTestCasual) - 1, 0) + round(exp(gbm.PredTestRegistered) - 1, 0)
gbm.PredTestCountFinal = round(exp(gbm.PredTestCasualFinal) - 1, 0) + round(exp(gbm.PredTestRegisteredFinal) - 1, 0)

# Calculate test RMSLE
gbm.rf_test_rmsle_full = rmsle(test$count, gbm.PredTestCount)
gbm.rf_test_rmsle2_reduced = rmsle(test$count, gbm.PredTestCountFinal)


cat("\nTraining RMSLE - Gradient Boosting (Full Model): ", gbm.rf_train_rmsle_full)
cat("\nTraining RMSLE - Gradient Boosting (Reduced Model): : ", gbm.rf_train_rmsle2_reduced)


cat("\nTest RMSLE - Gradient Boosting (Full Model): ", gbm.rf_test_rmsle_full)
cat("\nTest RMSLE - Gradient Boosting (Reduced Model): ", gbm.rf_test_rmsle2_reduced)

# Save the GBM results
gbm_test_casual = round(predict(gbm.CasualFinal, bike_test),0)
gbm_test_registered = round(predict(gbm.RegisteredFinal, bike_test),)
#rf_results = rf_test_casual + rf_test_registered

hist(bike$count, main="Training Data")
hist(lm_results, main="Linear Regression Fit")
hist(rf_results, main="Random Forest Fit")

