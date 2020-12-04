
# ----------- Step 1a: Define and categorize the problem statement --------------

# The problem statement is to "Predict the daily bike rental count based on the environmental and seasonal settings"
# This is clearly a 'Supervised machine learning regression problem' to predict a number based on the input features.

# ----------- Step 1a ends here ----------------- 


# ----------Step 1b: Import all the required libraries ----------

#---- for data transformations----
    #install.packages("lubridate")
    library(lubridate)

#---- for EDA Visualizations ------
    #install.packages("corrplot")
    library(corrplot)
    #install.packages("ggplot2")
    library(ggplot2)
    #install.packages("GGally")
    library("GGally")
    #install.packages("ggExtra")
    library(ggExtra)

#---- for model building----
    library(caret)
    #install.packages("Metrics")
    library(Metrics)
    #install.packages("randomForest")
    library(randomForest)

    #install.packages(gbm)
    library (gbm)

# ------------------- Step 1b ends here --------------------------



# ------------------- Step 2: Gather the data -----------------

  # Data is provided as .csv file and already split into Test and Train.
  # The training set is comprised of the first 19 days of each month, while the test set is the 20th to the end of the month.
  # Let's import the data
    bike= read.csv("/Users/snehashrungarpawar/Documents/Master in Data Science/DPA/Project/Data/train.csv", header=TRUE)
    bike_test = read.csv("/Users/snehashrungarpawar/Documents/Master in Data Science/DPA/Project/Data/test.csv", header=TRUE)
# ------------------- Step 2 ends here --------------------------


# ------------------- Step 3: Data Preparation ---------------------
  # 3a. Analyze Attributes: Check properties of data
  # 3b. Complete Data Perform missing value analysis and Impute if needed
  # 3c. Correct Data: Check for any invalid data points
  # 3d. Create Derived Attributes - Feature Extraction
  # 3e. Convert - Converting data to proper formats


  # 3a. Analyze Attributes: Check properties of data
      dim(bike)
      str(bike)
      head(bike, 10)
  # 3a -> Inference: 
        #i. The dataset has 10,886 observations (n=10886) and 12 columns of type int, num and factor.
        #ii. Season, Holiday, Working day and weather are categorical variables.
        #ii. temp, atemp, humidity, windspeed, casual, registered and count are continuous numerical variables.


  # 3b. Complete Data Perform missing value analysis and Impute if needed
      table(is.na(bike))
  # 3b -> Inference: There are no null values in the dataset. If it had, then either the rows/columns had to be
    # dropped or the null values be imputed based on the % of null values


  # 3c. Correct Data: Check for any invalid data points
    # From above observations data doesnot seem to have any invalid datatypes to be handled.
    # Let's check for the outliers in EDA step


  # 3d. Create Derived Attributes - Feature Extraction
      # Lets extract 'date','month','weekday' and 'year' from 'datetime' column as we will be needing it for analysis
      bike$date=as.factor(day(bike$datetime))
      bike$year = as.factor(year(bike$datetime))
      bike$month = as.factor(month(bike$datetime))
      bike$hour = as.factor(hour(bike$datetime))
      bike$wkday = as.factor(wday(bike$datetime))
      
      bike_test$date=as.factor(day(bike_test$datetime))
      bike_test$year = as.factor(year(bike_test$datetime))
      bike_test$month = as.factor(month(bike_test$datetime))
      bike_test$hour = as.factor(hour(bike_test$datetime))
      bike_test$wkday = as.factor(wday(bike_test$datetime))

      # Drop datetime as we have extracted all the above needed information from it
      bike = bike[-c(1)]
      bike_test = bike_test[-c(1)]

      head(bike, 5)
      head(bike_test, 5)

  # 3d -> Inference: There are no null values in the dataset. If it had, then either the rows/columns had to be 
                    #dropped or the null values be imputed based on the % of null values.


  # 3e. Convert - Converting data to proper formats
    # We can clearly see that "season", "holiday", "workingday" and"weather" are categories rather than continous variable.
    # Let's convert them to categories
      names = c("season", "holiday", "workingday", "weather")
      bike[,names] = lapply(bike[,names], factor)
      bike_test[,names] = lapply(bike_test[,names], factor)

      str(bike) 
      str(bike_test)

# ------------------- Step 3: Data Preparation ends here --------------------------





# ------------- Step 4: Exploratory Data Analysis -----------
    # 4a. Outlier Analysis

    # 4a(1). Visualize continuos variables
        par(mfrow=c(1,5))
        boxplot(bike$count, main="Count", col="Gray", border = "black")
        boxplot(bike$temp, main="Temperature", col="blue", border = "black")
        boxplot(bike$atemp, main="Feels Like Temp", col="purple", border = "black")
        boxplot(bike$humidity, main="Humidity", col="green", border = "black")
        boxplot(bike$windspeed, main="Windspeed", col="orange", border = "black")

    # 4a(2). Visualize categorical variables wrt target variable
    par(mfrow=c(3,4))
    ggplot(data = bike, aes(x=season, y=count, fill=as.factor(season))) + geom_boxplot() + labs(title="Season vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=weather, y=count, fill=as.factor(weather))) + geom_boxplot() + labs(title="weather vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=holiday, y=count, fill=as.factor(holiday))) + geom_boxplot() + labs(title="holiday vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=workingday, y=count, fill=as.factor(workingday))) + geom_boxplot() + labs(title="workingday vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=year, y=count, fill=as.factor(year))) + geom_boxplot() + labs(title="year vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=month, y=count, fill=as.factor(month))) + geom_boxplot() + labs(title="month vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=wkday, y=count, fill=as.factor(wkday))) + geom_boxplot() + labs(title="weekday vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=hour, y=count, fill=as.factor(hour))) + geom_boxplot() + labs(title="hour vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=date, y=count, fill=as.factor(day(date)))) + geom_boxplot() + labs(title="date vs Count") + theme(legend.title = element_blank())



# 4b. Correlation Analysis

# --------------- Explore Continuous Variables----------------
    # 4b(1). Explore continous features
        # i. Check distribution of target variable
        # ii. Explore correlation between independent continuous variables with target variable
        # iii. Plot heatmap for correlation matrix (to check for multicolinearity)
        # iv. Visualize the relationship among all continuous variables using pairplots
        # v. Explore relationship between independent continuous variables and dependent variables using Joint Plot


   # 4b(1) i. Check distribution of target variable
          hist(bike$count, col="blue") 
          plot(bike$count)
   # Inference: Target variable "count" is almost normally distributed.


    # 4b(1) ii. Explore correlation between independent continuous variables with target variable      
        plot(bike$temp,bike$count)
        plot(bike$atemp,bike$count)
        plot(bike$windspeed,bike$count)
        plot(bike$humidity,bike$count)


    # 4b(1) iii. Plot heatmap for correlation matrix (to check for multicolinearity)
        corr <- as.data.frame(lapply(bike[c(5:8, 11)], as.numeric))
        corrplot(cor(corr), method = "color", type='lower')
    # Inference: 
        # i. temp and atemp are highly correlated, we would need to drop one of them to remove multicolinearity.
        # ii. We can also drop Registered and Casual from our analysis as Counts are categorized as Registered and Casual 
             # and we will be predicting "Count" variable only.


    # 4b(1) iv. Visualize the relationship among all continuous variables using pairplots
        ggpairs(bike[c(5:8)], lower=list(continuous=wrap("smooth", colour="orange")) )


    # 4b(1) v. Explore relationship between independent continuous variables and dependent variables using Joint Plot       
        
        # 1. temp vs Count
        plot_center = ggplot(bike, aes(x=temp,y=count)) + geom_point(colour="blue") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="blue")
        # Inference: temp has good correlation with count.


        # 4b(1).v.2. atemp vs Count
        plot_center = ggplot(bike, aes(x=atemp,y=count)) + geom_point(colour="red") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="red")
        # Inference: atemp has good correlation with count.


        # 4b(1).v.3. humidity vs Count
        plot_center = ggplot(bike, aes(x=humidity,y=count)) + geom_point(colour="green") + geom_smooth(method="lm") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="green")
        # Inference: Humidity has low correlation with count.


        # 4b(1).v.4. windspeed vs Count
        plot_center = ggplot(bike, aes(x=windspeed,y=count)) + geom_point(colour="orange") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="orange")


    # 4b(1) Inferences Summary - Analysis of continous variables
        # 1. Target variable 'count' is almost normally distributed.
        # 2. From correlation with dependent variable "count", we can see that 'casual','registered' are very 
             # highly correlated to cnt. Needs to be dropped from the dataset.
        # 3. 'humidity' has low correlation with 'count'. For now, lets keep it.
        # 4. atemp and temp has good correlation with 'count'
        # 5. From heatmap, we can see that atemp and temp are highly correlated. So we need to drop 1 to remove multicollinearity.
        # 6. Since, as seen from jointplot, p(atemp) < p(temp), we can drop 'temp' and retain 'atemp' in the dataset.


# --------------- Explore Catogorical Variables----------------
    # 4b(2) Explore categorical features
          # i. Check distribution of categorical variables
            ggplot(bike, aes(x=" ",fill=year))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "year")+theme_void()

            ggplot(bike, aes(x=" ",fill=month))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "month")+theme_void()
            bike$season = factor(bike$season)

            ggplot(bike, aes(x=" ",fill=season))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "Season")+theme_void()
            bike$holiday = factor(bike$holiday)

            ggplot(bike, aes(x=" ",fill=holiday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "holiday")+theme_void()

            ggplot(bike, aes(x=" ",fill=wkday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "weekday")+theme_void()
            bike$workingday = factor(bike$workingday)

            ggplot(bike, aes(x=" ",fill=workingday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "workingday")+theme_void()
            bike$weather = factor(bike$weather)

            ggplot(bike, aes(x=" ",fill=weather))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "weather")+theme_void()


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

            ggplot(bike, aes(x=bike$hour, y=count, group=wkday, color=wkday)) + 
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


        ggplot(bike, aes(x=bike$day, y=count, group=day, color=day)) + 
              stat_summary(
                fun.y=mean, 
                geom='line'
              ) + 
              stat_summary(
                fun.y=mean, 
                geom='point'
              )+ 
              labs(title="Average Count By Day") +
              labs(x="HDay", y="Count")

# 4c. Drop some variables from the dataset based on the analysis so far 
        # drop temp, casual, registered and date
        bike_subset = bike[-c(5,9:10, 12)] 
        head(bike_subset,5)


#------ Step 4: Exploratory Data Analysis ENDS Here------------------
# Final observations:
#1.) 'atemp' and 'temp' are very strongly correlated . Drop 'atemp' from the dataset (since it has higher p-value 
        #than 'temp')
#2.) 'date' does not seem to have any affect on count of bikes, it can be dropped from the dataset
#------------------------------------------------------------


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


    # 5b. Linear Regression
        # Fit Linear Model
        # drop atemp, registered, casual and date
        train_subset = train[-c(6,9:10, 12)]
        test_subset = test[-c(6,9:10, 12)]

        lm_fit = lm(count ~ ., data = train_subset)
        summary(lm_fit)
        
        # Choosing the best model by AIC in a Stepwise Algorithm
        # The step() function iteratively removes insignificant features from the model.
        step(lm_fit)
        summary(lm_fit)

        # Calculate Train RMSLE
        y_act_train <- abs(train_subset$count)
        y_pred_train <- abs(predict(lm_fit, train_subset))
        lm_train_RMSLE = rmsle(y_act_train, y_pred_train)
        
        # Calculate Test RMSLE
        y_act_test <- abs(test_subset$count)
        y_pred_test <- abs(predict(lm_fit, test_subset))
        lm_test_RMSLE = rmsle(y_act_test, y_pred_test)
        
        # Save the results
        lm_results = predict(lm_fit, bike_test)
        hist(lm_results)


plot(lm_fit)

mse(y_act_train,y_pred_train)
rmse(y_act_train,y_pred_train)
rmsle(y_act_train,y_pred_train)

mse(y_act_test,y_pred_test)
rmse(y_act_test,y_pred_test)
rmsle(y_act_test,y_pred_test)

    # 5b. Random Forest
        Ntree=500
        Mtry = 5
        myImportance = TRUE
        
        # Predict Casual Counts
        set.seed(1)
        CasualData <- subset(train, select = -c(count, registered, date, atemp))
        CasualFit <- randomForest(casual ~ ., data=CasualData, ntree=Ntree, mtry=Mtry,
                                importance=myImportance)
        

        # Predict Registered Counts
        RegisteredData <- subset(train, select = -c(count, casual, date, atemp))
        RegisteredFit <- randomForest(registered ~ ., data=RegisteredData, ntree=Ntree, mtry=Mtry,
                                importance=myImportance)

        varImpPlot(CasualFit) 
        varImp(CasualFit) 

        varImpPlot(RegisteredFit)
        varImp(RegisteredFit) 

    #Inference - Casual Fit: season, holiday, windspeed and weather are not much significant here.
    #Inference - Registered Fit: season, holiday, windspeed and weekday are not much significant here.


        casualFitFinal <- randomForest(casual ~ hour + year + humidity + month + temp + workingday + wkday, 
                               data=CasualData, ntree=Ntree, mtry=Mtry,importance=myImportance)
        RegisteredFitFinal <- randomForest(registered ~ hour + year + month + weather + workingday + humidity + temp, 
                                        data=RegisteredData, ntree=Ntree, mtry=Mtry,importance=myImportance)


        # Prediction on train data
        
            # Prediction on train data - casual users
            PredTrainCasual = round(predict(CasualFit, train),0)
            PredTrainCasualFinal = round(predict(casualFitFinal, train),0)

            # Prediction on train data - Registered users
            PredTrainRegistered = round(predict(RegisteredFit, train),0)
            PredTrainRegisteredFinal = round(predict(RegisteredFitFinal, train),0)
            
            # Sum up Casual and Registered to get Total Count
            PredTrainCount = PredTrainCasual+PredTrainRegistered
            PredTrainCountFinal = PredTrainCasualFinal+PredTrainRegisteredFinal

            # Calculate Train RMSLE
            rf_train_rmsle_full = rmsle(train$count, PredTrainCount)
            rf_train_rmsle2_reduced = rmsle(train$count, PredTrainCountFinal)
    

        # Prediction on test data
            # Prediction on test data - casual users
            PredTestCasual = round(predict(CasualFit, test),0)
            PredTestCasualFinal = round(predict(casualFitFinal, test),0)

            # Prediction on test data - registered users
            PredTestRegistered = round(predict(RegisteredFit, test),0)
            PredTestRegisteredFinal = round(predict(RegisteredFitFinal, test),0)

            # Sum up Casual and Registered to get Total Count
            PredTestCount = PredTestCasual+PredTestRegistered
            PredTestCountFinal = PredTestCasualFinal+PredTestRegisteredFinal

            # Calculate Train RMSLE
            rf_test_rmsle_full = rmsle(test$count, PredTestCount)
            rf_test_rmsle2_reduced = rmsle(test$count, PredTestCountFinal)



cat("Training RMSLE - Linear Regression: ", lm_train_RMSLE)
cat("\nTraining RMSLE - Random Forest (Full Model): ", rf_train_rmsle_full)
cat("\nTraining RMSLE - Random Forest (Reduced Model): : ", rf_train_rmsle2_reduced)

cat("\n\nTest RMSLE - Linear Regression: ", lm_test_RMSLE)
cat("\nTest RMSLE - Random Forest (Full Model): ", rf_test_rmsle_full)
cat("\nTest RMSLE - Random Forest (Reduced Model): ", rf_test_rmsle2_reduced)


        hist(bike$count, main="Training Data")
        hist(lm_results, main="Linear Regression Fit")
        hist(rf_results, main="Random Forest Fit")

        # Inference: The distribution of predicted count looks similar to that of train data. 

        # Save the RF results
        rf_test_casual = round(predict(casualFitFinal, bike_test),0)
        rf_test_registered = round(predict(RegisteredFitFinal, bike_test),)
        rf_results = rf_test_casual + rf_test_registered

gbmtree=4000
iDepth = 3
set.seed(1) 

# Predict Casual Counts
CasualData <- subset(train, select = -c(count, registered, atemp, date))
gbm.Casual <- gbm(log1p(casual)~.,data=CasualData,distribution= "gaussian",n.trees=gbmtree,interaction.depth=iDepth)


# Predict Registered Counts
RegisteredData <- subset(train, select = -c(count, casual, atemp, date))
gbm.Registered <- gbm(log1p(registered)~.,data=RegisteredData,distribution= "gaussian",n.trees=gbmtree,interaction.depth=iDepth)


summary(gbm.Casual)
summary(gbm.Registered)

##Inference - gbm Casual: season, holiday, year, windspeed are not much significant here.
##Inference - gbm Registered: holiday, windspeed, season, weather are not much significant here.

gbm.CasualFinal <- gbm(log1p(casual) ~ hour + workingday + temp + month  +  wkday + humidity + weather, 
                               data=CasualData, distribution= "gaussian",n.trees=gbmtree,interaction.depth=iDepth)
gbm.RegisteredFinal <- gbm(log1p(registered) ~ hour + year + workingday + month + wkday + humidity + temp, 
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

# Calculate Test RMSLE
gbm.rf_test_rmsle_full = rmsle(test$count, gbm.PredTestCount)
gbm.rf_test_rmsle2_reduced = rmsle(test$count, gbm.PredTestCountFinal)

gbm.rf_train_rmsle_full
gbm.rf_train_rmsle2_reduced

gbm.rf_test_rmsle_full
gbm.rf_test_rmsle2_reduced

par(mfrow=c(2,2))
plot(y_act_test, y_pred_test, main="Linear Regression", xlab="actual", ylab="predicted")
plot(test$count, PredTestCount, main="Random Forest", xlab="actual", ylab="predicted")
plot(test$count, gbm.PredTestCountFinal, main="Gradient Boosting", xlab="actual", ylab="predicted")

cor(y_act_test, y_pred_test)
cor(test$count, PredTestCountFinal)
cor(test$count, gbm.PredTestCountFinal)

par(mfrow=c(2,2))
plot(test_subset$count, main = "Linear Model", ylab = "Test Set Rental Count", pch = 20)
points(predict(lm_fit, newdata = test), col = "red", pch = 20)

plot(test_subset$count, main = "Random Forest", ylab = "Test Set Rental Count", pch = 20)
points(PredTestCountFinal, col = "red", pch = 20)

plot(test_subset$count, main = "Gradient Boosting", ylab = "Test Set Rental Count", pch = 20)
points(gbm.PredTestCountFinal, col = "red", pch = 20)

 # Save the RF results
        gbm_test_casual = round(predict(gbm.CasualFinal, bike_test, n.trees=gbmtree),0)
        gbm_test_registered = round(predict(gbm.RegisteredFinal, bike_test, n.trees=gbmtree),0)
        gbm_results = gbm_test_casual + gbm_test_registered

par(mfrow=c(2,2))
hist(bike$count, main="Training Data")
hist(lm_results, main="Linear Regression Fit")
hist(rf_results, main="Random Forest Fit")
hist(rf_results, main="Gradient Boosting Fit")


