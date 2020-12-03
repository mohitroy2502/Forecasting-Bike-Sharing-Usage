# ----------Step 1: Import all the required libraries ----------
  # We will add to this as and when required
  library(ggplot2)

# ------------------- Step 1 ends here --------------------------


# ------------------- Step 2: Gather the data -----------------

  ### Data is provided as .csv file and already split into Test and Train.
  ### The training set is comprised of the first 19 days of each month, while the test set is the 20th to the end of the month.
  ### Let's import the data
    bike_train <- read.csv("C:/Users/Sean CG/Desktop/Sean/CSP 571/Project/train.csv")
    bike_test <- read.csv("C:/Users/Sean CG/Desktop/Sean/CSP 571/Project/test.csv")
    bike = bike_train #(for now)
    head(bike, 10)

# ------------------- Step 2 ends here --------------------------



# ------------------- Step 3: Data Cleaning -----------------
  # 3a. Analyze Attributes: Check properties of data
      dim(bike)
      str(bike)

  # 3b. Complete Data Perform missing value analysis and Impute if needed
      table(is.na(bike))

  # 3c. Correct Data: Check for any invalid data points


  # 3d. Create Derived Attributes - Feature Extraction


  # 3e. Convert - Converting data to proper formats


# ------------------- Step 3 ends here --------------------------

# ------------- Step 4: Exploratory Data Analysis -----------

  # 4a. Outlier Analysis
      boxplot(count~datetime, data = bike, main = "Date Time", xlab = "Date Time", ylab = "Rental Count")
      boxplot(count~season, data = bike, main = "Season", xlab = "Season", ylab = "Rental Count")
      boxplot(count~holiday, data = bike, main = "Holiday", xlab = "Holiday", ylab = "Rental Count")
      boxplot(count~workingday, data = bike, main = "Working Day", xlab = "Working Day", ylab = "Rental Count")
      boxplot(count~weather, data = bike, main = "Weather", xlab = "Weather", ylab = "Rental Count")
      boxplot(count~temp, data = bike, main = "Temperature", xlab = "Temperature", ylab = "Rental Count")
      boxplot(count~atemp, data = bike, main = "Feel-Like Temperature", xlab = "Feel-Like Temperature", ylab = "Rental Count")
      boxplot(count~humidity, data = bike, main = "Humidity", xlab = "Humidity", ylab = "Rental Count")
      boxplot(count~windspeed, data = bike, main = "Windspeed", xlab = "Windspeed", ylab = "Rental Count")

    # 4a(1). Visualize continuos variables wrt target variable
      count.vs.datetime <- ggplot(data = bike, aes(x = datetime, y = count, group = 1)) + geom_point() + labs(x = "Datetime", y = "Rental Count", title = "Rental Count vs. Datetime") + geom_smooth(method = "lm", color = "yellow")
      count.vs.temp <- ggplot(data = bike, aes(x = temp, y = count, group = 1)) + geom_point() + labs(x = "Temperature", y = "Rental Count", title = "Rental Count vs. Temperature") + geom_smooth(method = "lm", color = "yellow")
      count.vs.atemp <- ggplot(data = bike, aes(x = atemp, y = count, group = 1)) + geom_point() + labs(x = "Feel-Like Temperature", y = "Rental Count", title = "Rental Count vs. Feel-Like Temperature") + geom_smooth(method = "lm", color = "yellow")
      count.vs.humidity <- ggplot(data = bike, aes(x = humidity, y = count, group = 1)) + geom_point() + labs(x = "Humidity", y = "Rental Count", title = "Rental Count vs. Humidity") + geom_smooth(method = "lm", color = "yellow")
      count.vs.windspeed <- ggplot(data = bike, aes(x = windspeed, y = count, group = 1)) + geom_point() + labs(x = "Windspeed", y = "Rental Count", title = "Rental Count vs. Windspeed") + geom_smooth(method = "lm", color = "yellow")
      
    # 4a(2). Visualize categorical variables wrt target variable
      count.vs.season <- ggplot(bike) + geom_bar(aes(x = season, y = count), stat = "identity") + labs(x = "Season", y = "Rental Count", title = "Rental Count vs. Season")
      count.vs.holiday <- ggplot(bike) + geom_bar(aes(x = holiday, y = count), stat = "identity") + labs(x = "Holiday", y = "Rental Count", title = "Rental Count vs. Holiday")
      count.vs.workingday <- ggplot(bike) + geom_bar(aes(x = workingday, y = count), stat = "identity") + labs(x = "Working Day", y = "Rental Count", title = "Rental Count vs. Working Day")
      count.vs.weather <- ggplot(bike) + geom_bar(aes(x = weather, y = count), stat = "identity") + labs(x = "Weather", y = "Rental Count", title = "Rental Count vs. Weather")
      
  # 4b. Correlation Analysis

    # 4b(1). Explore continous features

      # i. Check distribution of target variable

      # ii. Explore correlation between independent continuous variables with target variable

      # iii. Plot heatmap for correlation matrix (to check for multicolinearity)

      # iv. Visualize the relationship among all continuous variables using pairplots

      # v. Explore relationship between independent continuous variables and dependent variables using Joint Plot

    # 4b(2) Explore categorical features

      # i. Check distribution of categorical variables

      # ii. Check how individual categorical features affects the target variable

      # iii. Explore trends over time

    # 4c. Drop some variables from the dataset based on the analysis so far 
