library(dplyr)

# STEP 1- Get data

# download zip file containing data if it hasn't already been downloaded
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

# STEP 2 - Read data

# read training data
subject_train <- read.table(file.path(dataPath, "train", "subject_train.txt"))
x_train <- read.table(file.path(dataPath, "train", "X_train.txt"))
y_train <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data
subject_test <- read.table(file.path(dataPath, "test", "subject_test.txt"))
x_test <- read.table(file.path(dataPath, "test", "X_test.txt"))
y_test<- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features.
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# Step 3 - Merge the training and the test sets to create one data set

# concatenate individual data tables to make single data table
mergeData <- rbind(
  cbind(subject_train, x_train, y_train),
  cbind(subject_test, x_test, y_test)
)

# remove individual data tables to save memory
rm(subject_train, x_train, y_train, 
   subject_test, x_test, y_test)

# assign column names
colnames(mergeData) <- c("subject", features[, 2], "activity")

# Step 4 - Extract only the measurements on the mean and standard deviation
# for each measurement


# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(mergeData))

# and keep data in these columns only
mergeData <- mergeData[, columnsToKeep]


# Step 5 - Use descriptive activity names to name the activities in the dataset

# replace activity values with named factor levels
mergeData$activity <- factor(mergeData$activity, 
  levels = activities[, 1], labels = activities[, 2])



# Step 6 - Appropriately label the data set with descriptive variable name

# get column names
mergeDataCols <- colnames(mergeData)

# remove special characters
mergeDataCols <- gsub("[\\(\\)-]", "", mergeDataCols)

# expand abbreviations and clean up names
mergeDataCols <- gsub("^f", "frequencyDomain", mergeDataCols)
mergeDataCols <- gsub("^t", "timeDomain", mergeDataCols)
mergeDataCols <- gsub("Acc", "Accelerometer", mergeDataCols)
mergeDataCols <- gsub("Gyro", "Gyroscope", mergeDataCols)
mergeDataCols <- gsub("Mag", "Magnitude", mergeDataCols)
mergeDataCols <- gsub("Freq", "Frequency", mergeDataCols)
mergeDataCols <- gsub("mean", "Mean", mergeDataCols)
mergeDataCols <- gsub("std", "StandardDeviation", mergeDataCols)

# correct typo
mergeDataCols <- gsub("BodyBody", "Body", mergeDataCols)

# use new labels as column names
colnames(mergeData) <- mergeDataCols


# Step 7 - Create a second, independent tidy set with the average of each
# variable for each activity and each subject


# group by subject and activity and summarise using mean
mergeDataMeans <- mergeData %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(mergeDataMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
