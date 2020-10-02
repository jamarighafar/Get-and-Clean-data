##############################################################################
#
# Date 02-10-2020
# FILE
#   run_analysis.R
#
# OVERVIEW
#   Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
#   
##############################################################################

library(dplyr)


##############################################################################
# STEP A - Get and download data
##############################################################################

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


##############################################################################
# STEP B - Read the download data file
##############################################################################

# Read training data from exercise
trainSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data from exercise
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features, don't convert text labels to factors
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
  ## note: feature names (in features[, 2]) are not unique
  ##       e.g. fBodyAcc-bandsEnergy()-1,8

# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


##############################################################################
# Step 1 - Merge the training and the test sets to create a single data set
##############################################################################

# concatenate individual data tables to make single data table
bodyActivity <- rbind(
  cbind(trainSubjects, trainValues, trainActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(trainSubjects, trainValues, trainActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(bodyActivity) <- c("subject", features[, 2], "activity")


##############################################################################
# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement
##############################################################################

# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(bodyActivity))

# ... and keep data in these columns only
bodyActivity <- bodyActivity[, columnsToKeep]


##############################################################################
# Step 3 - Use descriptive activity names to name the activities in the data
#          set
##############################################################################

# replace activity values with named factor levels
bodyActivity$activity <- factor(bodyActivity$activity, 
  levels = activities[, 1], labels = activities[, 2])


##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

# get column names
bodyActivityCols <- colnames(bodyActivity)

# remove special characters
bodyActivityCols <- gsub("[\\(\\)-]", "", bodyActivityCols)

# expand abbreviations and clean up names
bodyActivityCols <- gsub("^f", "frequencyDomain", bodyActivityCols)
bodyActivityCols <- gsub("^t", "timeDomain", bodyActivityCols)
bodyActivityCols <- gsub("Acc", "Accelerometer", bodyActivityCols)
bodyActivityCols <- gsub("Gyro", "Gyroscope", bodyActivityCols)
bodyActivityCols <- gsub("Mag", "Magnitude", bodyActivityCols)
bodyActivityCols <- gsub("Freq", "Frequency", bodyActivityCols)
bodyActivityCols <- gsub("mean", "Mean", bodyActivityCols)
bodyActivityCols <- gsub("std", "StandardDeviation", bodyActivityCols)

# correct typo
bodyActivityCols <- gsub("BodyBody", "Body", bodyActivityCols)

# use new labels as column names
colnames(bodyActivity) <- bodyActivityCols


##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

# group by subject and activity and summarise using mean
bodyActivityMeans <- bodyActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(bodyActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
