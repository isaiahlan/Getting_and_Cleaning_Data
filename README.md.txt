##Getting and Cleaning Data

This is Isaiah Lan's README.md document for the "Getting and Cleaning Data" Course project from John Hopkins, Bloomberg School of Health. 

The following Dataset used for this course can be found here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# 1. The Following code merges the training and the test sets to create one data set.

train <- read.table("train/X_train.txt")
test <- read.table("test/X_test.txt")
X <- rbind(train, test)

train1 <- read.table("train/subject_train.txt")
test1 <- read.table("test/subject_test.txt")
Sub <- rbind(train1, test1)

train2 <- read.table("train/y_train.txt")
test2 <- read.table("test/y_test.txt")
Y <- rbind(train2, test2)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement by using grep

features <- read.table("features.txt")
good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, good_features]
names(X) <- features[good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names. (creates the tidy data set with the descriptive activity names.) 

names(Sub) <- "subject"
tidydata <- cbind(Sub, Y, X)
write.table(tidydata, "tidydata.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
To achieve this, I use the for loop to average each variable. 

uniqueSubjects = unique(Sub)[,1]
numSubjects = length(unique(Sub)[,1])
numActivities = length(activities[,1])
numCols = dim(tidydata)[2]
avgtidydata = tidydata[1:(numSubjects*numActivities), ]
row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    avgtidydata[row, 1] = uniqueSubjects[s]
    avgtidydata[row, 2] = activities[a, 2]
    tmp <- tidydata[tidydata$subject==s & tidydata$activity==activities[a, 2], ]
    avgtidydata[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(avgtidydata, "avgtidydata.txt",row.name=FALSE)
