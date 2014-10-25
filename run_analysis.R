#run_analysis.R

#You should create one R script called run_analysis.R that does the following. 
#    Merges the training and the test sets to create one data set.
#    Extracts only the measurements on the mean and standard deviation for each measurement. 
#    Uses descriptive activity names to name the activities in the data set
#    Appropriately labels the data set with descriptive variable names. 
#    From the data set in step 4, creates a second, independent tidy data set with the average 
#    of each variable for each activity and each subject.

### Load following test, training, activity labels and features
# 	'test/X_test.txt': Test set.
# 	'test/y_test.txt': Test labels.
#
# 	'train/X_train.txt': Training set.
# 	'train/y_train.txt': Training labels.
#
# 	'test/subject_test.txt': Each row identifies the subject who performed the activity for each 
#		window sample. Its range is from 1 to 30. 
# 	'train/subject_train.txt': Each row identifies the subject who performed the activity for each 
#		window sample. Its range is from 1 to 30. 
#
# 	'activity_labels.txt': Links the class labels with their activity name.
#
#     'features.txt': List of all features.

library(data.table) 

# test data
testSET <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
testLABELS <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
testSUBJECT <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)

# training data
trainingSET <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
trainingLABELS <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
trainingSUBJECT <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)

# activity labels: activities performed within test data sets and training data sets
#   1 - WALKING
#   2 - WALKING_UPSTAIRS
#   3 - WALKING_DOWNSTAIRS
#   4 - SITTING
#   5 - STANDING
#   6 - LAYING
activityLABELS <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, colClasses = "character",
col.names = c("Activity_ID", "Activity"))
##View(activityLABELS)

# features  
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, colClasses = "character")

### Merges the training and the test sets to create one data set.
# Merge the test data using testSET ,testSUBJECT and testLABELS 
w_merged_testSET <- cbind(testSET, testSUBJECT)
merged_testSET <- cbind(w_merged_testSET, testLABELS)
##View(merged_testSET)
##nrow(merged_testSET) = 2947 entries

# Merge the training data using trainingSET, trainingSUBJECT and trainingLABELS
w_merged_trainingSET <- cbind(trainingSET, trainingSUBJECT)
merged_trainingSET <- cbind(w_merged_trainingSET, trainingLABELS)
##View(merged_trainingSET)
##nrow(merged_trainingSET) = 7352 entries

# Combine the merged test set and training set into one data set
mergedDATA <- rbind(merged_testSET, merged_trainingSET)
##View(mergedDATA)
##nrow(merged_trainingSET) + nrow(merged_testSET) = 10299 entries

### Label columns accordingly adding Subject and Activity_ID to the column_headers
# used for column headers "Subject" and Activity_ID".
##nrow(features) = 561, Subject and Activity_ID will be columns 562 and 563  

mergedDATA_LABELS <- rbind(rbind(features, c(562, "Subject")), c(563, "Activity_ID"))[,2]
names(mergedDATA) <- mergedDATA_LABELS
##View(names(mergedDATA))

# Extracts only the measurements on the mean and standard deviation for each measurement.
require(plyr) ## required for join function between mergedDATA_measurements and activityLABELS

mergedDATA_measurements <- mergedDATA[,grepl("mean|std|Subject|Activity_ID", names(mergedDATA))]
# Incorporate the activityLABELS into mergedDATA
mergedDATA_measurements <- join(mergedDATA_measurements, activityLABELS, by = "Activity_ID", match = "first")
mergedDATA_measurements <- mergedDATA_measurements[,-1]

### substitute column headers to appropriately label the data set with descriptive variable names.
## remove parentheses
names(mergedDATA_measurements) <- gsub('[-()]', '',names(mergedDATA_measurements))
features[,2] = gsub('[-()]', '', features[,2]) 
## rename mean
names(mergedDATA_measurements) = gsub('meanX', 'Mean_X', names(mergedDATA_measurements))
names(mergedDATA_measurements) = gsub('meanY', 'Mean_Y', names(mergedDATA_measurements))
names(mergedDATA_measurements) = gsub('meanZ', 'Mean_Z', names(mergedDATA_measurements))
names(mergedDATA_measurements) = gsub('mean', 'Mean', names(mergedDATA_measurements))
## rename std
names(mergedDATA_measurements) = gsub('stdX', 'Std_X', names(mergedDATA_measurements))
names(mergedDATA_measurements) = gsub('stdY', 'Std_Y', names(mergedDATA_measurements))
names(mergedDATA_measurements) = gsub('stdZ', 'Std_Z', names(mergedDATA_measurements))
names(mergedDATA_measurements) = gsub('std', 'Std', names(mergedDATA_measurements))
## rename Freq
names(mergedDATA_measurements) = gsub('FreqX', 'Freq_X', names(mergedDATA_measurements))
names(mergedDATA_measurements) = gsub('FreqY', 'Freq_Y', names(mergedDATA_measurements))
names(mergedDATA_measurements) = gsub('FreqZ', 'Freq_Z', names(mergedDATA_measurements))
##View(mergedDATA_measurements)

### From the merged data, create a second, independent tidy data set with the average 
### of each variable for each activity and each subject.

tidyDATA_dt <- data.table(mergedDATA_measurements)
tidyDATA <- ddply(tidyDATA_dt, c("Subject","Activity"), numcolwise(mean))
#CSV file
#write.table(tidyDATA, file="tidy_measurements2.csv", sep=",", row.names = FALSE)
#TEXT file
write.table(tidyDATA, file="tidy_measurements.txt", sep=",", row.names = FALSE)

