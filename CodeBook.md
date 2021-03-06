#CodeBook for the tidy dataset
##Data source

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

    http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project available in the UCI HAR Dataset directory:

    https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The dataset includes the following files:

        - 'features_info.txt': Shows information about the variables used on the feature vector.
        - 'features.txt': List of all features.
        - 'activity_labels.txt': Links the class labels with their activity name.
        - 'train/X_train.txt': Training set.
        - 'train/y_train.txt': Training labels.
        - 'test/X_test.txt': Test set.
        - 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

        - 'train/subject_train.txt': Each row identifies the subject who performed the activity for 
        each window sample.  Its range is from 1 to 30. 
        - 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone 
        accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The 
        same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for 
        the Y and Z axis. 
        - 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by 
        subtracting the gravity from the total acceleration. 
        - 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the 
        gyroscope for each window sample. The units are radians/second. 

##Activity labels derived from the UCI HAR Dataset

        1 - WALKING
        2 - WALKING_UPSTAIRS
        3 - WALKING_DOWNSTAIRS
        4 - SITTING
        5 - STANDING
        6 - LAYING

##Feature Selection derived from the UCI HAR Dataset

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

    tBodyAcc-XYZ
    tGravityAcc-XYZ
    tBodyAccJerk-XYZ
    tBodyGyro-XYZ
    tBodyGyroJerk-XYZ
    tBodyAccMag
    tGravityAccMag
    tBodyAccJerkMag
    tBodyGyroMag
    tBodyGyroJerkMag
    fBodyAcc-XYZ
    fBodyAccJerk-XYZ
    fBodyGyro-XYZ
    fBodyAccMag
    fBodyAccJerkMag
    fBodyGyroMag
    fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

    mean(): Mean value
    std(): Standard deviation
    mad(): Median absolute deviation 
    max(): Largest value in array
    min(): Smallest value in array
    sma(): Signal magnitude area
    energy(): Energy measure. Sum of the squares divided by the number of values. 
    iqr(): Interquartile range 
    entropy(): Signal entropy
    arCoeff(): Autorregresion coefficients with Burg order equal to 4
    correlation(): correlation coefficient between two signals
    maxInds(): index of the frequency component with largest magnitude
    meanFreq(): Weighted average of the frequency components to obtain a mean frequency
    skewness(): skewness of the frequency domain signal 
    kurtosis(): kurtosis of the frequency domain signal 
    bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
    angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

    gravityMean
    tBodyAccMean
    tBodyAccJerkMean
    tBodyGyroMean
    tBodyGyroJerkMean

The complete list of variables of each feature vector is available in 'features.txt'

##Transformations

Load following test, training, activity labels and features derived from the UCI HAR Datasets using:

    testSET <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
    testLABELS <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
    testSUBJECT <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
    trainingSET <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
    trainingLABELS <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
    trainingSUBJECT <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
    activityLABELS <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, colClasses =                        "character", col.names = c("Activity_ID", "Activity"))
    features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, colClasses = "character")

Merges the training and the test sets to create one data set.

    w_merged_testSET <- cbind(testSET, testSUBJECT)
    merged_testSET <- cbind(w_merged_testSET, testLABELS)
    w_merged_trainingSET <- cbind(trainingSET, trainingSUBJECT)
    merged_trainingSET <- cbind(w_merged_trainingSET, trainingLABELS)
    mergedDATA <- rbind(merged_testSET, merged_trainingSET)

Label columns accordingly adding Subject and Activity_ID to the column_headers and appropriately label the data set with descriptive variable names.

    mergedDATA_LABELS <- rbind(rbind(features, c(562, "Subject")), c(563, "Activity_ID"))[,2]
    names(mergedDATA) <- mergedDATA_LABELS
    names(mergedDATA_measurements) <- gsub('[-()]', '',names(mergedDATA_measurements))
    names(mergedDATA_measurements) = gsub('mean', 'Mean', names(mergedDATA_measurements))
    ...
    names(mergedDATA_measurements) = gsub('std', 'Std', names(mergedDATA_measurements))
    ...

Extracts only the measurements on the mean and standard deviation for each measurement.

    mergedDATA_measurements <- mergedDATA[,grepl("mean|std|Subject|Activity_ID", names(mergedDATA))]
    mergedDATA_measurements <- join(mergedDATA_measurements, activityLABELS, by = "Activity_ID", match = "first")
    mergedDATA_measurements <- mergedDATA_measurements[,-1]

From the merged data, create a second, independent tidy data set with the average 
of each variable for each activity and each subject.

    tidyDATA_dt <- data.table(mergedDATA_measurements)
    tidyDATA <- ddply(tidyDATA_dt, c("Subject","Activity"), numcolwise(mean))
    write.table(tidyDATA, file="tidy_measurements.txt", sep=",", row.names = FALSE)


    
    
    

    




