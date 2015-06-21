## Description of the DATA

#### The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. 
#### These time domain signals (prefix ‘t’ to denote time) were captured at a constant rate of 50 Hz. and the acceleration signal 
#### was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) 
#### – both using a low pass Butterworth filter.The body linear acceleration and angular velocity were derived in time to obtain 
#### Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated 
#### using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).
 
#### A Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, 
#### fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the ‘f’ to indicate frequency domain signals).
 
#### Description of abbreviations of measurements
 
###### leading t or f is based on time or frequency measurements.
###### Body = related to body movement.
###### Gravity = acceleration of gravity
###### Acc = accelerometer measurement
###### Gyro = gyroscopic measurements
###### Jerk = sudden movement acceleration
###### Mag = magnitude of movement
###### mean and SD are calculated for each subject for each activity for each mean and SD measurements.
###### The units given are g’s for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the corresponding jerks.

#### These signals were used to estimate variables of the feature vector for each pattern:
#### ‘-XYZ’ is used to denote 3-axial signals in the X, Y and Z directions. 
#### They total 33 measurements including the 3 dimensions - the X,Y, and Z axes.

###### tBodyAcc-XYZ
###### tGravityAcc-XYZ
###### tBodyAccJerk-XYZ
###### tBodyGyro-XYZ
###### tBodyGyroJerk-XYZ
###### tBodyAccMag
###### tGravityAccMag
###### tBodyAccJerkMag
###### tBodyGyroMag
###### tBodyGyroJerkMag
###### fBodyAcc-XYZ
###### fBodyAccJerk-XYZ
###### fBodyGyro-XYZ
###### fBodyAccMag
###### fBodyAccJerkMag
###### fBodyGyroMag
###### fBodyGyroJerkMag

#### The set of variables that were estimated from these signals are:
         
###### mean(): Mean value
###### std(): Standard deviation


### Data Set Information:

###### The experiments have been carried out with a group of 30 volunteers 
###### within an age bracket of 19-48 years. Each person performed six activities 
###### (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) 
###### wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded 
###### accelerometer and gyroscope, we captured 3-axial linear acceleration and 
###### 3-axial angular velocity at a constant rate of 50Hz. The experiments have been 
###### video-recorded to label the data manually. The obtained dataset has been randomly 
###### partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.
 
###### The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows. 
###### From each window, a vector of features was obtained by calculating variables from the time and frequency domain.
 
###### Files in folder ‘UCI HAR Dataset’ that will be used are:
         
##### 1. SUBJECT FILES:
###### test/subject_test.txt
###### train/subject_train.txt

##### 2. ACTIVITY FILES:
###### test/X_test.txt
###### train/X_train.txt
 
##### 3. DATA FILES:
###### test/y_test.txt
###### train/y_train.txt

##### 4. features.txt - Names of column variables in the dataTable
 
###### 5. activity_labels.txt - Links the class labels with their activity name.

##Download data

DataPath <- "E:/study/Coursera/Data Science Specialization/Getting_and_Cleaning_Data"
setwd(DataPath)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")
##Unzip DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")
##List files
list.files("./data/UCI HAR Dataset")

###Load required packages
install.packages("dplyr")
install.packages("data.table")
install.packages("tidyr")
library(dplyr)
library(data.table)
library(tidyr)

####Read the above files and create data tables.

filesPath <- "E:/study/Coursera/Data Science Specialization/Getting_and_Cleaning_Data/data/UCI HAR Dataset"
#### Read subject files
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

#### Read activity files
dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

####Read data files.
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

####1. Merges the training and the test sets to create one data set.
#### for both Activity and Subject files this will merge the training 
#### and the test sets by row binding 
####and rename variables "subject" and "activityNum"
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

#####combine the DATA training and test files
dataTable <- rbind(dataTrain, dataTest)

##### name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#####column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

##### Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

####2. Extracts only the measurements on the mean and standard deviation for each measurement.
#### Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) 
####var name

##### Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

####3. Uses descriptive activity names to name the activities in the data set
####enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

##### create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

#### 4. Appropriately labels the data set with descriptive variable names.
##### leading t or f is based on time or frequency measurements.
##### Body = related to body movement.
##### Gravity = acceleration of gravity
##### Acc = accelerometer measurement
##### Gyro = gyroscopic measurements
##### Jerk = sudden movement acceleration
##### Mag = magnitude of movement

##### mean and SD are calculated for each subject for each activity for each mean 
##### and SD measurements. The units given are g’s for the accelerometer and rad/sec 
##### for the gyro and g/sec and rad/sec/sec for the corresponding jerks.

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

head(str(dataTable))

#### 5. From the data set in step 4, creates a second, independent tidy data set 
#### with the average of each variable for each activity and each subject.

######write to text file on disk
write.table(dataTable, "TidyData.txt", row.name=FALSE)
