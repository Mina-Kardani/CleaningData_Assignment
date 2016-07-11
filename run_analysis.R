library(data.table)
library(dplyr)
#read the name of the activities and the name of the features
activity_labels = read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
feature_names = read.table("UCI HAR Dataset/features.txt")

#Reading 3 parts of training data (subjects,activities,features)
subject_train = read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activity_train = read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
feature_train = read.table("UCI HAR Dataset/train/x_train.txt", header = FALSE)

#Reading 3 parts of test data (subjects,activities,features)
subject_test = read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activity_test = read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
feature_test = read.table("UCI HAR Dataset/test/x_test.txt", header = FALSE)

#Merge corresponding parts of training and test date
subject_data =rbind(subject_train,subject_test)
activity_data =rbind(activity_train,activity_test)
feature_data =rbind(feature_train,feature_test)

#set the name of the columns in feature data based on 
#the name provided in second column of feature_names
colnames(feature_data) = t(feature_names[2])
colnames(activity_data)="Activity"
colnames(subject_data)="Subject"

#1-Merges the training and the test sets to create one data set
merged_data=cbind(feature_data,activity_data,subject_data)

#2-Extracts only the measurements on the mean and standard deviation for each measurement
meanStd_columns = grep(".*Mean.*|.*Std.*",names(merged_data),ignore.case = TRUE)
Requested_columns <- c(meanStd_columns, 562, 563)
dim(merged_data)
extracted_data = merged_data[,Requested_columns]
dim(extracted_data)

#3-Uses descriptive activity names to name the activities in the dataset
#Change Activity variable from integer to character
extracted_data$Activity = as.character(extracted_data$Activity)
for(i in 1:6)
{
  extracted_data$Activity[extracted_data$Activity==i] = activity_labels[i,2]
}

extracted_data$Activity = as.factor(extracted_data$Activity)

#4-Appropriately labels the data set with descriptive variable names
#t-->Time
#f-->Frequency
#Acc--> Accelerator
#Gyro-->Gyroscope
#Mag-->Magnitude
#BodyBody-->Body
#-mean()_-->Mean
#_std()_-->STD
#_freq()_-->Frequency
names(extracted_data) = gsub("^t","Time",names(extracted_data))
names(extracted_data) = gsub("^f","Frequency",names(extracted_data))
names(extracted_data) = gsub("Acc","Accelerator",names(extracted_data))
names(extracted_data) = gsub("Gyro","Gyroscope",names(extracted_data))
names(extracted_data) = gsub("Mag","Magnitude",names(extracted_data))
names(extracted_data) = gsub("BodyBody","Body",names(extracted_data))
names(extracted_data) = gsub("_mean()_","Mean",names(extracted_data))
names(extracted_data) = gsub("_Std()_","STD",names(extracted_data))
names(extracted_data) = gsub("_Freq()_","Frequency",names(extracted_data))

names(extracted_data)

#From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject
#First we need to change subject to a factor variable
extracted_data$Subject = as.factor(extracted_data$Subject)
tidy_data <- aggregate(. ~ Subject + Activity, data=extracted_data,FUN = "mean")
tidy_data = tidy_data[order(tidy_data$Subject,tidy_data$Activity),]
write.table(tidy_data,file="finalData.txt",row.names = FALSE)

