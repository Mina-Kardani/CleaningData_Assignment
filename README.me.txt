Prepare the necessary input files for run_analysis.R

1-After downloading the zip file from Url and extracting , copy below files 

test/subject_test.txt
test/x_test.txt
test/y_test.txt
train/subject_train.txt
train/x_train.txt
train.y_train.txt
activity_labels.txt
features_info.txt
features.txt
in the root directory as the .R module.

The data of related files are:
    Values of Varible Activity consist of data from “Y_train.txt” and “Y_test.txt”
    values of Varible Subject consist of data from “subject_train.txt” and subject_test.txt"
    Values of Varibles Features consist of data from “X_train.txt” and “X_test.txt”
    Names of Varibles Features come from “features.txt”
    levels of Varible Activity come from “activity_labels.txt”

So we will use Activity, Subject and Features as part of descriptive variable names for data in data frame.

The run_analysis.R is including following steps:

1- Read the ACtivity files, Subject files and Features files.
2- concatenate the data tables by rows
3- set the variable names
4- merge columns to get the data frame "merged_data" for all data
5- subset name of features by measurment on the mean and standard deviation and put the result in "meanStd_columns"
6-subset the  "meanStd_columns" by adding requested columnes and selected names of features in "extracted_data"
7-factorise variable "activity" in the "extracted_data" using descriptive activity names provided in "activity_labels.txt"
8- change the variable name to descriptive names.
#t-->Time
#f-->Frequency
#Acc--> Accelerator
#Gyro-->Gyroscope
#Mag-->Magnitude
#BodyBody-->Body
#-mean()_-->Mean
#_std()_-->STD
#_freq()_-->Frequency

9-create an indipendent tidy data set with the average of each variable for each activity and each subject using "extracted_data"
and save the result in "tidy_data"

10-write "tidy_data" in the file "Tidydata.txt"
11- generate a codebook for "tidy_data" using "memisc" library and save it in "codebook.txt"

