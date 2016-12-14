suppressMessages(library(Rcpp))
suppressMessages(library(plyr)) #
suppressMessages(library(dplyr)) #
suppressMessages(library(reshape2)) #
suppressMessages(library(data.table)) #
suppressMessages(library(openxlsx))
suppressMessages(library(devtools))
suppressMessages(library(tidyr))
suppressMessages(library(descr))
suppressMessages(library(zoo))
suppressMessages(library(stats))
suppressMessages(library(xlsx))
library(rJava)
library(XML)


Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip")
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_111")

wd <- setwd("C:/Users/PMF/Documents/Coursera/Data/wearables/UCI HAR Dataset")

#Load the two data sets
datatrain <- read.table("./train/X_train.txt")
datatest <- read.table("./test/X_test.txt")

#Merge the two data sets -> row binding
data <- rbind(datatrain, datatest)

#Add variable names
variablenames <- read.table("features.txt")
colnames(data) <- variablenames[,"V2"]

#Extract only measurements on mean and standard deviation
table(grepl("(mean|std)[^Freq]", variablenames$V2))
mean_std <- grepl("(mean|std)[^Freq]", variablenames$V2)
datasub <- data[, mean_std]

#Descriptive names for activity variables
activities_train <- read.table("./train/y_train.txt")
activities_test <- read.table("./test/y_test.txt")
activity_names <- read.table("activity_labels.txt") #matches activity names with numbers (class labels)
activities <- rbind(activities_train, activities_test)
activities <- join(activities, activity_names)

#Descriptive names for variables in data set
colnames(datasub) <- gsub("^t", "Time_", colnames(datasub))
colnames(datasub) <- gsub("^f", "Frequency_", colnames(datasub))
colnames(datasub) <- gsub("mean", "_Mean", colnames(datasub))
colnames(datasub) <- gsub("std", "_Std", colnames(datasub))
colnames(datasub) <- gsub("-", "", colnames(datasub))
colnames(datasub) <- gsub("\\(\\)", "_", colnames(datasub))
colnames(datasub) <- gsub("_$", "", colnames(datasub))
colnames(datasub) <- gsub("BodyBody", "Body", colnames(datasub))

#Independent tidy data set with the average of each variable for each activity and each subject
Activity <- activities[,2]
datasub <- cbind(datasub, Activity)

subject_train <- read.table("./train/subject_train.txt")
subject_test <- read.table("./test/subject_test.txt")
subjects <- rbind(subject_train, subject_test)
datasub <- cbind(datasub, subjects)
datasub <- rename(datasub, Subject = V1)

Average <- datasub %>%
        group_by(Subject, Activity) %>%
        summarise_each(funs(mean))
Average
write.table(Average, file = "tidydataset")
colnames(Average) <- gsub("^T", "MeanOf_T", colnames(Average))
colnames(Average) <- gsub("^F", "MeanOf_F", colnames(Average))
