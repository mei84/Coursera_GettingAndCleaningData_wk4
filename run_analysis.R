rm(list=ls())

setwd("C:/Users/ÀçÇö/Desktop/R/GCdata")
getwd()

library(dplyr)
library(data.table)

x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
s_test <- read.table("subject_test.txt")

x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
s_train <- read.table("subject_train.txt")

feature <- read.table("features.txt")
label <- read.table("activity_labels.txt")



##1.Merges the training and the test sets to create one data set.
x <- rbind(x_test, x_train)
y <- rbind(y_test, y_train)
s <- rbind(s_test, s_train)

##naming
names(x) <- feature[[2]]
names(y) <- "Activity"
names(s) <- "Subject"

##merge data
m <- cbind(s,y,x)

head(m,5)

##2.Extracts only the measurements on the mean and standard deviation for each measurement. 
data_Mean <- grep("Mean", names(m))
data_mean <- grep("mean", names(m))
data_Std <- grep("Std", names(m))
data_std <- grep("std", names(m))
t_data <- c(data_Mean, data_mean, data_Std, data_std)
Edata <-m[,c(1,2,t_data)]



##3.Uses descriptive activity names to name the activities in the data set
Edata <- mutate(Edata, Activity=factor(Activity, labels=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")))
str(Edata)

##4.Appropriately labels the data set with descriptive variable names. 

names(Edata) <- gsub("^t", "Time", names(Edata)) 
names(Edata) <- gsub("^f", "Frequency", names(Edata)) 
names(Edata) <- gsub("BodyBody", "Body", names(Edata)) 
names(Edata) <- gsub("Acc", "Accelerometer", names(Edata))
names(Edata) <- gsub("Gyro", "Gyroscope",names(Edata))
names(Edata) <- gsub("Mag", "Magnitude", names(Edata))
names(Edata) <- gsub("angle", "Angle", names(Edata))
names(Edata) <- gsub("gravity", "Gravity", names(Edata))
names(Edata)


##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Edata$Subject <- as.factor(Edata$Subject)
Edata <- data.table(Edata)

tData <- aggregate(. ~Subject + Activity, Edata, mean)
tData <- tData[order(tData$Subject,tData$Activity),]

write.table(tData,"tidy.txt", row.names=F)