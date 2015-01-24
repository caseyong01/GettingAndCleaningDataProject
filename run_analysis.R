require(data.table)
require(reshape)
defaultdir<-getwd()

## ----- Acknowledgement ------
## Many thanks to Benjamin Chan from Portland,OR. 
## I have learnt a lot from referencing his codes and documentation on this assignment.


## ----- Set directory and read in the files ------- 
mydirectory<-paste(defaultdir, "UCI HAR Dataset/test", sep="/")
setwd(mydirectory)
testX <- data.table(read.table("X_test.txt"))
testY <- data.table(read.table("y_test.txt"))
subject_test <-data.table(read.table("subject_test.txt"))


mydirectory<-paste(defaultdir, "UCI HAR Dataset/train", sep="/")
setwd(mydirectory)
trainX <- data.table(read.table("X_train.txt"))
trainY <- data.table(read.table("y_train.txt"))
subject_train <-data.table(read.table("subject_train.txt"))

## ----- row bind the different data to merge into one dataset
dtData<-rbind(testX,trainX)
dtLabel <-rbind(testY,trainY)
setnames(dtLabel,"V1","activityNum")
dtSubject <- rbind(subject_test,subject_train)
setnames(dtSubject,"V1","subject")

## ------ column bind the labels, subject to complete the merging of dataset
dtSubject <- cbind(dtSubject, dtLabel)
dtData <-cbind(dtSubject, dtData)
setkey(dtData, subject, activityNum)

## ------ read in the feature file and extract the feature names
mydirectory<-paste(defaultdir, "UCI HAR Dataset", sep="/")
setwd(mydirectory)
featureData <- data.table(read.table("features.txt"))
setnames(featureData, names(featureData),c("featureNum","featureName"))
featureData <- featureData[grepl("mean\\(\\)|std\\(\\)", featureName)]
featureData$featureCode <- featureData[,paste0("V",featureNum)]

select <- c(key(dtData), featureData$featureCode)
dtData <- dtData[, select, with=FALSE]

## ----- read and extract the activity labels
ActivityNames <- data.table(read.table("activity_labels.txt"))
setnames(ActivityNames, names(ActivityNames), c("activityNum", "activityName"))

## ----- merge the activity names to the main dataset using activity number
dtData <-merge(dtData, ActivityNames, by="activityNum", all.x=TRUE)
setkey(dtData,subject,activityNum, activityName)

## ----- use melt function to reshape the table from a short and wide format to a tall and narrow format for easier viewing and manipulation
dtData <-data.table(melt(dtData,key(dtData), variable.name="featureCode"))
setnames(dtData, "variable","featureCode")

## ----- merge the feature data to the main dataset.
dtData <- merge(dtData,featureData[,list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

## ----- create a new variable "activity" which is same as activitName as a factor class 
## ----- create a new variable "feature" which is same as featureName as a factor class 
dtData$activity <- factor(dtData$activityName)
dtData$feature <- factor(dtData$featureName)


## ----- to extract the features from the variable "feature", a function is created
## using the grepl()
grepthis <- function (regex) {
  grepl(regex, dtData$feature)
}

## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dtData$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))

x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dtData$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dtData$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dtData$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dtData$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dtData$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dtData$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

## ----- Create a tidy dataset and write to the disk
setkey(dtData, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dtData[, list(count = .N, average = mean(value)), by=key(dtData)]
write.table(dtTidy, file = "TidyData.txt", sep = "\t", row.names = FALSE)




