library(data.table)
library(dplyr)

#Download Project Files
#url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip";
#f <- file.path(getwd(),UCI HAR DATASET.zip);
#download.file(url,f)
#unzip(zipfile="UCI HAR DATASET.zip", exdir="UCI HAR DATASET");

testDir<-as.character("UCI HAR DATASET/test/");
trainDir<-as.character("UCI HAR DATASET/train/");
#Labels for features and activity
featurePath <- file.path(getwd(), "UCI HAR DATASET/features.txt");
featureNames<-read.table(featurePath, sep="")
activityPath <- file.path(getwd(), "UCI HAR DATASET/activity_labels.txt");
activityNames<-read.table(activityPath, sep="")

#Reading and Merging Feature Data

testPath_X <- paste(file.path(getwd(), testDir),"X_test.txt",sep="")
testdata_X = read.table(testPath_X, sep=" ");
trainPath_X <- paste(file.path(getwd(),trainDir),"X_train.txt",sep="");
traindata_X = read.table(trainPath_X, sep="");

fulldata_features<-rbind(testdata_X,traindata_X);

#Reading and Merging Activity Data
testPath_Y <- paste(file.path(getwd(),testDir),"Y_test.txt",sep="");
testdata_Y = read.table(testPath_Y, sep="");
trainPath_Y <- paste(file.path(getwd(), trainDir),"Y_train.txt",sep="")
traindata_Y = read.table(trainPath_Y, sep="")

fulldata_activity<-rbind(testdata_Y,traindata_Y);

#Reading and Merging Subject Data
testPath_Subject <- paste(file.path(getwd(), testDir),"subject_test.txt",sep="");
testdata_Subject<-read.table(testPath_Subject, sep="");
trainPath_Subject <- paste(file.path(getwd(), trainDir),"subject_train.txt",sep="")

fulldata_Subject<-rbind(testdata_Subject,traindata_Subject);
#Naming the columns
colnames(fulldata_features) <- t(featureNames[2])
colnames(fulldata_activity) <- 'Activity'
colnames(fulldata_Subject) <- 'Subject'


#Mean and Std
filteredData<-fulldata_features[,grep(".*Mean.*|.*Std.*",colnames(fulldata_features), ignore.case=TRUE)]

#Merging full data
completeData <- cbind(filteredData,fulldata_activity,fulldata_Subject)

#descriptive  names
completeData$Activity <- as.character(completeData$Activity)
for (i in 1:6){
  completeData$Activity[completeData$Activity == i] <- as.character(activityNames[i,2])
}

completeData$Activity <- as.factor(completeData$Activity)
names(completeData)<-gsub("Acc", "Accelerometer", names(completeData))
names(completeData)<-gsub("Gyro", "Gyroscope", names(completeData))
names(completeData)<-gsub("BodyBody", "Body", names(completeData))
names(completeData)<-gsub("Mag", "Magnitude", names(completeData))
names(completeData)<-gsub("^t", "Time", names(completeData))
names(completeData)<-gsub("^f", "Frequency", names(completeData))
names(completeData)<-gsub("tBody", "TimeBody", names(completeData))
names(completeData)<-gsub("-mean()", "Mean", names(completeData), ignore.case = TRUE)
names(completeData)<-gsub("-std()", "STD", names(completeData), ignore.case = TRUE)
names(completeData)<-gsub("-freq()", "Frequency", names(completeData), ignore.case = TRUE)
names(completeData)<-gsub("angle", "Angle", names(completeData))
names(completeData)<-gsub("gravity", "Gravity", names(completeData))

completeData$Subject <- as.factor(completeData$Subject)
completeData <- data.table(completeData)

#Creating Tidy data set
tidyData <- aggregate(. ~Subject + Activity, completeData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
f <- file.path(getwd(), "Tidy.txt");
write.table(tidyData, file = f, row.names = FALSE)
