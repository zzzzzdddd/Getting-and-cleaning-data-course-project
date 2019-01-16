# download the file and unzip. If file already exist, then don't download
link <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
flnm <- "UCIdata.zip"
if (!file.exists("flnm")){
download.file(link,flnm,method = "curl")
}
if (!file.exists("UCI HAR Dataset")) { 
  unzip(flnm) 
}  


# According to the README.txt, read the data and assign to variables
# all features
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
# link class labels with their activity name
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
# test subjects
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
# test set
tsset <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
# test label
tslab <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
# training subjects
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
# training set
trset <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
# training label
trlab <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# merge training and test set into one.
library(dplyr)
x <- rbind(trset, tsset)
y <- rbind(trlab, tslab)
subjects <- rbind(subject_train, subject_test)
md <- cbind(subjects, y, x)

# Extracts only the measurements on the mean and standard deviation for 
# each measurement.
td <- md %>% 
  select(subject, code, contains("mean"), contains("std"))

# Uses descriptive activity names to name the activities in the data set
td$code <- activities[td$code, 2]

# Appropriately labels the data set with descriptive variable names.
names(td)[2] = "activity"
names(td)<-gsub("Acc", "accelerometer ", names(td))
names(td)<-gsub("Gyro", "gyroscope ", names(td))
names(td)<-gsub("BodyBody", "body ", names(td))
names(td)<-gsub("Body", "body ", names(td))
names(td)<-gsub("Mag", "magnitude ", names(td))
names(td)<-gsub("^t", "time of ", names(td))
names(td)<-gsub("^f", "frequency of ", names(td))
names(td)<-gsub("tbody", "body ", names(td))
names(td)<-gsub("-mean()", "mean", names(td), ignore.case = TRUE)
names(td)<-gsub(".std", "standard deviation", names(td), ignore.case = TRUE)
names(td)<-gsub("-freq()", "frequency", names(td), ignore.case = TRUE)
names(td)<-gsub("angle", "angle of ", names(td))
names(td)<-gsub("Gravity", "gravity ", names(td))
names(td)<-gsub("Jerk","jerk ", names(td))
names(td)<-gsub("meanFreq", "mean frequency", names(td))
names(td)<-gsub("gravityMean", "gravity mean", names(td))
names(td)<-gsub(".mean", "mean", names(td))

# From the data set in in the previous step, create another independent 
# tidy data set with the average of each variable for each activity and 
# each subject.
sitd <- td %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
View(sitd)
write.table(sitd, "independent data.txt", row.name=FALSE)