library(dplyr)
#1. Merges the training and the test sets to create one data set.

train_set<-read.table("./UCI HAR Dataset/train/X_train.txt")
train_subjects<-read.table("./UCI HAR Dataset/train/subject_train.txt")
train_labels<-read.table("./UCI HAR Dataset/train/y_train.txt")

test_set<-read.table("./UCI HAR Dataset/test/X_test.txt")
test_subjects<-read.table("./UCI HAR Dataset/test/subject_test.txt")
test_labels<-read.table("./UCI HAR Dataset/test/y_test.txt")

train<-cbind(train_set, train_subjects, train_labels)
test<-cbind(test_set, test_subjects, test_labels)

all_data<-rbind(train, test)


#2. Extracts only the measurements on the mean and standard deviation for each measurement.
# We need first to load features names and assign to colnames in the all_data dataset
features<-read.table("./UCI HAR Dataset/features.txt", as.is=TRUE)

colnames(all_data)<-c(features[,2], "subject", "activity")
# find all columns about mean and standard deviation

col_select <- c(grep("mean|std", features[,2], value=TRUE), "subject", "activity")
#thought about using select, but it throws an error because some of the column names are repeated
#so I built a vector with the columns with mean or std in its name and added the last ones for subject and activity
#then selected only this columns
extracted_data<-all_data[,col_select]

#3. Uses descriptive activity names to name the activities in the data set
#Extracted date does not have any repeated colname, so we can use select, ... from dplyr
#we need first to load activity names
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")

#now using factor we find the description of the activity and replace the activity column in the dataset by using mutate
extracted_data<-mutate(extracted_data, activity=as.character(factor(activity, levels = activities[, 1], labels = activities[, 2])))


#4. Appropriately labels the data set with descriptive variable names.
# From the features_info.txt file we now about the meaning of the abbreviations used in the colnames. We're going to
# substitute some strings

column_names <- colnames(extracted_data)
column_names <- gsub("^t", "TimeDomain", column_names)
column_names <- gsub("^f", "FrequencyDomain", column_names)
column_names <- gsub("Acc", "Accelerator", column_names)
column_names <- gsub("Gyro", "Gyroscope", column_names)
column_names <- gsub("Mag", "Magnitude", column_names)
column_names <- gsub("std", "Standard", column_names)
column_names <- gsub("mean", "Mean", column_names)
column_names <- gsub("Freq", "Frequency", column_names)


# Don't like - sign and () in colnames
column_names <- gsub("[-|(|)]", "", column_names)

# Some "Body" are repeated
column_names <- gsub("BodyBody", "Body", column_names)


colnames(extracted_data)<-column_names



#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
activity_subject_average <- extracted_data %>%
  group_by(subject, activity) %>%
  summarize_all(mean)

# Resulting data are in activity_subect_average. Let's save it
write.table(activity_subject_average, "tidy_data_set.txt", quote=FALSE, row.names=FALSE)
