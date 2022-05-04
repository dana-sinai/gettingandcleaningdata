rm(list = ls())

# define url and choose destination
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile<-"data.zip"

# download zip and extract
download.file(url, destfile)
unzip("data.zip", exdir="data")

# read files into tables and bind
features <- read.table("C:/Users/danas/Downloads/data/UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("C:/Users/danas/Downloads/data/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

subject_train<-read.table("C:/Users/danas/Downloads/data/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
subject_test<-read.table("C:/Users/danas/Downloads/data/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

x_train<-read.table("C:/Users/danas/Downloads/data/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
x_test<-read.table("C:/Users/danas/Downloads/data/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)


y_train<-read.table("C:/Users/danas/Downloads/data/UCI HAR Dataset/train/y_train.txt", col.names = "code")
y_test<-read.table("C:/Users/danas/Downloads/data/UCI HAR Dataset/test/y_test.txt", col.names = "code")

# 1.  Merges the training and the test sets to create one data set.

subjects<-rbind(subject_train,subject_test)
x <- rbind(x_train, x_test)
y<-rbind(y_train,y_test)

merged<-cbind(subjects,y,x)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

data <- merged %>%
        select(subject,code,contains("mean"), contains("std"))

# 3. Uses descriptive activity names to name the activities in the data set

data$code <- activities[data$code, 2]

# 4. Appropriately labels the data set with descriptive variable names.

data <- data %>%
        rename(activity=code) %>%
        rename_with(~gsub("Acc.","_Accelerometer_",.)) %>%
        rename_with(~gsub("Gyro.","_Gyroscope_",.))%>%
        rename_with(~gsub("BodyBody","_Body",.))%>%
        rename_with(~gsub("Mag", "Magnitude",.))%>%
        rename_with(~gsub("angle", "Angle_",.))%>%
        rename_with(~gsub("^t", "Time_",.))%>%
        rename_with(~gsub("^f","Frequency_",.))%>%
        rename_with(~gsub("tBody", "TimeBody",.))%>%
        rename_with(~gsub("mean.", "Mean",.))%>%
        rename_with(~gsub("-std()", "STD",.))%>%
        rename_with(~gsub("gravity", "Gravity",.))
        

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

data.final<- data %>%
        group_by(activity,subject) %>%
        summarize_all(funs(mean))


write.table(data.final, "data.final.txt", row.name=FALSE)
