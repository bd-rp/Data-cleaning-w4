#run_analysis.R
#Step0:read all data needed
activity_labels = activity_labels <- read.csv("UCI HAR Dataset/activity_labels.txt", header = FALSE, sep ="")
features = features <- read.csv("UCI HAR Dataset/features.txt", header = FALSE, sep ="")

subject_train = subject_train <- read.csv("UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep ="")
subject <- "subject"
colnames(subject_train) <- subject

y_train =  read.csv("UCI HAR Dataset/train/y_train.txt", header = FALSE, sep ="")
y_train_col <- "activity"
colnames(y_train) <- y_train_col

X_train = read.csv("UCI HAR Dataset/train/x_train.txt", header = FALSE, sep ="")
feature_list <- features$V2
colnames(x_train) <- feature_list

subject_test = read.csv("UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep ="")
subject <- "subject"
colnames(subject_test) <- subject

y_test = read.csv("UCI HAR Dataset/test/y_test.txt", header = FALSE, sep ="")
y_test_col <- "activity"
colnames(y_test) <- y_test_col

X_test = read.csv("UCI HAR Dataset/test/x_test.txt", header = FALSE, sep ="")
feature_list <- features$V2
colnames(x_test) <- feature_list

#Step1: Merges the training and the test sets to create one data set.
merged_data <- rbind(cbind(subject_train, y_train, x_train),
                     cbind(subject_test,  y_test,  x_test))
#Step2: #Extracts only the measurements on the mean and standard deviation for each measurement. 
target_column_index <- grep("mean\\(\\)|std\\(\\)", names(merged_data))
target_rows_index <- c(1, 2, target_column_index + 2)
target_data <- merged_data[ , target_rows_index]
#Step3:Uses descriptive activity names to name the activities in the data set
target_data[[2]] <- factor(target_data[[2]],
                           levels = activity_labels[[1]],
                           labels = activity_labels[[2]])
#STEP4: creates a second, independent tidy data set with the average of each variable for each activity and each subject.
summary <- tidy_data %>%
  group_by(subject, activity) %>%
  summarise_all(tibble::lst(mean)) %>%
  ungroup()
#STEP5: Save new table
write.table(summary, "summary.txt", row.names = FALSE)