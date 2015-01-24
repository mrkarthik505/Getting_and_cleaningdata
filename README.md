The task at hand

 You should create one R script called run_analysis.R that does the following. 

    1.Merges the training and the test sets to create one data set.
    2.Extracts only the measurements on the mean and standard deviation for each measurement. 
    3.Uses descriptive activity names to name the activities in the data set
    4.Appropriately labels the data set with descriptive variable names. 
    5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The code is given below

Note : We use 'dplyr' external package in the code ,so please upload the package before running the query

/* we set the directory to the dataset folder */
setwd("C:/Users/radhakrishnak/Documents/UCI HAR Dataset")

/* reading the subject to a data frame */
  test_subject <- read.table("./test/subject_test.txt")

/* changing the column name because the while merging the column with the results it will create duplicate column names */
  test_subject <- rename(test_subject,subject = V1)

 /* reading the y_test(label) to a data frame */
  test_y <- read.table("./test/y_test.txt")

/* changing the column name because the while merging the column with the results it will create duplicate column names */
  test_y <- rename(test_y,label = V1)

/* reading the X_test(experiment results) to a data frame */
  test_x <- read.table("./test/X_test.txt")

/* combinging all thr three dataframes so that all the experiment is with subject number and activity label */
  test_tot <- cbind(test_x,test_subject,test_y)

/*Same set of activities done on the test data are done here in train data
  train_subject <- read.table("./train/subject_train.txt")
  train_subject <- rename(train_subject,subject = V1)
  train_y <- read.table("./train/y_train.txt")
  train_y <- rename(train_y,label = V1)
  train_x <- read.table("./train/X_train.txt")
  train_tot <- cbind(train_x,train_subject,train_y)


/*Now we have a complete set of the test data and the complete set of the training data, so we are combining both the data frames into one data frame using rbind */
  merge_data <- rbind(test_tot,train_tot)

/*Now we are taking all the results name from the features.txt and placing it in a data frame with read.table*/
  features <- read.table("./features.txt")

/* We have merge_data column names as V1,V2 and so on and now we are replacing it with names from the features dataframe */
  names(merge_data) <- features[,2]
  names(merge_data)[562] <- "subject"
  names(merge_data)[563] <- "label"

/* Since we need only the columns with mean and std in it we subsetting the merged data and saving it in a new data frame */
  ext_data <- merge_data[,c(c(1:6),c(41:46),c(81:86),c(121:126),c(161:166),c(201:202),
                            c(214:215),c(227,228),c(240:241),c(253:254),c(266:271),
                            c(345:350),c(424:429),c(503:504),c(516:517),c(529:530),
                            c(542:543),c(562:563))]

/* Now after subsetting i get a result of 68 columns and to change the activity numbers to the names i am converting the label column type to numeric */
  ext_data[,68] <- as.numeric(as.character(ext_data[,68]))

/*Converting all the labels to the activity names using for loop*/
  for(i in 1:10299){
    if(ext_data[i,68] == 1){
      ext_data[i,68] = "WALKING"
    }
    if(ext_data[i,68]==2){
      ext_data[i,68] = "WALKING_UPSTAIRS"
    }
    if(ext_data[i,68] ==3){
      ext_data[i,68] = "WALKING_DOWNSTAIRS"
    }
    if(ext_data[i,68] == 4){
      ext_data[i,68] = "SITTING"
    }
    if(ext_data[i,68] == 5){
      ext_data[i,68] = "STANDING"
    }
    if(ext_data[i,68] == 6){
      ext_data[i,68] = "LAYING"
    }
  }

/* Now i am remove all the special characters and white spaces from the column names using gsub function */
  names(ext_data) <- gsub("[[:punct:]]", " ", names(ext_data))
  names(ext_data) <- gsub("[[:space:]]", "", names(ext_data))

/* Now i am grouping the entire data frame by label and subject*/
  grp <- group_by(ext_data,ext_data[,67],ext_data[,68])

/*And i am using the group data and all the results i am calculating the mean of specific subject for the specific activity label */

  main_data <- summarize(grp,mean(tBodyAccmeanX),mean(tBodyAccmeanY),mean(tBodyAccmeanZ),
                         mean(tBodyAccstdX), mean(tBodyAccstdY), mean(tBodyAccstdZ),
                         mean(tGravityAccmeanX), mean(tGravityAccmeanY),mean(tGravityAccmeanZ),
                         mean(tGravityAccstdX),mean(tGravityAccstdY),mean(tGravityAccstdZ),
                         mean(tBodyAccJerkmeanX),mean(tBodyAccJerkmeanY),mean(tBodyAccJerkmeanZ),
                         mean(tBodyAccJerkstdX),mean(tBodyAccJerkstdY),mean(tBodyAccJerkstdZ),
                         mean(tBodyGyromeanX),mean(tBodyGyromeanY),mean(tBodyGyromeanZ),
                         mean(tBodyGyrostdX),mean(tBodyGyrostdY),mean(tBodyGyrostdZ),
                         mean(tBodyGyroJerkmeanX), mean(tBodyGyroJerkmeanY),mean(tBodyGyroJerkmeanZ),
                         mean(tBodyGyroJerkstdX),mean(tBodyGyroJerkstdY),mean(tBodyGyroJerkstdZ),
                         mean(tBodyAccMagmean),mean(tBodyAccMagstd),mean(tGravityAccMagmean),
                         mean(tGravityAccMagstd),mean(tBodyAccJerkMagmean),mean(tBodyAccJerkMagstd),
                         mean(tBodyGyroMagmean),mean(tBodyGyroMagstd),mean(tBodyGyroJerkMagmean),
                         mean(tBodyGyroJerkMagstd),mean(fBodyAccmeanX),mean(fBodyAccmeanY),
                         mean(fBodyAccmeanZ),mean(fBodyAccstdX),mean(fBodyAccstdY),mean(fBodyAccstdZ),
                         mean(fBodyAccJerkmeanX),mean(fBodyAccJerkmeanY),mean(fBodyAccJerkmeanZ),
                         mean(fBodyAccJerkstdX),mean(fBodyAccJerkstdY),mean(fBodyAccJerkstdZ),
                         mean(fBodyGyromeanX),mean(fBodyGyromeanY),mean(fBodyGyromeanZ),
                         mean(fBodyGyrostdX),mean(fBodyGyrostdY),mean(fBodyGyrostdZ),
                         mean(fBodyAccMagmean),mean(fBodyAccMagstd),mean(fBodyBodyAccJerkMagmean),
                         mean(fBodyBodyAccJerkMagstd),mean(fBodyBodyGyroJerkMagstd),
                         mean(fBodyBodyGyroMagstd),mean(fBodyBodyGyroJerkMagmean),
                         mean(fBodyBodyGyroJerkMagstd),mean(fBodyBodyGyroMagmean))
/*naming the subject and label columns */
  names(main_data)[1] <- "subject"
  names(main_data)[2] <- "label"