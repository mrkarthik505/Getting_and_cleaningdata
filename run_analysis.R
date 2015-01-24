run_analysis <- function(){
  setwd("C:/Users/radhakrishnak/Documents/UCI HAR Dataset")
  test_subject <- read.table("./test/subject_test.txt")
  test_subject <- rename(test_subject,subject = V1) 
  test_y <- read.table("./test/y_test.txt")
  test_y <- rename(test_y,label = V1)
  test_x <- read.table("./test/X_test.txt")
  test_tot <- cbind(test_x,test_subject,test_y)
  train_subject <- read.table("./train/subject_train.txt")
  train_subject <- rename(train_subject,subject = V1)
  train_y <- read.table("./train/y_train.txt")
  train_y <- rename(train_y,label = V1)
  train_x <- read.table("./train/X_train.txt")
  train_tot <- cbind(train_x,train_subject,train_y)
  merge_data <- rbind(test_tot,train_tot)
  features <- read.table("./features.txt")
  names(merge_data) <- features[,2]
  names(merge_data)[562] <- "subject"
  names(merge_data)[563] <- "label"
  ext_data <- merge_data[,c(c(1:6),c(41:46),c(81:86),c(121:126),c(161:166),c(201:202),
                            c(214:215),c(227,228),c(240:241),c(253:254),c(266:271),
                            c(345:350),c(424:429),c(503:504),c(516:517),c(529:530),
                            c(542:543),c(562:563))]
  ext_data[,68] <- as.numeric(as.character(ext_data[,68]))
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
  names(ext_data) <- gsub("[[:punct:]]", " ", names(ext_data))
  names(ext_data) <- gsub("[[:space:]]", "", names(ext_data))
  grp <- group_by(ext_data,ext_data[,67],ext_data[,68])
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
  names(main_data)[1] <- "subject"
  names(main_data)[2] <- "label"
}