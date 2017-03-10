
run_analysis <- function() {
  
  ## function to create a tidy data set for the Getting and Cleaning Data course
  ## The data set can be found here: 
  ## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
  ## The data set should be placed in your working directory and unzipped
  ## After unzipping the files should be in a folder called "UCI HAR Dataset"
  ## The output file will be located at "UCI HAR Dataset/FinalProjectOutput.txt"
  
  ## get X files and join them
  
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
  x_data <- rbind(x_test,x_train)
  
  ##get column names
  cn <- read.table("UCI HAR Dataset/features.txt")
  names(x_data) <- cn[,2]
  
  ## get Y files (row labels) and join them in same order
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
  y_data <- rbind(y_test,y_train) 
  
  names(y_data)<- c("activities")
  
  ## get subject files and join them
  sub_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  sub_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  sub_data <- rbind(sub_test,sub_train) 
  
  names(sub_data)<- c("subjects")
  ## join them together
  final_ds <- cbind(sub_data,y_data,x_data)
   
  ## filter to keep only the mean and standard deviation columns
  final_ds <- final_ds[,grep("subjects|activities|mean()|std()",names(final_ds))]
  
  ## get activity labels
  act_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
  names(act_labels)<-c("lab_id","activity_name")
  
  ## translate label numbers into meaningful text
  library(dplyr)
  act_labels <- tbl_df(act_labels)
  final_ds <-  tbl_df(final_ds)
  final_ds <- inner_join(act_labels,final_ds,by=c("lab_id"="activities"))%>%
                          select(-lab_id)
  ## summarize - average of each activity and subject
  summary_ds <- group_by(final_ds,activity_name,subjects)%>%
                          summarize_each(funs(mean))
  ## save to file
  write.table(summary_ds,"UCI HAR Dataset/FinalProjectOutput2.txt", row.name=FALSE)
  
}