### Programing assignment 
##
##You should create one R script called run_analysis.R that does the following. 
##1. Merges the training and the test sets to create one data set.
##2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##3. Uses descriptive activity names to name the activities in the data set
##4. Appropriately labels the data set with descriptive variable names. 
##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##
require("dplyr")

  directory <- getwd()
  testDir <- file.path(directory,"test")
  trainDir <- file.path(directory,"train")
  tiny_datafile <- file.path(getwd(),"tiny_data.txt")
  
  #test set
  #load the features 
  features <-  read.table(file.path(directory,"features.txt"),header=FALSE)[,2]
  
  #load the activity_labels 
  activity_labels <-  read.table(file.path(directory,"activity_labels.txt"),header=FALSE)
  
  X_test <-  read.table(file.path(testDir,"X_test.txt"),header=FALSE)
  Y_test <-  read.table(file.path(testDir,"y_test.txt"),header=FALSE)
  subject_test <-  read.table(file.path(testDir,"subject_test.txt"),header=FALSE)
  
  
  #add activity column
  names(Y_test) <- c("C1")
  #add subject column
  names(subject_test) <- c("C2")
  
  # 3. translate the activty id to the labels
  Y_test[,1] <- activity_labels[Y_test[,1],2]
  
  #build the aggregated test dataset
  test_dataset <- cbind(subject_test,Y_test,X_test)
  
  #train data
  X_train <-  read.table(file.path(trainDir,"X_train.txt"),header=FALSE)
  Y_train <-  read.table(file.path(trainDir,"y_train.txt"),header=FALSE)
  subject_train <-  read.table(file.path(trainDir,"subject_train.txt"),header=FALSE)
  
  #add activity column
  names(Y_train) <- c("C1")
  #add subject column
  names(subject_train) <- c("C2")
  
  # 3. translate the activty id to the labels
  Y_train[,1] <- activity_labels[Y_train[,1],2]
  
  #build the aggregated train dataset
  train_dataset <- cbind(subject_train,Y_train,X_train)
  
  ## 1. merge test and train dataset 
  merged_dataset <- rbind(test_dataset,train_dataset)
    
  #clean up to release large objects
  rm(list=c("Y_test","X_test","Y_train","X_train","subject_test","subject_train","activity_labels"))
  
  #5. create subset  of merged data calculating the averages,omit column 3 and 4 that calculate variable name means
  tiny_data <- aggregate(merged_dataset,list(subject=merged_dataset$C1,Activity=merged_dataset$C2),mean)[c(-3,-4)]
  
  #4. apply colun names to tiny merged dataset
  names(merged_dataset) <- combine(c("subject","Activity"),features)
  
  ##2. extract the std deviation and mean 
  dataset_columns = names(merged_dataset)  
  measurements_subset <- subset(merged_dataset,select=dataset_columns[grep("subject|Activity|mean|std",names(merged_dataset))])
  
  #apply colun names to tiny data
  names(tiny_data) <- combine(c("subject","Activity"),features)
  rm("features")
  write.table(row.names=FALSE,tiny_data,file = tiny_datafile)
  
  print("merged dataset is stored in - > merged_dataset")
  print("extracted measurements is stored in - > measurements_subset")
  print(sprintf("tiny_data exported to %s ",tiny_datafile))
 
