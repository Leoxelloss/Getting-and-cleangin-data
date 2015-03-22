run_analysis<- function(){
  #set variable for directory
  trainDir<-"train"
  testDir<-"test"
  #read in all the files and combined them into one table
  
  ## features
  filenames<-"features.txt"
  features<-read.table(filenames,header=FALSE)
  
  #train file list
  filenames <- list.files(trainDir, pattern="*.txt", full.names=TRUE)
  ## read the files
  data <- lapply(filenames,read.table,header=FALSE)
  names(data[[1]])<-c("Subject")
  names(data[[2]])<-features$V2
  names(data[[3]])<-c("Activity")
  ## combine the first set of files
  trainSet <- cbind(data[[1]],data[[3]],data[[2]])

  #test file list
  filenames <- list.files(testDir, pattern="*.txt", full.names=TRUE)
  ## read the files
  data <- lapply(filenames,read.table,header=FALSE)
  names(data[[1]])<-c("Subject")
  names(data[[2]])<-features$V2
  names(data[[3]])<-c("Activity")
  ## combine the first set of files
  testSet <- cbind(data[[1]],data[[3]],data[[2]])

  #combine the two set of data
  Combine_data <- rbind(train=trainSet,test=testSet)
 
  #change all activity index to activity name
  Combine_data$Activity[Combine_data$Activity==1] <-"WALKING"
  Combine_data$Activity[Combine_data$Activity==2] <-"WALKING_UPSTAIRS"
  Combine_data$Activity[Combine_data$Activity==3] <-"WALKING_DOWNSTAIRS"
  Combine_data$Activity[Combine_data$Activity==4] <-"SITTING"
  Combine_data$Activity[Combine_data$Activity==5] <-"STANDING"
  Combine_data$Activity[Combine_data$Activity==6] <-"LAYING"
  
  #give meaningful name
  names(Combine_data)<-gsub("^t", "time", names(Combine_data))
  names(Combine_data)<-gsub("^f", "frequency", names(Combine_data))
  names(Combine_data)<-gsub("Acc", "Accelerometer", names(Combine_data))
  names(Combine_data)<-gsub("Gyro", "Gyroscope", names(Combine_data))
  names(Combine_data)<-gsub("Mag", "Magnitude", names(Combine_data))
  names(Combine_data)<-gsub("BodyBody", "Body", names(Combine_data))

  #produce the second set of data
  library(plyr);
  Data2<-aggregate(. ~Subject + Activity, Combine_data, mean)
  Data2<-Data2[order(Data2$Subject,Data2$Activity),]
  write.table(Data2, file = "tidydata.txt",row.name=FALSE)
  #Write the cook book
  library(knitr)
  knit2html("codebook.rmd");

}