# Getting and Cleaning data course assignment
# code to read in  data and produce two tidy datasets

install.packages("dplyr")
library(dplyr)

# code assumes the current directory contains
# the data files subject_test.txt,x_test.txt,y_test.txt
# the data files subject_train.txt,x_train.txt,y_train.txt


#=========  produce a 1st tidy data set

activity <- c("Walking","Walking_upstairs",
              "Walking_downstairs","Sitting","Standing","Laying")

#====================read test data set
testsub <- read.table("subject_test.txt")
testxdat <- read.table("X_test.txt")
testydat <- read.table("y_test.txt")

#check for nan's 
sum(is.na(testxdat))

testxdat_mean <- c(1:nrow(testxdat))
testxdat_sdev <- c(1:nrow(testxdat))
test_set <- c(1:nrow(testxdat))
test_acti <-c(1:nrow(testxdat))

#calculate mean and sd columns for test data
for(i in 1:nrow(testxdat)) {  
  testxdat_mean[i] <- mean(as.numeric(testxdat[i,1:ncol(testxdat)],na.rm = TRUE))
  testxdat_sdev[i] <- sd(as.numeric(testxdat[i,1:ncol(testxdat)],na.rm = TRUE))
  test_set[i]<-"test_data"
  test_acti[i]<- activity[as.numeric(testydat[i,1])]
}

#bind data to obtain full test set data array
testindex <- c(1:nrow(testxdat))
testdat <- cbind(testindex,test_set,testsub,test_acti,testxdat_mean,testxdat_sdev)                     
                       
#check dims)
ncol(testdat)
nrow(testdat)
#relabel columns subject,activity,mean,sdev,
col_label <-c("index","data-set","subject","activity","Mean","Sdev")
dimnames(testdat)<-list((1:nrow(testxdat)),col_label)

#====================read training data set
trainsub <- read.table("subject_train.txt")
trainxdat <- read.table("X_train.txt")
trainydat <- read.table("y_train.txt")

#check for nan's 
sum(is.na(trainxdat))

trainxdat_mean <- c(1:nrow(trainxdat))
trainxdat_sdev <- c(1:nrow(trainxdat))
train_set <- c(1:nrow(trainxdat))
train_acti <-c(1:nrow(trainxdat))

#calculate mean and sd columns for train data
for(i in 1:nrow(trainxdat)) {  
  trainxdat_mean[i] <- mean(as.numeric(trainxdat[i,1:ncol(trainxdat)],na.rm = TRUE))
  trainxdat_sdev[i] <- sd(as.numeric(trainxdat[i,1:ncol(trainxdat)],na.rm = TRUE))
  train_set[i]<-"train_data"
  train_acti[i]<- activity[as.numeric(trainydat[i,1])]
}

#bind data to obtain full train set data array
trainindex <- c(1:nrow(trainxdat))
traindat <- cbind(trainindex,train_set,trainsub,train_acti,trainxdat_mean,trainxdat_sdev)                     

#check dims)
ncol(traindat)
nrow(traindat)

#relabel columns subject,activity,mean,sdev,
col_label <- c("index","data-set","subject","activity","Mean","Sdev")
dimnames(traindat)<-list((1:nrow(trainxdat)),col_label)

#=========#merge test and training sets
train_test <- merge(traindat,testdat,all=TRUE)
#head(train_testdat)

write.table(train_test, file = "train_testdat.txt",row.name=FALSE)  

#=========  produce a 2nd tidy data set
#=========simplified subject data set
subjectindex <- c(1:30)
subject_mean <- c(1:30)
subject_sdev <- c(1:30)

for(i in 1:30) { 
  subjectindex[i] <- paste("Subject",i)
  subject_mean[i] <- mean(train_test[train_test[,3] == i,5])
  subject_sdev[i] <- mean(train_test[train_test[,3] == i,6])
}
subjectdat <- cbind(subjectindex,subject_mean,subject_sdev)
col_label <- c("Catagory","Mean","Sdev")
dimnames(subjectdat)<-list((1:nrow(subjectdat)),col_label)

#=========simplified activity data set
activity_mean <- c(1:6)
activity_sdev <- c(1:6)
for(i in 1:6) { 
  
  activity_mean[i] <- mean(train_test[train_test[,4] == activity[i],5])
  activity_sdev[i] <- mean(train_test[train_test[,4] == activity[i],6])
}
activitydat <- cbind(activity,activity_mean,activity_sdev)
col_label <- c("Catagory","Mean","Sdev")
dimnames(activitydat)<-list((1:nrow(activitydat)),col_label)

#=========#merge test and training sets
sub_act_dat <- merge(subjectdat,activitydat,all=TRUE)
#head(sub_act_dat)
write.table(sub_act_dat, file = "acti_subject.txt",row.name=FALSE)  

