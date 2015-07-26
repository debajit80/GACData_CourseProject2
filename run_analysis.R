
## PART 1 

## Downloading the zip file to home directory

Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(Url,dest="./GACDataCProj.zip",method="curl")
    unzip("./GACDataCProj.zip")

## PART 2 

## Join the train and test datasets along with variable names, activity labels and subject 

library(dplyr)
    
## 2.1 Create a vector of column names for the 562 variables in the train and test datasets
  
  columns <- read.table("./UCI HAR Dataset/features.txt")
    columns1 <- t(columns)
      
## 2.2 Read the test data, add column names, activity labels and subject that performed the test 
    
    testdata <- read.table("./UCI HAR Dataset/test/X_test.txt")
      colnames(testdata) <- c(columns1[2,1:ncol(columns1)]) ## adds column names from 2.1
        testlabels <- read.table("./UCI HAR Dataset/test/y_test.txt")
          testsubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
            test <- cbind(testlabels,testsubject, testdata)
              names(test)[1] <- "Label" ## adding descriptive labels 
                names(test)[2] <- "Subject" ## adding descriptive labels
               
              
      
## 2.3 Read the train dataset, add column names, activity labels and subject that performed the test 
            
                traindata <- read.table("./UCI HAR Dataset/train/X_train.txt")
                  colnames(traindata) <- c(columns1[2,1:ncol(columns1)]) ## adds column names from 2.1
                    trainlabels <- read.table("./UCI HAR Dataset/train/y_train.txt")
                      trainsubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
                        train <- cbind(trainlabels,trainsubject, traindata)
                          names(train)[1] <- "Label" ## adding descriptive labels 
                            names(train)[2] <- "Subject" ## adding descriptive labels 
                        
                
## 2.4 Join the test data from 2.2 and train data from 2.3
                      
                                datamaster <- rbind(test,train)
                   

## PART 3
                                
## Extracts only the measurements on the mean and standard deviation for each measurement and adds descriptive activity names to name the activities in the data set
## Install and load the dplyr package before running this section
library(dplyr)
## 3.1 Subsets the data master created in 2.4 based on the column names containing mean and standard deviation while retaining the Label and Subject columns
                  
                  meansd <- datamaster[,grep("mean()|std()|Label|Subject", names(datamaster))]

## 3.2 Further subsets the dataset created in 3.1 to exclude columns containing Mean Frequencies which is a weighted average of the frequency components and not a straught arithmetic mean
                  
                      meansd <- meansd[,grep("Freq", names(meansd), invert=TRUE)] 

## 3.3 Reads the activity list table and changes the column names to match column names in the meansd and datamaster datasets

                        activitylist <- read.table ("./UCI HAR Dataset/activity_labels.txt")
                          names(activitylist)[1] <- "Label"
                            names(activitylist)[2] <- "Activity"
                          
## 3.4 Adds descriptive activity labels from activiylist to the meansd dataset to create the meansd_act dataset 
                            
                              meansd_act <- activitylist %>%
                                            right_join(meansd, by="Label")

## PART 4
## Label the data set with descriptive variable names wherever approrpiate
  
                              names(meansd_act) <- gsub("mean()","Mean", names(meansd_act))
                                names(meansd_act) <- gsub("std()","SD", names(meansd_act))
                                  names(meansd_act) <- gsub("mad()","Median Abs Dev", names(meansd_act))
                                      names(meansd_act) <- gsub("^t","Time", names(meansd_act))
                                          names(meansd_act) <- gsub("^f","Frequency", names(meansd_act))
                                            names(meansd_act) <- gsub("Acc","Accelerometer", names(meansd_act))
                                              names(meansd_act) <- gsub("Gyro","Gyrosocope", names(meansd_act))
                                                names(meansd_act) <- gsub("Mag","Magnitutde",names(meansd_act))
                                                  names(meansd_act) <- gsub("BodyBody","Body", names(meansd_act))
                              
## PART 5
## Creates a second, independent tidy data set with the average of each variable for each activity and each subject

## Output - TidyData.txt : dataframe with 11880 rows and 7 columns 
## Check against features of Tidy Data 
## 1 variable - 1 column : each column represents one variable (Activity (Name), Label (Activity), Subject, Activity Description, Stats (Type of statistics - Mean, SD etc.), Axes (X, Y or Z) and the value)
## Rows - each observation of the variable : each observation is assocuated with a type of activity performed by a subject resulting in a specific statistical measure

                                    library(tidyr)
                                     meansd_act %>%
                                          group_by(Activity,Label,Subject) %>%
                                                summarise_each(funs(mean)) %>%
                                                        arrange(Subject,Activity) %>%
                                                            gather("ActivityDescription-Stats-Axes","Value",4:ncol(meansd_act)) %>%
                                                                separate("ActivityDescription-Stats-Axes",c("ActivityDescription","Stats","Axes")) %>%
                                                                  write.table("./TidyData.txt",row.name=FALSE)
                                        
                                      
                                      
                                    