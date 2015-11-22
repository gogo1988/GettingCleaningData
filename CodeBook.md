#CodeBook for Getting and Cleaning Data Project 

## Data Source:

Data was obtained from the following website:

[http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
    
## Data for this project was downloaded using the following hyperlink:

[https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

The donwloaded file is uncompressed into the Working Directory and named as "unUCI HAR Dataset"
        
## Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.
        
       
## Implementation of the "run_analysis.R" function
        
###Step 1. Merges the training and the test sets to create one data set.

####Variables defined and their purpose are given below:

# 1. Merges the training and the test sets to create one data set.
        
        # load the featrure data for test and train
        
        dataTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
        dataTrain <-
                read.table("./UCI HAR Dataset/train/X_train.txt")
        
        # load the activities for test and train
        dataCombined <- rbind(dataTest,dataTrain)
        
        labelTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
        labelTrain <-
                read.table("./UCI HAR Dataset/train/y_train.txt")
        
        labelCombined <- rbind(labelTest,labelTrain)
        
        # load the subject IDs for test and train
        subjectTest <-
                read.table("./UCI HAR Dataset/test/subject_test.txt")
        subjectTrain <-
                read.table("./UCI HAR Dataset/train/subject_train.txt")
        
        subjectCombined <- rbind(subjectTest, subjectTrain)

###Step 2   :Extracts only the measurements on the mean and standard deviation for each measurement. 
 
        # load the feature data
        
        featureList <-
                read.table("./UCI HAR Dataset/features.txt", col.names = c("index", "featureLables"))
        
        # create a list of indices for the rows that contain "mean()" or "std()"
        featureMeanStdSubset <-
                grep("mean\\(\\)|std\\(\\)", featureList$featureLables)
        
        # create a new list of features that contains "mean()" or "std()"
        featureNewList <-
                as.character(featureList$featureLables[featureMeanStdSubset])
        
        colnames(dataCombined) <- featureList$featureLables
        
        dataCombined <- dataCombined[,featureMeanStdSubset]

###Step 3   : Uses descriptive activity names to name the activities in the data set

        #
        labelActivity <-
                read.table(
                        "./UCI HAR Dataset/activity_labels.txt", sep = " ", col.names = c("activityLabel","activity")
                )
        
        colnames(labelCombined) <- "activityLabel"
        
        labelAcitivityAll <-
                join(labelActivity,labelCombined,by = "activityLabel",type = "left")
        labelAcitivityAll$activityLabel <- NULL



###Step 4   :Appropriately labels the data set with descriptive activity names. 

        # assign a column name to the subject variable
        
        colnames(subjectCombined) <- "subject"
        
        # combine subject, acitivity, feature data into one data frame.
        
        allCombined <- cbind(subjectCombined,labelAcitivityAll,dataCombined)
        
        # output the merged dataset file
        
        write.table(allCombined, "mergedData.txt")


The file mergedData.txt is produced

###Step 5  :Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

        # melt data frame for reshaping (using the melt function in the reshape2 package)
        
        meltedAllCombined <-
                melt(
                        allCombined, id = c("subject", "activity"), measure.vars = featureNewList
                )
        
        # reshape into a tidy data frame
        
        meltedAllCombined <-
                dcast(meltedAllCombined, activity + subject ~ variable, mean)
        
        # reorder the data by subject then activity
        
        tidyIndex <-
                order(meltedAllCombined$subject, meltedAllCombined$activity)
        meltedAllCombined <- meltedAllCombined[tidyIndex,]
        
        # rearrange the the first two columns: column 1 is the subject, column 2 is the activity
        
        rownames(meltedAllCombined) <-
                seq_along(rownames(meltedAllCombined))
        meltedAllCombined <-
                meltedAllCombined[,c(2,1,3:dim(meltedAllCombined)[[2]])]
        
        # output the tidy dataset file
        write.table(meltedAllCombined,file = "tidyDataset.txt", row.name = FALSE)
        
        # This "tidyDataset.txt" file is available on the GitHub repository 

The file tidyDataset.txt is produced

## Summary of the output files :

UCI HAR Dataset
In the working directory there are two outputed files:

1. mergedData.txt
2. tidyDataset.txt
        
