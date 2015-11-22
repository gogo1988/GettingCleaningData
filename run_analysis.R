run_analysis <- function() {
        # check if "plyr" or "reshape" package are installed; if not, install the two packages.
        have.pkg <- c("plyr","reshape2") %in% rownames(installed.packages())
        if (any(!have.pkg)) {
                        need <- pkgs[!have.pkg]
                        install.packages(need)
        }
        
        library(plyr)
        library(reshape2)
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
        
        
        
        
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
        
        
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
        
        
        
        
## 3. Uses descriptive activity names to name the activities in the data set
        
        #
        labelActivity <-
                read.table(
                        "./UCI HAR Dataset/activity_labels.txt", sep = " ", col.names = c("activityLabel","activity")
                )
        
        colnames(labelCombined) <- "activityLabel"
        
        labelAcitivityAll <-
                join(labelActivity,labelCombined,by = "activityLabel",type = "left")
        labelAcitivityAll$activityLabel <- NULL
        
        
## 4. Appropriately labels the data set with descriptive variable names.
        

        # assign a column name to the subject variable
        
        colnames(subjectCombined) <- "subject"
        
        # combine subject, acitivity, feature data into one data frame.
        
        allCombined <- cbind(subjectCombined,labelAcitivityAll,dataCombined)
        
        # output the merged dataset file
        
        write.table(allCombined, "mergedData.txt")
        
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        
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
}