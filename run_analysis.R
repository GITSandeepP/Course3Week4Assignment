
# Load "dplyr" package which will be used for data cleaning
library(dplyr)

        # load features list files. 
        # This file contains the column names for the data in test and train files
        setwd("/Users/mycomputer/Documents/UCI HAR Dataset")
        featurefile <- "features.txt"
        con <- file(featurefile, "r")
        open(con)
        feature <- read.table(con, header = FALSE)
        close(con)

        # Convert the data frame into a character vector, which will be then 
        # assigned to the columns name of test and train data
        feature.list <- as.list(feature$V2)
        feature.list <- sapply(feature.list, paste0, collapse="")

        # load x_test.txt file        
        setwd("/Users/mycomputer/Documents/UCI HAR Dataset/test")
        filename <- "X_test.txt"
        con <- file(filename, "r")
        open(con)
        # data from the file is read into x.test variable
        x.test <- read.table(con, header = FALSE)
        close(con)

        # assign column names to the data frame
        names(x.test) <- as.vector(feature.list)

        # load y_test.txt file. This file contains activities id for each observation in x.test file
        filename <- "y_test.txt"
        con <- file(filename, "r")
        open(con)
        # data from the file is read into y.test variable
        y.test <- read.table(con, header = FALSE)
        close(con)

        # assign column name to the data frame
        names(y.test) <- c("activity_id")

        # load subject_test.txt file. This file contains subject number for each observation in x.test file
        filename <- "subject_test.txt"
        con <- file(filename, "r")
        open(con)
        # data from the file is read into subject.test variable
        subject.test <- read.table(con, header = FALSE)
        close(con)

        # assign column name to the data frame
        names(subject.test) <- c("subject")

        # test.df data frame is formed using cbind function 
        # it is formed using y.test, subject.test & x.test data frames
        test.df <- cbind(y.test, subject.test, x.test)
        
        # load x_train.txt file        
        setwd("/Users/mycomputer/Documents/UCI HAR Dataset/train")
        train.filename <- "X_train.txt"
        con <- file(train.filename, "r")
        open(con)
        # data from the file is read into x.train variable
        x.train <- read.table(con, header = FALSE)
        close(con)
        
        # assign column names to the data frame
        names(x.train) <- as.vector(feature.list)
        
        # load y_train.txt file. This file contains activities id for each observation in x.train file
        filename <- "y_train.txt"
        con <- file(filename, "r")
        open(con)
        # data from the file is read into y.train variable
        y.train <- read.table(con, header = FALSE)
        close(con)
        
        # assign column name to the data frame
        names(y.train) <- c("activity_id")
        
        # load subject_train.txt file. This file contains subject number for each observation in x.train file
        filename <- "subject_train.txt"
        con <- file(filename, "r")
        open(con)
        # data from the file is read into subject.train variable
        subject.train <- read.table(con, header = FALSE)
        close(con)
        
        # assign column name to the data frame
        names(subject.train) <- c("subject")
        
        # train.df data frame is formed using cbind function 
        # it is formed using y.train, subject.train & x.train data frames
        train.df <- cbind(y.train, subject.train, x.train)
        
        # Question 1 - Merge the training and the test sets to create one data set
        # merge.df is formed using test.df (test data) and train.df (training data)
        merge.df <- rbind(test.df, train.df) 
        print(str(merge.df))
        print(dim(test.df))
        print(dim(train.df))
        
        # Question 2 - Extracts only the measurements on the mean and standard deviation for each measurement
        # mean.std is formed using merge.df. it contains all the observations for columns containing mean/std
        # Please note that column name containing meanFreq is not included here.
        mead.std <- merge.df[grepl("mean[()]", names(merge.df)) | grepl("std[()]", names(merge.df))]
        print(str(mead.std))
        
        # Question 3 - Uses descriptive activity names to name the activities in the data set
        
        # load activity_labels.txt file. 
        # This file contains activities name against which the observations are recorded.                
        setwd("/Users/mycomputer/Documents/UCI HAR Dataset")
        activities.filename <- "activity_labels.txt"
        con <- file(activities.filename, "r")
        open(con)
        # data from the file is read into activity.label data frame
        activity.label <- read.table(con, header = FALSE)
        close(con)
        
        # assign column names to the data frame
        names(activity.label) <- c("activity_id", "activity_name")
        
        # merge.df and activity.label are joined based on activity_id
        # Please note that inner join is applied here hence all the non matching values 
        # i.e. NAs will not be included here
        join.df <- merge(activity.label, merge.df, by = "activity_id")
        # activity_id column is removed from join.df as we now have activity_name information
        join.df <- select(join.df, -activity_id)
        print(str(join.df))
        
        # Question 4 - Appropriately labels the data set with descriptive variable names
        # parenthesis are removed from the column name
        # abbreviated values are replaced with their full name
        names.df <- names(join.df)
        names.df <- gsub("[()]", "", names.df)
        names.df <- gsub("^t", "Time_", names.df)
        names.df <- gsub("^f", "Frequency_", names.df)
        names.df <- gsub("Acc", "_Accelerometer", names.df)
        names.df <- gsub("Gyro", "_Gyroscope", names.df)
        names.df <- gsub("Mag", "_Magnitude", names.df)
        names.df <- gsub("-MeanFreq-", "_MeanFrequency_", names.df)
        names.df <- gsub("-std-", "_StandardDeviation_", names.df)
        names.df <- gsub("-std", "_StandardDeviation", names.df)
        names.df <- gsub("-maxInds-", "_Max_Index_", names.df)
        names.df <- gsub("-maxInds", "_Max_Index", names.df)
        names.df <- gsub("-iqr-", "_Interquartile_Range_", names.df)
        names.df <- gsub("-iqr", "_Interquartile_Range", names.df)
        names.df <- gsub("-sma-", "_Signal_Magnitude_Area_", names.df)
        names.df <- gsub("-sma", "_Signal_Magnitude_Area", names.df)
        names.df <- gsub("-", "_", names.df)
        names(join.df) <- names.df
        print(names(join.df))
        
        # Question 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
        # join.df which is the final data frame is used to produce independent tidy data frame
        # which is then written in a file named data_tidy.txt
        tidy.df <- join.df %>% group_by(activity_name = join.df$activity_name, subject = join.df$subject) %>% summarise_all(funs(mean))
        write.table(x = tidy.df, file = "data_tidy.txt", row.names = FALSE)
        print(str(tidy.df))