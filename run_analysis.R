#Course Project - Getting and Cleaning Data

#"UCI HAR Dataset" must be saved in the working directory.

# 1. Merges the training and the test sets to create one data set.

        #Library the dplyr package.
        library(dplyr)

        #Load the files: "X_test.txt" and "X_train.txt".
        my_local <- getwd()        
        path_file<- paste0(my_local, "/","UCI HAR Dataset", "/",
                           "test","/","X_test.txt")
        data_test <- read.table(path_file)
        path_file<- paste0(my_local, "/","UCI HAR Dataset", "/",
                           "train","/","X_train.txt")
        data_train <- read.table(path_file)
        
        #Use the file "features.txt" to put the names in the tables.
        path_file<- paste0(my_local, "/","UCI HAR Dataset", "/","features.txt")
        names <- read.table(path_file)
        names(data_test) <- names[,2]
        names(data_train) <- names[,2]
        remove(names)
        
        #Load "subject_train" and "y_train".
        path_file<- paste0(my_local, "/","UCI HAR Dataset", "/",
                           "train","/","y_train.txt")
        num_activity_train <- read.table(path_file)
        path_file<- paste0(my_local, "/","UCI HAR Dataset", "/",
                           "train","/","subject_train.txt")
        subject_train <- read.table(path_file)
        
        #Load "subject_test" and "y_test".
        path_file<- paste0(my_local, "/","UCI HAR Dataset", "/",
                           "test","/","y_test.txt")
        num_activity_test <- read.table(path_file)
        path_file<- paste0(my_local, "/","UCI HAR Dataset", "/",
                           "test","/","subject_test.txt")
        subject_test <- read.table(path_file)
        
        #Add num_activity and subject to data_test and data_train.
        data_test$subject <- as.matrix(subject_test)
        data_test$num_activity <- as.matrix(num_activity_test)
        data_train$subject <- as.matrix(subject_train)
        data_train$num_activity <- as.matrix(num_activity_train)
        remove(num_activity_test,subject_test,num_activity_train,subject_train)
        
        #Merge the data frames using rbind.
        mydata <- rbind(data_test,data_train)
        remove(data_test,data_train)
        
        #Change num_activity and subject columns to numeric.
        mydata$subject <- as.numeric(mydata$subject)
        mydata$num_activity <- as.numeric(mydata$num_activity)
        
# 2. Extracts only the measurements on the mean and standard deviation 
# for each measurement. 
        
        #Get the vector of columns with mean, std, subject and
        #num_activity.
        vector_names <- names(mydata)
        mean_columns <- grep("mean", vector_names, ignore.case = TRUE)
        std_columns <- grep("std", vector_names, ignore.case = TRUE)
        mean_and_std <- c(mean_columns,std_columns)
        vector_columns <- c(mean_and_std, 562, 563)
        vector_columns <- sort(vector_columns)
        remove(std_columns,mean_columns, mean_and_std, vector_names)
        
        #Subset mydata with vector_columns.
        mydata <- mydata[,vector_columns]
        remove(vector_columns)
        
# 3. Uses descriptive activity names to name the activities in the data set.
 
        #Get the activity labels.
        path_file<- paste0(my_local, "/","UCI HAR Dataset", "/","activity_labels.txt")
        activity_labels <- read.table(path_file)
        names(activity_labels) <- c("num_activity", "activity")
        
        #Create an ID and merging mydata with activity_labels.
        ID <- as.numeric(row.names(mydata))
        mydata$ID <- ID
        mydata <- merge(mydata, activity_labels, by = "num_activity")
        remove(activity_labels, ID)
        
        #Order mydata by ID to get same order before merging.
        mydata <- mydata[order(mydata$ID),]
        
        #Remove num_activity and ID columns.
        mydata$num_activity <- NULL
        mydata$ID <- NULL

# 4. Appropriately labels the data set with descriptive variable names.
        #Done in the previous steps.

# 5. From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each subject and each activity.
        
        #Transform column subject as factor.
        mydata$subject <- as.factor(mydata$subject)
        
        #Group by subject and activity. 
        by_subject <- group_by(mydata, subject, activity)
        
        #Summarise each variable by mean.
        tidy_data <- by_subject %>% summarise_each(funs(mean))
        
        #Remove unnecessary objects.
        remove(by_subject,mydata,my_local,path_file)
        
        #Save the tidy data in the working space.
        write.table(tidy_data, file = "tidy_data.txt", row.name=FALSE)

#END      
        
