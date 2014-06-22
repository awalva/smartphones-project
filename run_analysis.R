# You should create one R script called run_analysis.R that does the 
# following. 
#   1. Merges the training and the test sets to create one data set.
#   2. Extracts only the measurements on the mean and standard deviation 
#      for each measurement. 
#   3. Uses descriptive activity names to name the activities in the data 
#      set
#   4. Appropriately labels the data set with descriptive variable names. 
#   5. Creates a second, independent tidy data set with the average of each 
#      variable for each activity and each subject. 


####################### helpful comment from forums ############
# you want a run_analysis R script, a ReadMe markdown document, a Codebook 
# markdown document, and a tidy data text file (this last goes on Coursera)
################################################################


# Read in features and activity information
activity <- read.table("activity_labels.txt", stringsAsFactors=F)
features <- read.table("features.txt", stringsAsFactors=F)



# Read in test/train data, labels, and subjects
test <- read.table("./test/X_test.txt", stringsAsFactors=F)
test_lab <- read.table("./test/y_test.txt", stringsAsFactors=F)
subject_test<- read.table("./test/subject_test.txt")

train <- read.table("./train/X_train.txt", stringsAsFactors=F)
train_lab <- read.table("./train/y_train.txt", stringsAsFactors=F)
subject_train<- read.table("./train/subject_train.txt")


# Create a character vector of appropriate column names (acceptable to R)
# based on features
vars<-make.names(features$V2)


# Combine test and train data sets, and rename columns using char vector
test_train <- rbind(test, train)
names(test_train) <- vars


# Combine labels (for activity) and subjects for test and train, then 
# append to the data set. `V1` = label, `V1.1` = subject in combined set.
label <- rbind(test_lab, train_lab)
subject <- rbind(subject_test, subject_train)
test_train_lab <- cbind(test_train, label, subject)
colnames(test_train_lab)[563]<-"subject"


# merge with activities.  This will reorder the data.
test_train_act <- merge(test_train_lab, activity)


# subset combined data set to include only mean and std, 
# then re-append subject and activity labels
columns <- grep("[Mm]ean|std", names(test_train_act))
test_train_act_sub <- test_train_act[,columns]
test_train_act_sub$subject <- test_train_act$subject
test_train_act_sub$activity <- test_train_act$V2                                  

test_train_act_sub <- test_train_act_sub[,c(87,88,1:86)]


# next we must create tidy data for step 5. Use melt, ddply, and dcast.
molten<-melt(test_train_act_sub, id.vars=c("subject","activity"))
d <- ddply(mx, .(subject, activity, variable), summarize, mean=mean(value))
tidy5<- dcast(d, subject + activity ~ variable, value.var="mean")


# write tidy data set to a txt file called `tidy.txt`
write.table(tidy5, file = "tidy.txt")
