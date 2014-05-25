# load the dataset 
X_train <- read.table("./data/UCIHARDataset/train/X_train.txt",header=FALSE)
Y_train <- read.table("./data/UCIHARDataset/train/y_train.txt",header=FALSE)
SUB_train <- read.table("./data/UCIHARDataset/train/subject_train.txt",header=FALSE)
X_test <- read.table("./data/UCIHARDataset/test/X_test.txt",header=FALSE)
Y_test <- read.table("./data/UCIHARDataset/test/y_test.txt",header=FALSE)
SUB_test <- read.table("./data/UCIHARDataset/test/subject_test.txt",header=FALSE)

# -----------------------------------------------------------------------------
#1. Merges the training and the test sets to create one data set. 
# -----------------------------------------------------------------------------

train <- cbind(X_train,Y_train,SUB_train)
test <- cbind(X_test,Y_test,SUB_test)
allData <- rbind(train,test)

# get the column names from features.txt
dataColname <- read.table("./data//UCIHARDataset/features.txt", header=FALSE,stringsAsFactors = FALSE)
cols <- dataColname[,2]
othCols <- c("activity_lables","subject")
final_cols <- c(cols,othCols)

# Assign the names to the dataframe:
colnames(allData) <- final_cols

# -----------------------------------------------------------------------------
#2. Extracts only the measurements on the mean and standard deviation for each 
#   measurement. 
# -----------------------------------------------------------------------------
alldata_subset = allData [,grep("mean\\(\\)|std\\(\\)|activity_lables|subject", final_cols)]

# -----------------------------------------------------------------------------
#3. Uses descriptive activity names to name the activities in the data set
# -----------------------------------------------------------------------------
activity_labels = read.table("./data/UCIHARDataset/activity_labels.txt", stringsAsFactors=FALSE)
activityColName = c ("activity","activity_desc")

colnames(activity_labels) <- activityColName

vector_activity_desc = sapply(alldata_subset$activity_lables, function (activity) {
  activity_labels[activity_labels$activity==activity,2]
})

alldata_subset_with_desc = cbind(alldata_subset, vector_activity_desc)


# -----------------------------------------------------------------------------
#4. Appropriately labels the data set with descriptive activity names. 
# -----------------------------------------------------------------------------
colnames (alldata_subset_with_desc) <- c(final_cols[grep("mean\\(\\)|std\\(\\)|activity_lables|subject", final_cols)], "activity_desc")


#5. Creates a second, independent tidy data set with the average of each 
#   variable for each activity and each subject. 
# -----------------------------------------------------------------------------
require(plyr)
alldata_with_mean <- ddply(allData,.(subject, activity_lables), colMeans)

# Also Add ACtivity Lables: 
activity_desc = sapply(alldata_with_mean$activity_lables, function (activity) {
  activity_labels[activity_labels$activity==activity,2]
})

alldata_with_mean_and_desc  = cbind(alldata_with_mean, activity_desc)

# Writing tidy data in a file
write.table(alldata_with_mean_and_desc, "./data/UCIHARDataset/tidy_data.txt", row.names=FALSE, quote = FALSE,sep="\t")

