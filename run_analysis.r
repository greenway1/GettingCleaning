# Instructions----
#SUBMIT
# 1) a tidy data set as described below, 
# 2) a link to a Github repository with your script for performing the analysis, and 
# 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md
# You should create one R script called run_analysis.R that does the following: 
#    1. Merges the training and the test sets to create one data set.
#    2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#    3. Uses descriptive activity names to name the activities in the data set
#    4. Appropriately labels the data set with descriptive variable names. 
#
#    5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#---------------------------------------------------------------------------

##GRADING
#Has the student submitted a tidy data set? Either a wide or a long form of the data is acceptable if it meets the tidy data principles of week 1 (
#Each variable you measure should be in one column, Each different observation of that variable should be in a different row).
#
#Was code book submitted to GitHub that modifies and updates the codebooks available to you with the data to indicate all the variables and summaries you calculated, along with units, and any other relevant information?
#
#I was able to follow the README in the directory that explained what the analysis files did. 
#---------------------------------------------------------------------------

# STEPS----
# I. READ IN DATA & ADD HEADERS
#  1) Read in features.txt -- labels for the test and train columns
#       -- 2 column; Numeric values (1-561) as row # and TEXT values
#  2) Read in activity_labels.txt -- classifications/reference for the activity type
#       -- 2 columns; Numeric (1-6), Text values
#  FOR BOTH TEST & TRAIN FOLDERS
#  3) Read in subject_test/train.txt -- Subject ID or reference for which subject the rows in test data refer to
#       -- 1 column; Numeric (1-30) values
#  4) Read in X_test/train.txt -- Actual data captured for the test and training sets.  This combined set represents the average of the values of the individual
#     files in the "Inertial Signals" folders
#       -- 561 columns; all Numeric values
#  5) Read in y_test/train.txt -- Activity ID referring to the activity type for each row
#       -- 1 column; Numeric values (1-6)
#
# HEADERS
#  6) x_test/train.txt
#       -- use the features.txt as headers; do not change the order (row 1 in features = first column in x_test)
#  7) activity_labels.txt
#       a) Activity_ID
#       b) Activity_Name
#  8) subject_test/train.txt
#       a) Subject_ID
#  9) y_test/train.txt
#       a) Activity_ID
#
# II. REMOVE COLUMNS
#  10) x_test/train.txt - keep only the following columns
#       a) Subject_ID
#       b) Activity_ID
#       c) All columns with 'mean' in the name
#       d) All columns with 'std' in the name
#
# III. MERGE/COMBINE DATA
#  11) TEST: Combine X_test.txt with y_test.txt and subject_test.txt
#       -- DO NOT SORT!  Combine exactly as is since the y_test and subject_test don't have row identifiers.
#       -- Combine Row 1 = Row 1 and Row 2947 = Row 2947 for each file
#       -- Result will be a test data set with the Subject ID, Activity ID, and actual data
#  12) TRAIN: Combine X_train.txt with y_train.txt and subject_train.txt
#       -- DO NOT SORT!  Combine exactly as is since the y_train and subject_train don't have row identifiers.
#       -- Combine Row 1 = Row 1 and Row 2947 = Row 2947 for each file
#       -- Result will be the training data set with the Subject ID, Activity ID, and actual data
#  13) FULL: Combine TEST and TRAIN
#       -- Should have the same number and names of the fields. Use the equivalent of a UNION SQL query
#  14) Merge FULL with activity_labels
#       -- Merge on Activity_ID from both data frames
#
# IV. AVERAGE DATA SET
#  15) Create new data set taking average of each variable except activity and subject which you group on.  
#       -- SQL equivalent - SELECT activity_ID, activity_Name, Subject_ID, AVG(var1), AVG(var2)... FROM tbl_combined GROUP BY 1,2,3
# DONE/END------------------------------------------------------------------



#CODE----
# I. READ IN DATA & ADD HEADERS
setwd("~/R/GetCleaning/UCI HAR Dataset") #Set directory to correct location
features <- read.table("./features.txt", sep = " ", header = FALSE, col.names = c("featureID","featureNAME")) #step 1-read features
refActivity <- read.table("./activity_labels.txt", sep = " ", header = FALSE, col.names = c("activityID", "activityName")) #step 2-read activity_labels.txt

setwd("./train") #Go to train folder
trainSubject <- read.table("./subject_train.txt", header = FALSE, col.names = c("subjectID")) #read train subject ID
trainData <- read.table("./x_train.txt", header = FALSE, col.names = features[[2]]) #read in train data
trainActivity <- read.table("./y_train.txt", header = FALSE, col.names = c("activityID")) #read train activity ID

setwd("../test") #Go to test folder
testSubject <- read.table("./subject_test.txt", header = FALSE, col.names = c("subjectID")) #read test subject ID
testData <- read.table("./x_test.txt", header = FALSE, col.names = features[[2]]) #read in test data
testActivity <- read.table("./y_test.txt", header = FALSE, col.names = c("activityID")) #read test activity ID


# II. REMOVE COLUMNS
trainDataMeanStd <- trainData[, c(grep("mean", names(trainData), ignore.case=TRUE),grep("std", names(trainData), ignore.case=TRUE))] #Create new training data set with just mean & std columns
testDataMeanStd <- testData[, c(grep("mean", names(testData), ignore.case=TRUE),grep("std", names(testData), ignore.case=TRUE))] #Create new test data set with just mean & std columns


# III. MERGE/COMBINE DATA
trainDataFinal <- cbind(trainSubject, trainActivity, trainDataMeanStd) #add in the subject ID and activity ID for the training data set
testDataFinal <- cbind(testSubject, testActivity, testDataMeanStd) #add in the subject ID and activity ID for the test data set

combinedData <- rbind(trainDataFinal, testDataFinal) #combine the test and training data sets into one
finalData <- merge(refActivity, combinedData, by.x="activityID", by.y="activityID", all = TRUE) #Merge combinedData with the Activity type


# IV. AVERAGE DATA SET
install.packages("reshape2")
library(reshape2)

finalDataLong <- melt(finalData, id=1:3, measure.vars=4:89) #melting the data set from wide to long
averageData <- dcast(finalDataLong, subjectID +activityName~variable, mean)

setwd("../")
write.table(averageData, file = "CleanedAvgData.txt", row.name = FALSE)
