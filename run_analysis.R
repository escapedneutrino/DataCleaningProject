# Data Cleaning Week 4 Project
# 10/22/2016

# This code will produce two tidy data frames in a linux environment
# df contains a subset of the combined test and training data with 
# mean and std quantities from the original data set.
# df2 has one row per subject per activity and captures  
# the means of the measurement columns from df with corresponding
# test subject and activty.

# Note: this code uses = instead of <- for assignment for readability 
# because [Thing] less than negative [Other thing] is confusing

# Include useful library
library(dplyr)

# Step 0 - get the data, if necessary:
# Comment out 3 lines below if data has been retrieved already
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              "./thedata.zip")
unzip("./thedata.zip")


# Step 1:
# Merging training and test data
# I am ignoring the raw accelerometer and gyro data and only using
# the X, y, subject files.

# Note: this could also be done on a bash command line with something like: 
# $ paste subject_test.txt, X_test.txt, y_test.txt >temp1
# $ paste subject_train.txt, X_train.txt, y_train.txt >temp2
# $ cat temp1, temp2 >fulldata.txt

# Within each set (test and train), combine subject, data, and label:
df_train = read.table("./UCI HAR Dataset/train/X_train.txt")
df_train_labs = read.table("./UCI HAR Dataset/train/y_train.txt")
df_train_subs = read.table("./UCI HAR Dataset/train/subject_train.txt")

df_test = read.table("./UCI HAR Dataset/test/X_test.txt")
df_test_labs = read.table("./UCI HAR Dataset/test/y_test.txt")
df_test_subs = read.table("./UCI HAR Dataset/test/subject_test.txt")

# Combine data, labels, and subjects into training and test tables:
df_train = cbind(df_train, df_train_labs, df_train_subs) 
df_test = cbind(df_test, df_test_labs, df_test_subs)
# Cols are now: [561 features], label (i.e. activity), subject (i.e. #1-30)
mycols = 562:563
mycolnames = c("activity","subject")

# Combine test and train sets into one dataframe
df = rbind(df_train, df_test)

# Clean up temporary data frames
rm(df_train, df_train_labs, df_train_subs, df_test, df_test_subs, df_test_labs)

# Step 2:
# Extract column names from the data's own feature list file and 
# pull out the columns with "mean" and "std" in their names
# Use features.txt to get columns with means and stds
features = read.table("./UCI HAR Dataset/features.txt", col.names = c("col","name"))

# col is equivalent to the row number in features, grep will return desired column #s
meanstdcols = grep("[Mm][Ee][Aa][Nn]|[Ss][Tt][Dd]", features$name)
meanstdcolnames = features$name[meanstdcols]

allcols = c(meanstdcols, mycols)
allcolnames_unclean = c(as.character(meanstdcolnames), mycolnames)
# Note: the regex above gives several columns in addition to the plain mean() ones
# (and that have no corresponding std()).  I'm leaving them for now because they look
# like potentially useful quantities.

# Clean up temporary objects
rm(features, meanstdcols, meanstdcolnames, mycols, mycolnames)

# Trim df down to desired columns and apply descriptive column labels 
# as taken from the data's documentation:
df = df[,c(allcols)]
colnames(df) = allcolnames_unclean

# Step 3
# Replace activity number labels with their descriptions:
activitylabs = read.table("./UCI HAR Dataset/activity_labels.txt", 
                          col.names = c("label","activity_desc"))
# Get rid of "_" for tidy-ness
activitylabs$activity_desc = gsub("_", "", tolower(activitylabs$activity_desc))
# Make factor for convenience later
df$activity = factor(df$activity,labels = activitylabs$activity_desc)

# Clean up temporary objects
rm(activitylabs, allcolnames_unclean, allcols)

# Step 4
# Tidy up the remaining variable (column) names by removing non-alphanumeric
# characters and converting to lowercase
colnames(df) = gsub("\\(\\)|-","", tolower(colnames(df)))

# Generate second data set of average values by subject and activity
df2 = df %>% group_by(subject,activity) %>%
  summarise_all(funs(mean))
