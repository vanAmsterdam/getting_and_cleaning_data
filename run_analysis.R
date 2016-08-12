# Initialization ----------------------------------------------------------
rm(list = ls()) # clear environment
library(data.table) # I like data.tables

# function to read in all datasets within a directory
readinDir <- function(dir, recursive = T) {
  require(data.table)
  fileNames <- list.files(dir, recursive = recursive)
  fileNames <- grep(".txt", fileNames, value = T)
  datasetNames <- character()
  lapply(fileNames, function(fileName) {
    datasetName <- gsub(".txt", '', fileName)
    if (recursive) {
      datasetName <- tail(strsplit(datasetName, split = "/")[[1]], 1) # splits subdirectories, last item is file name
    }
    datasetNames <<- append(datasetNames, datasetName)
    eval(parse(text =
                 paste0(datasetName, " <<- fread(\"",dir,fileName,"\")")
    ))
  })
  setListName <- paste0(tail(strsplit(dir, split = '/')[[1]], 1), "_datasets")
  assign(setListName, datasetNames, envir = .GlobalEnv)
  return()
}

# Download and read in data -----------------------------------------------
# url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download.file(url, destfile = "data/project_data.zip")
## assuming there is a 'data' directory in your working directory which contains the needed data
activity_labels <- fread("data/UCI HAR Dataset/activity_labels.txt")
features <- fread("data/UCI HAR Dataset/features.txt")
readinDir('data/UCI HAR Dataset/train/', recursive = F)
readinDir('data/UCI HAR Dataset/test/', recursive = F)
features <- features[[2]]

# rbind data
df <- rbind(X_train, X_test)
colnames(df) <- features
## add factor variable for partition
df[, partition:=factor(c(rep('train', nrow(X_train)), rep('test', nrow(X_test))))]
table(df$partition) # check that the labeling of the partitioning worked out
# add 'outcome' data (labels) with their nice names
df[partition == 'train', activity:=factor(y_train$V1, levels = activity_labels$V1, labels = activity_labels$V2)]
df[partition == 'test', activity:=factor(y_test$V1, levels = activity_labels$V1, labels = activity_labels$V2)]
table(df$activity) # check activity labels
# add subject identifier
subject <- rbind(subject_train, subject_test)
df[, subject:=subject]

# select features with mean or SD
selFeatures <- grep("mean|std", features, value = T)
df <- df[, c(selFeatures, "subject", "partition", "activity"), with = F]

# Create second tidy dataset ----------------------------------------------
df2 <- df[, lapply(.SD, mean), by = "activity,subject", .SDcols = selFeatures]
length(unique(df$activity)) * length(unique(df$subject)) # check that the dimension is correct
