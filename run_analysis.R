#Install required packages
install.packages("dplyr")
install.packages("reshape2")

## Load packages
library("dplyr")
library("reshape2")


data_path <- "UCI HAR Dataset"

## Reads a file
## Params:
##  Param 1 : filename .. name of the file to load
##  Param 2: .. a comma separated dir list
read_file <- function(filename, ...) {
  file.path(..., filename) %>%
  read.table(header = FALSE)
}

## Reads a training file
read_train_file <- function(filename) {
  read_file(filename, data_path, "train")
}

## Reads a test file
read_test_file <- function(filename) {
  read_file(filename, data_path, "test")
}

## Makes use of activity list values to describe test/training labels
## Param 1: dataset : label dataset
## Return : the original dataset with human readable column name and values
describe_lbl_ds <- function(dataset) {
  names(dataset) <- col_name_activity  
  dataset$Activity <- factor(dataset$Activity, levels = activity_label$V1, labels = activity_label$V2)
  dataset
}

## The dataset with results of feature tests & maps columns with individual features
## Param 1 : ds : activity dataset
## Return : the original dataset with columns with the feature it describes
desc_actvity_ds <- function(ds) {
  col_names <- gsub("-", "_", features$V2)
  col_names <- gsub("[^a-zA-Z\\d_]", "", col_names)
  names(ds) <- make.names(names = col_names, unique = TRUE, allow_ = TRUE)
  ds
}

## Adjusts column name in the data set identifying test participants
desc_subject_ds <- function(ds) {
  names(ds) <- col_name_subject
  ds
}

## Download and extract a zip file with datasets if it doesn't exist
if (!file.exists(data_path)) {
  dataFileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  dest_file <- data_zip_file_name
  download.file(dataFileURL, destfile = dest_file, method = "curl")
  unzip(dest_file)
  if (!file.exists(data_path)) 
    stop("Error reading the expected file structure")
}


## Custom columns
col_name_subject <- "Subject"
col_name_activity <- "Activity"

## Load features as human-readable column names
features <- load_file("features.txt", data_path)

## Load activity labels
activity_label <- load_file("activity_labels.txt", data_path)

## Use descriptive activity names to name the activities in the data set
#### Training data
train_set <- load_train_file("X_train.txt") %>% desc_actvity_ds
train_labels <- load_train_file("y_train.txt") %>% describe_lbl_ds
train_subject <- load_train_file("subject_train.txt") %>% desc_subject_ds

#### Test data
test_set <- load_test_file("X_test.txt") %>% desc_actvity_ds
test_labels <- load_test_file("y_test.txt") %>% describe_lbl_ds
test_subject <- load_test_file("subject_test.txt") %>% desc_subject_ds

## Merge the training and the test sets to create one dataset
## Extract only the measurements on the mean and standard deviation for each measurement
merge_data <- rbind(
                cbind(train_set, train_labels, train_subject),
                cbind(test_set, test_labels, test_subject)
              ) %>%
              select(
                matches("mean|std"), 
                one_of(col_name_subject, col_name_activity)
              )

## The second, independent tidy data set with the average of each variable for each activity and each subject
sec_cols <- c(col_name_subject, col_name_activity)
tidy_data <- melt(
               merge_data, 
               id = sec_cols, 
               measure.vars = setdiff(colnames(merge_data), sec_cols)
             ) %>%
             dcast(Subject + Activity ~ variable, mean)
             
## Store the result as 'txt'
write.table(tidy_data, file = "tidy_data.txt", sep = ",", row.names = FALSE)