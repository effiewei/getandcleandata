#set path
mypath <- "/Volumes/Transcend/Statistics Language/R/Coursera/Collecting and Cleaning Data/Week 4/UCI HAR Dataset/Homework"
setwd(mypath)
#read in features name
features <- read.table("features.txt")
features[1] <- NULL
#read in test data
data <- read.table("X_test.txt")
colnames(data) <- features[,1]
subject <- read.table("subject_test.txt")
colnames(subject) <- "subject"
label <- read.table("y_test.txt")
colnames(label) <- "label"
test <- cbind(subject, data, label)
#read in train data
data <- read.table("X_train.txt")
colnames(data) <- features[,1]
subject <- read.table("subject_train.txt")
colnames(subject) <- "subject"
label <- read.table("y_train.txt")
colnames(label) <- "label"
train <- cbind(subject, data, label)
#combine test and train
combined <- rbind(test, train)
#TrueFalse: whether comtains mean or std
tf <- grepl("mean|std", colnames(combined))
colnames(combined) <- c("subject", 1:561, "label")
library(dplyr)
combined_tbl <- tbl_df(combined)
combined_meanstd <- select(combined_tbl, subject, which(tf), label)
#uses descriptive activity names to name the activities in the data set
combined_meanstd$label <- factor(combined_meanstd$label, labels = c("walking", "walking_upstaris", "walking_downstairs", "sitting", "standing", "laying"))
#change variable name
tf <- grepl("mean|std", features[,1])
features_tbl <- tbl_df(features)
features_true <- filter(features_tbl, tf == TRUE)
features_true_df <- as.data.frame(features_true)
features_final <- as.character(features_true_df[,1])
for (n in 1:79) {
	colnames(combined_meanstd)[n+1] <- features_final[n]
}
#group by subject and label
group <- group_by(combined_meanstd, subject, label)
output <- summarize_all(group, funs(mean))
write.table(output, "output.txt", row.name = FALSE)

