# set working directory
#setwd("~/Coursera/03_Getting_Cleaning_Data/Project")


# read the standard descriptive datasets: activitiy_label.txt & features.text
activities<-read.csv('./activity_labels.txt', header=FALSE, sep=" ", col.names=c("ActivityID", "Activity"))
features<-read.csv('./features.txt', header=FALSE,sep=" ", col.names=c("featureID", "feature")) 

# read the subject datasets (train and test) and append them together
testsubject<-read.fwf('./test/subject_test.txt', widths=2, sep=" ", as.is = FALSE, skip = 0, col.names="subject")
trainsubject<-read.fwf('./train/subject_train.txt', widths=2, sep=" ", as.is = FALSE, skip = 0, col.names="subject")
subject<-rbind(testsubject,trainsubject)
subject$RecordNr <- c(1:nrow(subject))

#read the the label datasets (train and test) and append them together
testylabel<-read.fwf('./test/y_test.txt', widths=1, sep=" ", as.is = FALSE, skip = 0, col.names="label")
trainylabel<-read.fwf('./train/y_train.txt', widths=1, sep=" ", as.is = FALSE, skip = 0, col.names="label")
ylabel<-rbind(testylabel,trainylabel)
ylabel$RecordNr <- c(1:nrow(ylabel))

#read the measurement datasets (train and test) and append them together
xtest<-read.table('./test/X_test.txt', header=FALSE)
xtrain<-read.table('./train/X_train.txt', header=FALSE)
x<-rbind(xtest,xtrain)
x$RecordNr <- c(1:nrow(x))


# make one dataset with subjects, activities and make sure it's kept in its original ordering
data1<-merge(subject,ylabel, by.x="RecordNr", by.y="RecordNr")
data2<-merge(data1,activities, by.x="label", by.y="ActivityID")
data3<-data2[order(data2$RecordNr),]

# Select only the measurement variables that contain mean() and std(). 
# The results are two vectors with the indices of the selected columns
SelVar1<-grep("mean()", features[,2] , ignore.case=FALSE, fixed=TRUE)
SelVar2<-grep("std()", features[,2] , ignore.case=FALSE, fixed=TRUE)

# Select the variables names that contain mean() in a vector 
SelVarName1<-as.matrix(features[SelVar1,2])
# Subset the measurement data set based on the vector of indices for the mean()
data4<-x[,SelVar1]
# Rename variable names
names(data4)<-SelVarName1
# Add recordnr
data4$RecordNr <- x$RecordNr


# Select the variables names that contain std() in a vector 
SelVarName2<-as.matrix(features[SelVar2,2])
# Subset the measurement data set based on the vector of indices for the mean()
data5<-x[,SelVar2]
# Rename variable names
names(data5)<-SelVarName2
# Add recordnr
data5$RecordNr <- x$RecordNr

# Combine the three datafiles based on RecordNr
data6<-merge(data3,data4, by.x="RecordNr", by.y="RecordNr")
data7<-merge(data6,data5, by.x="RecordNr", by.y="RecordNr")

# Calculate the means over each subject and each activity
result<-aggregate( as.matrix(data7[,5:70]), as.list(data7[,3:4]), FUN = mean)

# Write output file to a txt-file
write.table(result,file="TidyDataSet.txt", row.names=FALSE, col.names=TRUE)

View(result)
