###merge data
#read data(test&train)
traindata<-read.table("./UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
trainactivity<-read.table("./UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
trainsubject<-read.table("./UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
testdata<-read.table("./UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
testactivity<-read.table("./UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
testsubject<-read.table("./UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")

#merge data
merge_sub<-rbind(trainsubject,testsubject)
merge_data<-rbind(traindata,testdata)
merge_act<-rbind(trainactivity,testactivity)

#rename merge data 
merge_sub<-setnames(merge_sub,"V1","subject")
merge_act<-setnames(merge_act,"V1","activity")
namesdata<-read.table("./UCI HAR Dataset/UCI HAR Dataset/features.txt")#subset feature name of data
merge_data<-setnames(merge_data,as.character(namesdata[,2]))

#column bind
tempmerge<-cbind(merge_sub,merge_act)
data_all<-data.table(cbind(tempmerge,merge_data))

#set key
setkey(data_all,subject,activity)



###extract mean and std
extract<-grepl("(.*)mean(.*)|(.*)std(.*)",names(data_all))
extract_name<-grep("(.*)mean(.*)|(.*)std(.*)",names(data_all))
meanstd<-data_all[,extract_name,with=FALSE]
meanstd<-data.table(cbind(tempmerge,meanstd))



###name activities
#read activity_labels
activity_labels<-read.table("./UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")

#name them
activity_labels<-setnames(activity_labels,names(activity_labels),c("activity","activityname"))


###add labels
#merge activitylabels
data_use<-merge(meanstd,activity_labels,by="activity",all.x = TRUE)

#set key
setkey(data_use,subject,activity,activityname)

#melt data
data_melt<-melt(data_use,key(data_use))
data_melt<-setnames(data_melt,"variable","featurename")

#function of confirming element
grepelement<-function(charc){
        grepl(charc,data_melt$featurename)
}

#add factor with Jerk
data_melt$featJerk<-factor(grepelement("Jerk"),labels = c(NA,"Jerk"))

#add factor with Magnitude
data_melt$featMag<-factor(grepelement("Mag"),labels= c(NA,"Magnitude"))

#add factor with Domain
data_melt$featDomain<-factor(grepelement("^t"),labels = c("Frequence","Time"))

#add factor with instrument
data_melt$featinstrument<-factor(grepelement("Acc"),labels = c( "Gyroscope","Accelerometer"))

#add factor with variable
data_melt$featvariable<-factor(grepelement("mean"),labels = c("SD","Mean"))

#add factor with acceleration
temp1<-matrix(1:2,nrow=2)
temp2<-matrix(c(grepelement("Body"),grepelement("Gravity")),ncol=2)
data_melt$featacceleration<-factor(temp2 %*% temp1,labels=c("Body","Gravity"))

#add factor with axis
temp3<-matrix(1:3,nrow=3)
temp4<-matrix(c(grepelement("X"),grepelement("Y"),grepelement("Z")),ncol=3)
data_melt$axis<-factor(temp4 %*% temp3,labels=c(NA,"X","Y","Z"))

###create tidy data
data_tidy<-data_melt[,c(-2,-4),with=FALSE]
setkey(data_tidy,subject,activityname,featJerk,featMag,featDomain,featinstrument,featvariable,featacceleration,axis)

###write data_tidy
write.table(data_tidy,file="data_tidy.txt")
