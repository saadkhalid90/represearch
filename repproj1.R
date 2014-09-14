dir.create("reproducible research")
setwd(paste(getwd(),"/reproducible research",sep=""))
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "./repdata1.zip")
unzip("./repdata1.zip")
list.files()

projData<-read.csv("activity.csv")
day <- gl(61,288)
projData <- cbind(projData, day)

byDay <- aggregate(projData["steps"],by=list(projData$day),FUN=sum, na.rm=0)

names(byDay)[1]<-"Day"
mean(byDay$steps,na.rm=TRUE)
median(byDay$steps,na.rm=TRUE)


projData$interval <- as.factor(projData$interval)
byInterval <- aggregate(projData["steps"],by=list(projData$interval),FUN=mean, na.rm=TRUE)
names(byInterval)[1] <- "interval"
byInterval <- cbind(interval_index=1:288, byInterval)

with(byInterval, plot(interval_index, steps, type='l',main="Average steps in 5 min intervals", xlab="Interval Index (Total=288)", ylab="Avergae Steps"))
which(byInterval$steps==max(byInterval$steps))

table(is.na(projData$steps))

naidx <- which(is.na(projData$steps)==TRUE)
nadays<- projData$day[naidx]
nameans <- byDay$steps[nadays]
byDay$steps[is.na(byDay$steps)==TRUE]=0

projData$week <- as.factor(ifelse(weekdays(as.Date(projData$date)) %in% c("Saturday","Sunday"), "Weekend", "Weekday"))

projData$steps[naidx]=nameans


mean(byDay$steps,na.rm=TRUE)
median(byDay$steps,na.rm=TRUE)

byInterval_wd <- aggregate(projData["steps"],by=list(projData$interval,projData$week),FUN=mean, na.rm=TRUE)
names(byInterval_wd)[1:2]<-c("interval","week")
byInterval_we<-byInterval_wd[byInterval_wd$week=="Weekend",]
byInterval_wd<-byInterval_wd[byInterval_wd$week=="Weekday",]
par(mfrow=c(1,2))
with(byInterval_wd, plot(1:288, steps, type='l',main="Weekdays", xlab="Interval Index (Total=288)", ylab="Avergae Steps"))
with(byInterval_we, plot(1:288, steps, type='l',main="Weekends", xlab="Interval Index (Total=288)", ylab="Avergae Steps"))
