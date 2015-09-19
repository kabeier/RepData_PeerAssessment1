
go<-function(){
        library(plyr)
        data<-read.csv("activity.csv")
        data$date<-as.Date(data$date,"%Y-%m-%d") 
        
        
        datasum<-ddply(data,.(date),summarize, steps= sum(steps))
        
        ##print histogram on number of steps per day
        print(hist(datasum$steps, main="Number of Steps per Day",xlab="Numer of Steps",breaks=15,col="red"))
        
        ##prints mean steps per day
        print(mean(datasum$steps,na.rm=TRUE))
        
        ##prints median steps per day
        print(median(datasum$steps,na.rm=TRUE))
        
        ## average steps across days and plot by times
        dataave<-ddply(data,.(interval),summarize, steps=mean(steps,na.rm=TRUE))
        plot1<-plot(x=dataave$interval, y=dataave$steps,type="l",xlab="Interval",ylab="Average Steps",main="Daily Average Number of Steps per  Interval",col="red")
        
        
        ##dealing with NAs
        NAs <-sapply(data, function(data) sum(length(which(is.na(data)))))
        NAs <- data.frame(NAs)
        print(NAs)## prints Number of NAs in each column (finding our NAs)
        
        
        
        ##add col to show it is imputed or not
        dataref<-mutate(data,isImputed = is.na(data$steps))
        
        ##make a key of average steps per interval and day of the week to use to impute NA values
        dataref<-ddply(dataref[dataref$isImputed==FALSE,],.(interval,date),summarize, steps=mean(steps,na.omit=TRUE),date=weekdays(date))
        dataref<-aggregate(dataref$steps,list(dataref$interval, dataref$date),mean)
        dataref<-setNames(dataref,c("interval","date","steps"))
        
        ##create data with NAs imputed using dataref
        dataimp<-data
        dataimp$date<-weekdays(dataimp$date)
        dataimp<-mutate(dataimp,isImputed = is.na(data$steps))
        library(data.table)
        setDT(dataimp)
        setDT(dataref)
        ##Use int for steps (rounds the average to int)
        dataref$steps<-as.integer(dataref$steps)
        
        setkey(dataimp,interval,date); setkey(dataref,interval,date)
        dataimp<-
                dataimp[dataref, steps := ifelse(is.na(steps), i.steps, steps) ]
        
        dataimp<-data.frame(dataimp)
        
        #find NAs in imputed data
        NAis <-sapply(dataimp, function(dataimp) sum(length(which(is.na(dataimp)))))
        print("NAs in dataimp")
        print(NAis )## prints Number of NAs in dataframe (should have none)
        
        ##seperate weekends from weekdays
        library(chron)
        library(ggplot2)
        dataimp$date<-is.weekend(data$date)
        datawk<-ddply(dataimp, .(interval, date, isImputed),
                      summarize, steps=mean(steps)
        )
        
        datawk<-ddply(datawk,.(interval,date,isImputed),summarize, steps=sum(steps),date=if(date==TRUE){date<-"Weekend"}else{date<-"Weekday"})
        
 
        
        plot.final<-qplot(x=interval,y=steps,data=datawk,
                          geom=c("point","line"),facets = date~.,xlab="Interval",ylab="Ave Steps",type="l", col=isImputed, main="Average steps per interval")
        print(plot.final)
        
        
        
}
