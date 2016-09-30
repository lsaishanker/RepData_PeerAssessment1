# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data




```r
 png(file="./instructions_fig/Histogram_of_steps_each_day.png",height=480,width = 480)
 fig1<-hist(aggregateStepsByDay$steps,
        xlab = "Steps",
        col="red",
        main = "Steps per day"
   )
print(fig1)
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1]  5 12 28  6  2
## 
## $density
## [1] 1.886792e-05 4.528302e-05 1.056604e-04 2.264151e-05 7.547170e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "aggregateStepsByDay$steps"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

```r
dev.off()
```

```
## png 
##   2
```

```r
meanSteps<-mean(aggregateStepsByDay$steps)
medianSteps<-median(aggregateStepsByDay$steps)

## What is mean total number of steps taken per day?
print(paste("Mean of Steps per day:",meanSteps))
```

```
## [1] "Mean of Steps per day: 10766.1886792453"
```

```r
print(paste("Median of Steps per day:",medianSteps))
```

```
## [1] "Median of Steps per day: 10765"
```

```r
## What is the average daily activity pattern?
png(file="./instructions_fig/Time_series_average_steps_taken.png",height=480,width = 480)
fig2<-plot( aggregateStepsByDayAndInterval$interval,
         aggregateStepsByDayAndInterval$steps,
          ylab = "Steps",
          type = "l",
          xlab="Interval",
          main = "Average Daily Activity Pattern",
         col="darkred"
    )
print(fig2)
```

```
## NULL
```

```r
dev.off()
```

```
## png 
##   2
```

```r
aggregateStepsByDayAndInterval[which.max(aggregateStepsByDayAndInterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
nrow(monitoringData[is.na(monitoringData$steps),])
```

```
## [1] 2304
```

```r
## Imputing missing values

monitoringDataNoNAs<-monitoringData
monitoringDataNoNAs[is.na(monitoringDataNoNAs)]<-0

nrow(monitoringDataNoNAs[is.na(monitoringDataNoNAs$steps),])
```

```
## [1] 0
```

```r
noNAaggregageStepsByDay<-aggregate(steps ~ date, monitoringDataNoNAs, sum)

 png(file="./instructions_fig/Histogram_of_steps_each_day_after_imputing.png",height=480,width = 480)
 fig3<-hist(noNAaggregageStepsByDay$steps,
        xlab = "Steps",
        col="blue",
        main = "Steps per day no NA's"
   )
 print(fig3)
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1] 13 12 28  6  2
## 
## $density
## [1] 4.262295e-05 3.934426e-05 9.180328e-05 1.967213e-05 6.557377e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "noNAaggregageStepsByDay$steps"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

```r
dev.off()
```

```
## png 
##   2
```

```r
noNAmeanSteps<-mean(noNAaggregageStepsByDay$steps)
noNAmedianSteps<-median(noNAaggregageStepsByDay$steps)
print(paste("Mean of Steps per day:",noNAmeanSteps))
```

```
## [1] "Mean of Steps per day: 9354.22950819672"
```

```r
print(paste("Median of Steps per day:",noNAmedianSteps))
```

```
## [1] "Median of Steps per day: 10395"
```

```r
#yes there is a difference after imputing the dataset.


## Are there differences in activity patterns between weekdays and weekends?

Weektype<-as.factor(ifelse(weekdays(monitoringDataNoNAs$date)=="Saturday" | weekdays(monitoringDataNoNAs$date)=="Sunday","Weekend","Weekday"))

names(Weektype)<-"Weektype"

newNoNaDataSet<-cbind(monitoringDataNoNAs,Weektype)

stepsByDayType<-aggregate(steps ~ interval + Weektype, newNoNaDataSet, mean)
 png(file="./instructions_fig/Panel_plot_compare_average_steps_per_5-minute_interval_across_weekdays_and_weekends.png",height=480,width = 480)
 g<-ggplot(stepsByDayType, aes(interval,steps, color = Weektype)) +
        geom_path() +  
        facet_grid(. ~ Weektype ) +
        labs(x="Interval", y="Steps", title="Avgerage Daily Steps by Weektype")
   
    print(g)
dev.off()
```

```
## png 
##   2
```
