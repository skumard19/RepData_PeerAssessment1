# Reproducible Research - Project1

## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.

## Loading and preprocessing the data

#### Loading the data
Download the data set from the course web site to current working directory and unzip.
Load data file (csv) into data frame *activity*.

```r
activity <- read.csv("activity.csv")
```

#### Processing the data

```r
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")

##pulling data without nas
clean <- activity[!is.na(activity$steps),]
```

## What is mean total number of steps taken per day?

#### Calculate the total number of steps taken per day

```r
## summarizing total steps per date
sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum)
colnames(sumTable)<- c("Date", "Steps")
```

#### Make a histogram of the total number of steps taken each day

```r
## Creating the historgram of total steps per day
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day", col = "blue")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

#### Calculate and report the mean and median of the total number of steps taken per day

```r
steps_mean   <- as.integer(mean(sumTable$Steps))
steps_median <- as.integer(median(sumTable$Steps))
```

Average of **10766** steps taken each day.  
Median of **10765** steps taken each day.

## What is the average daily activity pattern?
#### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(plyr)
library(ggplot2)
##pulling data without nas
clean <- activity[!is.na(activity$steps),]

##create average number of steps per interval
intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))

##Create line plot of average number of steps per interval
p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line() + 
  xlab("Interval") + 
  ylab("Average Number of Steps") + 
  ggtitle("Average Number of Steps per Interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
## Maximum steps by interval
maxSteps <- max(intervalTable$Avg)
## Which interval contains the maximum average number of steps
maxStepsInteral <- intervalTable[intervalTable$Avg==maxSteps,1]
```

**835**th interval had the maximum of **206.1698113** steps for a 5-minute interval.

## Imputing missing values
#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
##Number of NAs in original data set
stepsNA <- nrow(activity[is.na(activity$steps),])
```
The data set has 2304 rows with steps = 'NA'

#### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy for filling in NAs will be to substitute the missing steps with the average 5-minute interval based on the day of the week.

```r
## Create the average number of steps per weekday and interval
avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))

## Create dataset with all NAs for substitution
nadata<- activity[is.na(activity$steps),]
## Merge NA data with average weekday interval for substitution
newdata<-merge(nadata, avgTable, by=c("interval", "day"))
```

#### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
## Reorder the new substituded data in the same format as clean data set
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")

##Merge the NA averages and non NA data together
mergeData <- rbind(clean, newdata2)
```

#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
##Create sum of steps per date to compare with step 1
sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum)
colnames(sumTable2)<- c("Date", "Steps")

## Mean of Steps with NA data taken care of
steps_mean_imp <- as.integer(mean(sumTable2$Steps))
steps_mean_diff <- steps_mean_imp - steps_mean

## Median of Steps with NA data taken care of
steps_median_imp <- as.integer(median(sumTable2$Steps))
steps_median_diff <- steps_median_imp - steps_median
```


```r
## Creating the histogram of total steps per day, categorized by data set to show impact
hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

The new mean of the imputed data is **10821** steps compared to the old mean of **10766** steps.  
That creates a difference of **55** steps on average per day.


The new median of the imputed data is **11015** steps compared to the old median of **10765** steps.  
That creates a difference of **250** steps for the median.


However, the overall shape of the distribution has not changed.

## Are there differences in activity patterns between weekdays and weekends?

#### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
## Create new category based on the days of the week
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
table(mergeData$DayCategory)
```

```
## 
## Weekday Weekend 
##   12960    4608
```

#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice) 

## Summarize data by interval and type of day
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

##Plot data in a panel plot
xyplot(Avg~interval|DayCategory, 
       data=intervalTable2, 
       type="l",  
       layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       xlab="Interval", 
       ylab="Average Number of Steps"
      )
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

Yes, there is difference in activity patterns between weekdays and weekends.  
This may be due to the people who work during the week having more opportunity for activities on weekends.


