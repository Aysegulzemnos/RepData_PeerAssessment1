

## ----loaddata------------------------------------------------------------

```r
unzip(zipfile="activity.zip")
```

```
## Warning in unzip(zipfile = "activity.zip"): error 1 in extracting from zip
## file
```

```r
data <- read.csv("activity.csv") 
```


## ------------------------------------------------------------------------

```r
library(ggplot2)
install.packages("psych")
```

```
## Error in install.packages : Updating loaded packages
```

```r
library(psych)
```


## Loading and preprocessing the data 




```{r} activitydataset <- read.csv("activity.csv", header=TRUE, na.strings = "NA")
   head(activitydataset)
   ```
## What is mean total number of steps taken per day?



```r
steps_per_day <- aggregate(steps ~ date, data = activitydataset, FUN=sum, na.rm = TRUE)
activitydataset_mean <-    mean(steps_per_day$steps)
activitydataset_mean
```

```
## [1] 10766.19
```

```r
hist(steps_per_day$steps,xlab="Frequency",ylab="Steps",main="Total Number of Steps Taken Each Day", border="purple",lwd=2,lty=268,col="pink") 
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

## What is the average daily activity pattern?


```r
 time_series <- tapply( activitydataset$steps,  activitydataset$interval, mean, na.rm = TRUE)

plot(row.names(time_series), time_series, type = "l", xlab = "5-min interval", 
     ylab = "Average across all Days", main = "Total Steps vs. 5-Minute Interval", 
     col = "blue")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
max_interval <- which.max(time_series)
names(max_interval)
```

```
## [1] "835"
```

## Imputing missing values


```r
activity_NA <- sum(is.na(activity))
activity_NA
```

```
## [1] 2304
```

##In the original data set aggregating (mean) steps over 5-minute interval

```r
meaninterval<- aggregate(steps ~ interval, activitydataset, FUN=mean)
```
##Merging the mean of total steps for a date with the original data set

```r
new_activitydataset <- merge(x=activitydataset, y=meaninterval, by="interval")
```
##Replacing the NA values with the mean for that 5-minute interval

```r
new_activitydataset$steps <- ifelse(is.na(new_activitydataset$steps.x), new_activitydataset$steps.y, new_activitydataset$steps.x)
```
##Merged dataset which will be subsetted in the next step by removing not required columns

```r
head(new_activitydataset)
```

```
##   interval steps.x       date  steps.y    steps
## 1        0      NA 2012-10-01 1.716981 1.716981
## 2        0       0 2012-11-23 1.716981 0.000000
## 3        0       0 2012-10-28 1.716981 0.000000
## 4        0       0 2012-11-06 1.716981 0.000000
## 5        0       0 2012-11-24 1.716981 0.000000
## 6        0       0 2012-11-15 1.716981 0.000000
```

##Fetching only the required columns (steps, date, interval) and storing in the new data set.

```r
new_activitydataset <- select(new_activitydataset, steps, date, interval)
```

```
## Error in select(new_activitydataset, steps, date, interval): could not find function "select"
```
##New dataset with NA imputed by mean for that 5-minute interval

```r
head(new_activitydataset)
```

```
##   interval steps.x       date  steps.y    steps
## 1        0      NA 2012-10-01 1.716981 1.716981
## 2        0       0 2012-11-23 1.716981 0.000000
## 3        0       0 2012-10-28 1.716981 0.000000
## 4        0       0 2012-11-06 1.716981 0.000000
## 5        0       0 2012-11-24 1.716981 0.000000
## 6        0       0 2012-11-15 1.716981 0.000000
```


## Are there differences in activity patterns between weekdays and weekends?
#Aggregating(summation) of steps over date

```r
steps_per_day_new<- aggregate(steps ~ date, new_activitydataset, FUN=sum)
```
#Plotting
#Setting up the pannel for one row and two columns
```
par(mfrow=c(1,2))
```
#Histogram after imputing NA values with mean of 5-min interval

```r
hist(steps_per_day_new$steps, 
     col="Red",
     xlab = "Steps", 
     ylab = "Frequency",
     ylim = c(0,35),
     main = "Total Number Of Steps Taken Each day \n(After imputing NA values with \n mean of 5-min interval)",
     cex.main = 0.7)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
#Histogram with the orginal dataset

```r
hist(steps_per_day$steps, 
     col="purple", 
     xlab = "Steps", 
     ylab = "Frequency",
     ylim = c(0,35),
     main = "Total Number Of Steps Taken Each day \n(Orginal Dataset)",
     cex.main = 0.7) 
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)
