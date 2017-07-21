Working on activity data
========================

Loading the File and reading the summary
----------------------------------------

``` r
data = read.csv("C:\\Users\\nilak\\Coursera_DSTB\\activity.csv")
summary(data)
```

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

Histogram of total steps taken each day
---------------------------------------

``` r
totalsteps = aggregate(steps~date,data,sum)
names(totalsteps)[2] = 'totalsteps'
hist(totalsteps$totalsteps,xlab = 'Total Steps in a day',ylab = 'Frequency of total steps',main = 'Histogram of total steps', col = 'blue')
```

![](Week_2_Activity_Analysis_files/figure-markdown_github/unnamed-chunk-2-1.png)

Mean and Median of the data Steps
---------------------------------

``` r
print(c(mean(totalsteps$totalsteps),median(totalsteps$totalsteps)))
```

    ## [1] 10766.19 10765.00

Histogram of steps in different intervals
-----------------------------------------

``` r
intervalsteps = aggregate(steps~interval,data,mean)
names(intervalsteps)[2] = 'Avg steps'
hist(intervalsteps$`Avg steps`,xlab = "Average steps in the 5 min interval",ylab = 'Frequency of the avgsteps over all days',main = 'Histogram of the average steps in the 5 min interval',col = 'green')
```

![](Week_2_Activity_Analysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

Time Series Plot of the steps taken in the five minute interval
---------------------------------------------------------------

``` r
plot(intervalsteps$interval, intervalsteps$`Avg steps`, type="l", col="green" , lwd=2)
```

![](Week_2_Activity_Analysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

Finding the interval that contains on average max steps
-------------------------------------------------------

``` r
intervalsteps[which.max(intervalsteps$`Avg steps`),1]
```

    ## [1] 835

Number of missing steps
-----------------------

``` r
sum(is.na(data$steps))
```

    ## [1] 2304

Filling missing values with mean of interval steps
--------------------------------------------------

``` r
datafill = merge(x = data, y = intervalsteps, by = "interval", all.x = TRUE)
datafill$steps[is.na(datafill$steps)] = datafill$`Avg steps`[is.na(datafill$steps)]
sum(is.na(datafill))
```

    ## [1] 0

Histogram of total steps taken each day
---------------------------------------

``` r
totalstepsfill = aggregate(steps~date,datafill,sum)
names(totalstepsfill)[2] = 'totalsteps'
hist(totalstepsfill$totalsteps,xlab = 'Total Steps in a day',ylab = 'Frequency of total steps',main = 'Histogram of total steps', col = 'red')
```

![](Week_2_Activity_Analysis_files/figure-markdown_github/unnamed-chunk-9-1.png)

Median and mean of the total steps taken each day after imputing missing values
-------------------------------------------------------------------------------

``` r
mean(totalstepsfill$totalsteps)
```

    ## [1] 10766.19

``` r
median(totalstepsfill$totalsteps)
```

    ## [1] 10766.19

Creating the weekday variable
-----------------------------

``` r
datafill$date = as.Date(datafill$date)
datafill$week = weekdays(datafill$date)
datafill$weekfactor = ifelse(datafill$week == "Monday"|datafill$week == "Tuesday"|datafill$week == "Wednesday"|datafill$week == "Thursday"|datafill$week == "Friday" ,1,0)
```

Time Series Plot of the average steps taken in the 5 minute intervals on weekdays and weekends
----------------------------------------------------------------------------------------------

``` r
plot(intervalsteps$interval, intervalsteps$`Avg steps`, type="l", col="green" , lwd=2)
intervalsteps = aggregate(steps~interval+weekfactor,datafill,mean)
names(intervalsteps)[3] = 'Average'
names(intervalsteps)[2] = 'weekfactor'
library(lattice)
```

    ## Warning: package 'lattice' was built under R version 3.3.3

![](Week_2_Activity_Analysis_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
xyplot(Average~interval|factor(weekfactor),type='l', layout=c(1,2), xlab='Interval',ylab='Number of Steps',data = intervalsteps,col = "brown")
```

![](Week_2_Activity_Analysis_files/figure-markdown_github/unnamed-chunk-12-2.png)
