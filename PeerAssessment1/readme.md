# Reproducible-Research - PeerAsseeement
title: "Reproducible Research-Assessment 1"

author: "Lin weifu"

date: "July , 2015"


###About
This was the first project for the **Reproducible Research** course in Coursera's Data Science specialization track. The purpose of the project was to answer a series of questions using data collected from a [FitBit](http://en.wikipedia.org/wiki/Fitbit).


##Synopsis
The purpose of this project was to practice:

* loading and preprocessing data
* imputing missing values
* interpreting data to answer research questions

## Data
The data for this assignment was downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data


```r

setwd('./data')
mydata <- read.csv("./activity.csv",header = TRUE, stringsAsFactors=FALSE)
mydata$date<- as.Date(mydata$date, "%Y-%m-%d")
time <- formatC(mydata$interval/100, 2, format = "f")
time <- strptime(time, "%H.%M")
mydata$time <- format(time, format = "%H:%M:%S")
mydata$time <- as.POSIXct(mydata$time, format = "%H:%M:%S"

```


## What is mean total number of steps taken per day?
Sum steps by day, create Histogram, and calculate mean and median.

```r
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-2](https://github.com/boyboykitty/Reproducible-Research/blob/master/PeerAssessment1/figure/PA1.png?raw=true) 


The mean and median of the total number of steps taken per day

```{r}

mean(agtable$steps)
median(agtable$steps)

```


##3.What is the average daily activity pattern?

Time series plot of the 5-minute interva

```{r}

agtable_mean<-aggregate(steps~time,mydata,mean,na.rm=TRUE)
plot(agtable_mean,type="l",col="red" ,main="Average daily activity pattern"
     ,xlab = " 5-Minute interval ",ylab="Mean steps")

```

![plot of chunk unnamed-chunk-2](https://github.com/boyboykitty/Reproducible-Research/blob/master/PeerAssessment1/figure/PA2.png?raw=true) 


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
agtable_mean$time <- format(agtable_mean$time, format = "%H:%M:%S")
max_steps_time <- which.max(agtable_mean$steps)
print(agtable_mean[max_steps_time,])

```

##4.Imputing missing values

Calculate and report the total number of missing values

```{r}

sum(is.na(mydata$steps))

```

Create a new dataset with the missing data replaced by mean and filled in.

```{r}

# Find the NA positions
na_pos <- which(is.na(mydata$steps))

# Create a vector of means
mean_vec <- rep(mean(mydata$steps, na.rm=TRUE), times=length(na_pos))

# Replace the NAs by the means
mydata$newsteps<-mydata$steps
mydata[na_pos, "newsteps"] <- mean_vec

# Clear the workspace
rm(mean_vec, na_pos)

```

Make a histogram of the total number of steps with missing data

```{r, echo=FALSE}

agtable2<-aggregate(mydata$newsteps, by=list(mydata$date), sum, na.rm = TRUE)
names(agtable2) <- c("date", "newsteps")
hist(agtable2$newsteps,col="red",main = "Total steps by day (NA replaced)"
     ,xlab = "Total steps",breaks=seq(from=0, to=25000, by=2500))

```

![plot of chunk unnamed-chunk-3](https://github.com/boyboykitty/Reproducible-Research/blob/master/PeerAssessment1/figure/PA3.png?raw=true) 

The mean and median are computed like

```{r}

mean(agtable2$newsteps)
median(agtable2$newsteps)

```

These values differ greatly from the estimates from the first part of the assignment. The impact of imputing the missing values is to have more data, hence to obtain a bigger mean and median value.

##5.Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend”

```{r}

# show the date as English date 

Sys.setlocale(,"C")  

# Create a new weekday dataframe 
mydata$weekday<-weekdays(mydata$date)
mydata$weekday[mydata$weekday %in% c("Saturday","Sunday")]<- "weekend"
mydata$weekday[mydata$weekday != "weekend"]<- "weekday"

# Compute the average number of steps taken, averaged across all daytype variable
mean_data <- aggregate(mydata$steps, 
                       by=list(mydata$weekday,mydata$interval), mean,na.rm=TRUE)
names(mean_data) <- c("weekday", "interval", "mean")

```

The time series plot take the following form:

```{r}

# Load the lattice graphical library
library(lattice)

# Compute the time serie plot
xyplot(mean ~ interval | weekday, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))

```

![plot of chunk unnamed-chunk-4](https://github.com/boyboykitty/Reproducible-Research/blob/master/PeerAssessment1/figure/PA4.png?raw=true) 
