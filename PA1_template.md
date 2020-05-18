# Coursera Data Science Courses Projects  
# Reproducible Research - Week 2 Peer Graded Project -  
**Author: Mohamed Osama**  
--------------------------------------------------------  
**Date: May 17, 2020**


**1- Loading and preprocessing the data**  
Load the data Process/transform the data (if necessary) into a format suitable for your analysis
```{r,echo = TRUE}
#Get Your Current Working Directory to Download Data In it 
getwd()
#Make Sure that the Dataset is Downloaded In your Current Working Directory
dir()

#Load Your Data To Your Environment To Work On It
df<-read.csv("activity.csv")
head(df)

```
## 2- Calculate Total Number Of Steps Taken Each Day  
**Showing The Histogram Corresponding to that Calculation**

```{r,echo = TRUE}
#Calculating Total Steps For Each Day
dfused<-aggregate(steps ~ date,df,sum)
head(dfused)
# Plotting The Histogram
hist(dfused$steps,breaks = 25 ,xlab = "steps",ylab = "frequency", main = "Histgram of Total Steps" , ylim = c(0,15) , col = "red")

```

## 3- Calculate Mean And Median Of steps Each Day  
  
Mean : 
```{r,echo = TRUE}
mean(dfused$steps)
```
Median:
```{r,echo = TRUE}
median(dfused$steps)
```
## 4- Time series plot of the average number of steps taken  
```{r,echo = TRUE}
avaraged_day <- aggregate(steps~interval , df ,mean)
head(avaraged_day)
plot(avaraged_day$interval,avaraged_day$steps,type = "l",col="blue", xlab = "5 minutes Interval" , ylab = "Average Number of Steps steps", main = "Average daily activity pattern")
```

## 5-The(5)minute interval that on average, contains the maximum number of steps  
At first, we can look at the plot of the number of steps taken averaged across all days, along all 5-min intervals

```{r,echo = TRUE}
max_steps<-which.max(avaraged_day$steps)
max_interval <- avaraged_day[max_steps,1]
max_interval
```

## 6-Code to describe and show a strategy for imputing missing data  
- 1st I calculated Number Of Missing Values :
```{r,echo = TRUE}
n_missing<- sum(is.na(df))
n_missing
```
**- Then I Used The Median Function - For Steps of Each Interval To Replace The Missing Values :**  

- I installed The Package ("Hmisc"), Then I used It To Impute The Missing Values

```{r,echo = TRUE}
# install.packages("Hmisc")
library(Hmisc)
df_filled <- df
df_filled$steps <- impute(df$steps, fun=median)
dfImputed<-aggregate(steps ~ date,df_filled,sum)
```

## 7-Histogram of the total number of steps taken each day after missing values are imputed
```{r,echo = TRUE}
hist(dfImputed$steps,breaks = 25, ylim = c(0,15) ,xlab = "steps",ylab = "frequency", main = "Histgram of Total StepsWith Imputed NA's" , col = "red")
```

**The effect Of Imputing Can Be shown By Comparing The Values Of Mean & Median Before & After The Imputation**  
Mean Of Imputed Data:
```{r,echo = TRUE}
mean(dfImputed$steps)
```
Median Of Imputed Data:
```{r,echo = TRUE}
median(dfImputed$steps)
```
Mean Of Original Data:
```{r,echo = TRUE}
mean(dfused$steps)
```
Median Of Original Data:
```{r,echo = TRUE}
median(dfused$steps)
```

## 8-Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends  

```{r,echo = TRUE}
df_filled$date <- as.Date(df_filled$date)
df_filled$weekday <- weekdays(df_filled$date)
df_filled$weekend <- ifelse(df_filled$weekday=="Saturday" | df_filled$weekday=="Sunday", "Weekend", "Weekday" )
meandataweekendweekday <- aggregate(df_filled$steps , by= list(df_filled$weekend,df_filled$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")
ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
ggtitle("Comparison of Average Number of Steps in Each Interval")
```

