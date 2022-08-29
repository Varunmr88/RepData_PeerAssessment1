
    ## Loading and preprocessing the data
    #file download and unzip in the working directory
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
unzip(temp)
unlink(temp)
df1<-read.csv("activity.csv")
library(readr)
library(dplyr)
sumtable<- df1 %>% group_by(date)%>%
    filter(!is.na(steps))%>% 
    summarise(SUM=sum(steps))
hist(sumtable$SUM)
st1<-summary(sumtable)
st1
st3<-df1 %>% group_by(interval)%>%
    filter(!is.na(steps))%>% 
    summarise(SUM=mean(steps))
plot(st3,type="l")
tail(st3)
st3%>% top_n(1,SUM)
install.packages("mice")
library(mice)
NA<-c(is.na(df1$steps))
summary(NA)
summary(df1)
library(mice)
df2=df1
my_imp=mice(df2,m=5,method =c("mean","",""),maxit = 20)
my_imp$imp$steps
final=complete(my_imp,1)
str(final)
md.pattern(df1)

df2<-df1%>%group_by(interval) %>%
    filter(!is.na(steps))%>%
    summarise(avg=mean(steps))
df3<- df1%>%
      left_join(df2,by="interval")%>%
      mutate(steps=coalesce(steps,avg))%>%
      select(steps,date,interval)
head(df3)
md.pattern(df3)
head(df2)
head(d)
View(df3)
df3$date<-as.Date(df3$date)
str(df3)
df4<-df3 %>% mutate(Day=ifelse(weekdays(date)=="Sunday" | weekdays(date)=="Saturday",
               "weekend","weekday"))
df4$Day<-as.factor(df4$Day)
str(df4)

library(ggplot2)
sumtable3<- df4 %>% 
  group_by(interval,Day)%>%
  summarise(avg=mean(steps))
ggplot(sumtable3,aes(interval,avg))+
  geom_line(color = "steelblue", size = 0.5)+
  facet_wrap(~Day,dir="v")

sumtable3

  
