library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(skimr)

##Reading the data set
bike_data <- read.csv("total_bike_ride_data.csv")
head(bike_data)

##Structure check
str(bike_data)

##Data Summary
skim(bike_data)

##New column called ride_date and changed its data type
bike_data$ride_date<-as.Date(bike_data$started_at)
##Calculating ride durations
bike_data$started_at<-as_datetime(bike_data$started_at)
bike_data$ended_at<-as_datetime(bike_data$ended_at)

##Creating day , week , month , year output out of the current data
bike_data$month <- strftime(bike_data$ride_date, "%B")
bike_data$day <- strftime(bike_data$ride_date, "%d")
bike_data$year <-strftime(bike_data$ride_date, "%Y")
bike_data$day_of_week <- strftime(bike_data$ride_date, "%A")

##checking column names & data summary
colnames(bike_data)
skim(bike_data)

##creating new column called lenght of ride 
bike_data$length_of_ride=difftime(bike_data$ended_at,bike_data$started_at)

##check
summary(bike_data$length_of_ride)

##changing data format into numeric
bike_data$length_of_ride=as.numeric(bike_data$length_of_ride)

##filtering out 0 seconds long rides
bike_data_2<-filter(bike_data,length_of_ride>0)

bike_data_2<-filter(bike_data,rideable_type!="docked_bike")

##min and max rides (in seconds)
min(bike_data_2$length_of_ride)
max(bike_data_2$length_of_ride)

## Average length of ride (in seconds)
bike_data_2%>%summarise(average_length_ride=mean(length_of_ride))

##length of ride by user type (in seconds)
aggregate(bike_data_2$length_of_ride~bike_data_2$member_casual,FUN=mean)
aggregate(bike_data_2$length_of_ride~bike_data_2$member_casual,FUN=median)
aggregate(bike_data_2$length_of_ride~bike_data_2$member_casual,FUN=max)
 
##Mean length of ride by member type and day of week
aggregate(bike_data_2$length_of_ride~bike_data_2$member_casual+bike_data_2$day_of_week,FUN=mean) 

##Max length of ride by member type and weekday
aggregate(bike_data_2$length_of_ride~bike_data_2$member_casual+bike_data_2$day_of_week,FUN=max) 

##Mean ride length by rider type and months
aggregate(bike_data_2$length_of_ride~bike_data_2$member_casual+bike_data_2$month,FUN=mean)

##count of total bike rides per month by rider type 
bike_data_2%>%count(month,member_casual)

##usage of each bike type by rider type
bike_data_2%>%count(member_casual,rideable_type)

##Visualization 

##Average ride length per ride type and day
bike_data_2%>%group_by(member_casual,day_of_week)%>%summarise(average_ride_length=mean(length_of_ride))%>%
  ggplot(aes(x=member_casual,y=average_ride_length,fill=day_of_week))+
  geom_bar(position="Dodge",stat="identity")+
  labs(title="distribution of average ride length by week",subtitle="sorted by rider type")

##Trips less than 5 minutes
bike_data_2%>%group_by(day_of_week,member_casual)%>%filter(length_of_ride<300)%>%summarise(average_ride_length=mean(length_of_ride))%>%
  ggplot(aes(x=day_of_week,y=average_ride_length,fill=member_casual))+
  geom_bar(position='Dodge',stat='identity')+
  labs(title="Average ride length among different members by day of week which is less than 5 mins",subtitle="Membership compensation")

##Trips more than 5 mins
bike_data_2%>%group_by(day_of_week,member_casual)%>%filter(length_of_ride>300)%>%summarise(average_ride_length=mean(length_of_ride))%>%
  ggplot(aes(x=day_of_week,y=average_ride_length,fill=member_casual))+
  geom_bar(position='Dodge',stat='identity')+
  labs(title="Average ride length among different members by day of week which is more than 5 mins",subtitle="Membership compensation")

##Total Rides per month by rider type
bike_data_2%>%group_by(month,member_casual)%>%summarise(Ridenumbers=n())%>%
  ggplot(aes(x=month,y=Ridenumbers,fill=member_casual))+
  geom_bar(position='Dodge',stat='identity')+
  labs(title="Distribution of number of rides by month",subtitle="Membership comparision")

##Total length of Ride per month by rider types
bike_data_2%>%group_by(month,member_casual)%>%summarise(totalrideduration=sum(length_of_ride))%>%
  ggplot(aes(x=month,y=totalrideduration,fill=member_casual))+
  geom_bar(position='Dodge',stat='identity')+
  labs(title="distribution of total ride duration by month",subtitle="Membership comparision")

##Total Rides per year by rider type
bike_data_2%>%group_by(year,member_casual)%>%summarise(Ridenumbers=n())%>%
  ggplot(aes(x=year,y=Ridenumbers, fill=member_casual)) +
  geom_bar(position='Dodge',stat='identity') +
  labs(title="distribution of number of rides by year",subtitle="Membership comparision")