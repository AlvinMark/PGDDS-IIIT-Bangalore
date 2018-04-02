##Importing all relevant packages, Please note: In this solution a package titled scales is used to genereate plots with a percentage scale. It is a standard package and should work with evaluation packages.
##Set working directory to path on local system containing the uber request data set.
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)


###Importing data set and resolving data Inconsistencies.

##Importing the Dataset into the R-enviornment
uber_airportdata <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)
str(uber_airportdata)

#Data Description :The input dataset has the following six attributes:
#[1]  Request ID: A unique token representing a customer request.
#[2] Pickup point: Customer pickup point with two levels based on customer's location (City-represents customers to be picked up at the city and dropped to the airport; Airport-represents customers to be picked up from the airport and dropped to the city).
#[3] Driver ID: Unique identification number of a driver.
#[4] Status: It represents the customer request status and may take up one of three possible values; trip completed, cancelled or no cars available.
#[5] Request Timestamp: Contains date and time information at which the request was registered.
#[6] Drop Timestamp: Contains date and time information at which the trip was completed.

##Data Inconcistencies with respect to imported dataset:
#Request.timestamp containing the date and time information of when a unique request was made by a customer. This column is in a string format and contains a mix of dd-mm-yyyy and dd/mm/yyyy formats.
#Drop.timestamp contains the similar date and time information of when a customer was dropped after execution of drop request. It contains a mix of dd-mm-yyyy and dd/mm/yyyy
uber_airportdata$Request.timestamp<- str_replace_all(uber_airportdata$Request.timestamp, pattern = "[/]", replacement = "-") 
uber_airportdata$Drop.timestamp<- str_replace_all(uber_airportdata$Drop.timestamp, pattern = "[/]", replacement = "-")

#Convert the format of Request.timestamp and Drop.timestamp from charater to a viable Date time format i.e, POSIXct format for manupilation as date and time.
#Note: As the exact seconds during which the request ticket was generated or the drop was completed is not relevant to the scope of our analysis. To resolve the inconcistency in timestamps we will only consider Hours and Minutes for analysis.
#Therefore we will address these format inconsistencies and convert date format to a standart yyyy-mm-dd H:M and the included time format as hours:minutes. The resulting format will be stored in the request and drop timestamp columns.
uber_airportdata$Request.timestamp<- as.POSIXct(uber_airportdata$Request.timestamp, format= "%d-%m-%Y %H:%M")
uber_airportdata$Drop.timestamp<- as.POSIXct(uber_airportdata$Drop.timestamp, format= "%d-%m-%Y %H:%M")

##Deriving Relevant metrics from the dataset. 
uber_airportdata$Request.hour<- as.integer(format(uber_airportdata$Request.timestamp, "%H"))
uber_airportdata$Request.day<- as.factor(weekdays(uber_airportdata$Request.timestamp))
#creating a categorical attribute Customer.serviced wherein if the customer's trip was completed the record value is Customer Serviced else it is Customer Denied. This is useful in calculating the service rate.
uber_airportdata$Customer.serviced[uber_airportdata$Status=="Trip Completed"]<- "Customer Serviced"
uber_airportdata$Customer.serviced[uber_airportdata$Status=="Cancelled"]<- "Customer Denied"
uber_airportdata$Customer.serviced[uber_airportdata$Status=="No Cars Available"]<- "Customer Denied"

#Creating a column for the Time slot.
#This grouping is not hard-coded and is open to the interpretation of the analyst. For the analysis; 5am to 10am-Morning Peak Hour, 10am to 2pm-Late Noon, 2pm to 6pm-Early Evening, 6pm to 11pm-Late Evening Rush and 11pm to 5am-Night-Midnight.
uber_airportdata$Time.slot<- ifelse(uber_airportdata$Request.hour>=5 & uber_airportdata$Request.hour<=9,"Morning_Peak_Hour", ifelse(uber_airportdata$Request.hour>=10 & uber_airportdata$Request.hour<=13,"Late_Noon", ifelse(uber_airportdata$Request.hour>=14 & uber_airportdata$Request.hour<=17, "Early_Evening", ifelse(uber_airportdata$Request.hour>=18 & uber_airportdata$Request.hour<=22,"Late_Evening_Rush", "Night_Midnight"))))


##Begin Data Analysis:
#Plotting Overall Service Rate [Requests Denied vs. Requests Serviced %]
overall_servicerate<- ggplot(uber_airportdata, aes(x = as.factor(uber_airportdata$Customer.serviced), fill=factor(uber_airportdata$Customer.serviced))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) + 
  labs(title = "Plot1. Denial vs. Service Rate", y = "Percentage relative to Total number of Requests", x = "Customer Service Rate") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Customer Service Status")
overall_servicerate

#Breaking up Overall Denied Analysis into Completed, Denied or No Cars.
overall_status_breakup<- ggplot(uber_airportdata, aes(x = as.factor(uber_airportdata$Status), fill=factor(uber_airportdata$Status))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) + 
  labs(title = "Plot2. Overall Request Status Segmentation", y = "Percentage relative to Total number of Requests", x = "Request Status") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Request Status")
overall_status_breakup

#Total Number of Customers Denied based on Pick-up Location
number_denied<- ggplot(data= subset(uber_airportdata, uber_airportdata$Customer.serviced=="Customer Denied"), aes(x = as.factor(Pickup.point), fill=factor(Pickup.point)))+ geom_bar()+ labs(title = "Plot3. Number of Customer Requests Denied based on Pick-up Location", y = "Number of customers reqests denied", x = "Customer pick-up location", fill= "Customer Pick-up Location")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.3)
number_denied

#Breakup of Request status as a percentage of total requests for trips from the Airport.
airport_pickup<- subset(uber_airportdata, uber_airportdata$Pickup.point=="Airport" )
airport_status_breakup<-ggplot(airport_pickup, aes(x = as.factor(airport_pickup$Status), fill=factor(airport_pickup$Status))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  labs(title = "Plot 4. Airport-Pickup Customer Request Segmentation", y = "Percentage relative to Total number of Requests", x = "Request Status") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Request Status")
airport_status_breakup

#Breakup of Request status as a percentage of total requests for trips from the City.
city_pickup<- subset(uber_airportdata, uber_airportdata$Pickup.point=="City")
city_status_breakup<-ggplot(city_pickup, aes(x = as.factor(city_pickup$Status), fill=factor(city_pickup$Status))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  labs(title = "Plot5. City-Pickup Customer Request Segmentation", y = "Percentage relative to Total number of Requests", x = "Request Status") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Request Status")
city_status_breakup

#Time-Slot Status Analysis from comment in line number 44.
time_slot_breakup<- ggplot(uber_airportdata, aes(x=as.factor(uber_airportdata$Time.slot), fill=factor(uber_airportdata$Status)))+
  geom_bar()+ labs(title = "Plot6. Request Status Segmentation based on Time-Slots", y = "Number of Customer Requests", x = "Request Time-Slots", fill= "Request Status")
time_slot_breakup

#Time slot analysis for Airport Pick-up.
airport_pickup_time_slot_trend<- ggplot(airport_pickup, aes(x=as.factor(airport_pickup$Time.slot), fill=factor(airport_pickup$Status)))+
  geom_bar()+ labs(title = "Plot7. Request Status Segmentation based on Airport Pickup-point", y = "Number of Customer Requests", x = "Request Time-Slots", fill= "Request Status")
airport_pickup_time_slot_trend

#Time slip analysis for City Pick-up.
city_pickup_time_slot_trend<- ggplot(city_pickup, aes(x=as.factor(city_pickup$Time.slot), fill=factor(city_pickup$Status)))+
  geom_bar()+ labs(title = "Plot8. Request Status Segmentation based on City Pickup-point", y = "Number of Customer Requests", x = "Request Time-Slots", fill= "Request Status")
city_pickup_time_slot_trend

#Let's Observe Total Hourly Demand vs. Total Supply breakdown for this Analysis.
##Creating a new grouped dataset showing Request hour, demand and supply.
hourly_demand<- uber_airportdata %>% group_by("Request Hour"=uber_airportdata$Request.hour) %>% summarise("Total_Demand" = n())
hourly_supply<- subset(uber_airportdata, uber_airportdata$Status=="Trip Completed") 
hourly_supply<- group_by(hourly_supply, "Request Hour"=hourly_supply$Request.hour) %>% summarise("Total_Supply" = n())
hourly_analysis<- cbind(hourly_demand,hourly_supply[,-1])
hourly_analysis$Supply_gap<- hourly_analysis$Total_Demand - hourly_analysis$Total_Supply

#Plotting Hourly Supply-Demand Gap Trend
hourly_gap_trend<- ggplot(hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Total_Demand, group=1))+ geom_line(col= "red", size=2)+ geom_point(data=hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Total_Demand),col="black", size=2)+
  geom_line(data=hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Total_Supply, group=1), col="green", size=1) + geom_point(data=hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Total_Supply),col="black", size=1)+
  geom_line(data=hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Supply_gap, group=1), col="blue", size=1.5) + geom_point(data=hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Supply_gap),col="yellow",size=1.5)+
  labs(title = "Plot9. Aggregated Hourly Total Demand vs. Supply and Gap Trend", y = "Number of Requests", x = "Request Hour")
hourly_gap_trend

#Time-Slot basis Demand and Supply Analysis
##Creating a new grouped dataset showing Time slot, demand and supply.
time_slot_demand<- uber_airportdata %>% group_by("Request Time Slot"= uber_airportdata$Time.slot) %>% summarise("Total_Demand"=n())
time_slot_supply<- subset(uber_airportdata, uber_airportdata$Status=="Trip Completed") 
time_slot_supply<- group_by(time_slot_supply, "Request Time Slot"= time_slot_supply$Time.slot) %>% summarise("Total_Supply" = n())
time_slot_analysis<- cbind(time_slot_demand,time_slot_supply[,-1]) %>% arrange(desc(time_slot_demand$Total_Demand))
time_slot_analysis$Supply_gap<- time_slot_analysis$Total_Demand - time_slot_analysis$Total_Supply 

#Plotting Time slot Supply-Demand Gap Trend
time_slot_gap_trend<- ggplot(time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Total_Demand, group=1))+ geom_line(col= "red", size=2)+ geom_point(data=time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Total_Demand),col="black", size=2)+
  geom_line(data=time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Total_Supply, group=1), col="green", size=1) + geom_point(data=time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Total_Supply),col="black", size=1)+
  geom_line(data=time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Supply_gap, group=1), col="blue", size=1.5) + geom_point(data=time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Supply_gap),col="yellow",size=1.5)+
  labs(title = "Plot10. Demand vs. Supply and Gap Trend based on Time Slots", y = "Number of Requests", x = "Time-Slot")
time_slot_gap_trend

#We have clearly identified the issue with respect to request cancellation by drivers during Morning Peak hour. Let us study the impact of pick-up location on this issue.
#Location based Demand-Supply Gap for Morning_Peak_Hour. 
morning_peak_hour_analysis<- ggplot(data=subset(uber_airportdata, uber_airportdata$Time.slot=="Morning_Peak_Hour"), aes(x=as.factor(Status), fill=factor(Pickup.point)))+
  geom_bar()+ labs(title = "Plot11. Morning Peak Hour Analysis", y = "Number of Customer Requests", x = "Request Status", fill= "Customer Pickup Location")
morning_peak_hour_analysis

#We have clearly identified the issue with respect to no cars available during Late Evening Rush Hours. Let us study the impact of pick-up location on this issue.
#Location based Demand-Supply Gap for Late_Evening_Rush
late_evening_rush_hour_analysis<- ggplot(data=subset(uber_airportdata, uber_airportdata$Time.slot=="Late_Evening_Rush"), aes(x=as.factor(Status), fill=factor(Pickup.point)))+
  geom_bar()+ labs(title = "Plot12. Late Evening Rush Hour Analysis", y = "Number of Customer Requests", x = "Request Status", fill= "Customer Pickup Location")
late_evening_rush_hour_analysis