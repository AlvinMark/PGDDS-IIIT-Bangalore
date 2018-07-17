#Time Series Forecasting for Global Retail Chain [Case Study]
#Coded by: Team Code Ninjas 
#Set Working Directory to location on local system containing the Global Superstore dataset
#Brief: Build a Time Series Forecasting model for the Demand and Sales for the top profit segments of a Global Retailer.

#Importing relevant packages into the R-environment
library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)
require(graphics)
library(lubridate)
library(stats)

#Steps followed during the execution of this Case Study/Project:
#[I] Business Understanding.
#[II] Data Understanding and Feature Engineering.
#[III] Data Preparation and deriving 21 Market Buckets from transactional data.
#[IV] Identifying the Optimal Profit Market Buckets as Focus Area of the Forecasting Model. 
#[V] Building Time Series Models to Forecast Demand and Sales of aforementioned Market Bucket segments. 
#[VI] Model Evaluation[MAPE], Model Selection, Forecasting Demand and Sales for the future 6 month Period.


#************************* [I] Business Understanding *************************#

# GLOBAL MART is an online retail giant having worldwide operations in 147 countries grouped into 7 Global Market Regions.
# GLOBAL MART retails wide range of entities from 3 product categories [1. Technology 2. Furniture 3. Office Supplies]
# It serves customers from 3 consumer segments [1. Consumer 2. Home Office 3. Corporate]
# For a store of operating in this mass scale planning Operations and Logistics becomes a monumental task. 
# The Sales/Operations requires to finalize the plan for the next six months by forecasting the demand and sales for the nect six months.
# This forecast would help manage revenue and inventory accordingly.

# Goals of this Case Study
#1. Subset the data from the transactional database to form the 21 Market Buckets [7 Global Market Regions x 3 Consumer Segments].
#2. Identify the top 2 Most Profitable and Consistently Profitable Market Buckets.
#3. Build a Time Series Model to forecast the Demand and Sales for these two Market Buckets using 1. Classical Decomposition and 2.Auto Arima
#4. Perform Model Evaluation using MAPE and Accuracy. Use the final models to predict the Demand and Sales of the next 6 months for the 2 Market Buckets.

#************************* [II] Data Understanding *************************#

#Importing the datasets into the R-environment
retail_mart_db<- read.csv("Global Superstore.csv", stringsAsFactors = FALSE, na.strings = c(" ", "", "NA", "na", "N/A", "n/a"))

#Understanding the dimentionality of the dataset
dim(retail_mart_db)
#The Transactional database contains 51,290 records with 24 attributes.
#Each record corresponds to a Customer Order.

str(retail_mart_db)
#As we can see Order.Date are Shipping.Date are represented as character format. We will convert them to Date Objects.
#Row.ID is synonymous to serial number and is not going to be of significance to us. Therefore we will eliminate it.
retail_mart_db<- retail_mart_db[,-1]

#Data Dictionary to understand the attribute significance:
#1. Order ID : Unique ID of the transaction 
#2. Order Date : Date on which the order was placed
#3. Ship Date : Date on which the shipment was made
#4. Ship Mode : The mode of shipment (category)
#5. Customer ID : The unique ID of the customer
#6. Customer Name : Name of the customer
#7. Segment : The market segment to which the product belongs
#8. City : City of the delivery address
#9. State : State of the delivery address
#10. Country : Country of the delivery address
#11. Postal Code : Postal code of the delivery address
#12. Market : Market segment to which the customer belongs
#13. Region : Geographical region of the customer
#14. Product ID : Unique ID of the product
#15. Category : Category of the product
#16. Sub-Category : Sub-category of the product
#17. Product Name : Name of the product
#18. Sales : Total sales value of the transaction
#19. Quantity : Quantity of the product ordered
#20. Discount : Discount percentage offered on the product
#21. Profit : Profit made on the transaction
#22. Shipping Cost : Shipping cost incured on the transaction
#23. Order Priority : Priority assigned to the order

#Checking for Duplicates in the dataset
length(which(duplicated(retail_mart_db)==TRUE))
#Therefore there are no duplicated rows or identical records in the dataset

#Checking if Order.ID is the primary key of this dataset
length(unique(retail_mart_db$Order.ID))
length(unique(retail_mart_db$Customer.ID))
length(unique(retail_mart_db$Product.ID))
#Since an Order can contain several Products and it can be placed on the Order Date by the Customer. 
#Hence there are 25,035 unique Order.ID 's but each order can contain several Product.ID's as Shown below.
#A combination of Order ID-Product ID can be used as the primary key for this dataset
#  Order ID	  Order Date	   Product ID
#AE-2014-3830	13-12-2014	TEC-MOT-10001535
#AE-2014-3830	13-12-2014	OFF-ELD-10002297
#AE-2014-3830	13-12-2014	OFF-BIC-10002270
#AE-2014-3830	13-12-2014	OFF-ROG-10003993
#AE-2014-3830	13-12-2014	OFF-AVE-10000357
#AE-2014-3830	13-12-2014	OFF-AVE-10004827


#Checking the dataset for Missing Values.
sapply(retail_mart_db, function(x) sum(is.na(x)))
trial_1<-retail_mart_db[which(retail_mart_db$Country!="United States"),]
sum(is.na(trial_1$Postal.Code))
rm(trial_1)
#Only Postal Code contains 41,296 missing records. It is also observed that the Postal Code of customers only in the United States is recorded.
#As this attribute is not critical to our time series model we can consider our dataset to have no critical missing values.
#Therefore Data Imputation is not required.

#From the previous segment we know that the Order Date and the Shippment Date are not in standard Format
#We will now transform the above date objects into standard date objects with the help of lubridate package
retail_mart_db$Order.Date<- parse_date_time(retail_mart_db$Order.Date, orders = c("dmy", "mdy"))
retail_mart_db$Ship.Date<- parse_date_time(retail_mart_db$Ship.Date, orders = c("dmy", "mdy"))
#We will now consider Order.Date as our reference point as it is recorded at Point of Sale for GLOBAL MART.

#Identifying the Earliest Order.Date
min(retail_mart_db$Order.Date)

#Identifying the Latest Order.Date
max(retail_mart_db$Order.Date)

#It is clear that the Order.Date information recorded ranges from 1st Jan 2011 to 31st Dec 2014. 
#We have 48 months of recorded information.
#We have derived a New Metric named Month.Code it is to represent the month in which the order is placed from 1st Jan 2011.
#This implies that all Order placed in Jan,2011<->Month.Code=1 , Feb,2011<->Month.Code=2,... Dec,2011<->Month.Code=12,........... Dec,2014<->Month.Code=48.
#This same Month.Code will be used for aggregation and will serve as the new Time Stamps for our Forecasting Model.
retail_mart_db$Month.Code <- sapply(retail_mart_db$Order.Date, function(x) length(seq(from= min(retail_mart_db$Order.Date), to=x, by='month')))
range(retail_mart_db$Month.Code)

#************************* [III] Data Preparation *************************#

unique(retail_mart_db$Segment)
#There are 3 unique Customer Segments [1]"Consumer" [2]"Corporate" [3]"Home Office"

unique(retail_mart_db$Market)
#There are 7 unique Market Segments as follows:
#1. Africa - African Continent
#2. APAC - Asia Pacific/Asia Central
#3. Canada - Canada
#4. EMEA - Europe, Middle East and Africa
#5. EU - European Union
#6. LATAM - Latin America
#7. US - United States of America

unique(retail_mart_db$Category)
#There are 3 unique product categories sold by GLOBAL MART [1]"Technology" [2]"Furniture" [3]"Office Supplies"

#We will now create 21 Market Buckets based on Market Segment and Customer Segment

#1. Africa - African Continent [Market Buckets]
af_consmr<- subset(retail_mart_db, retail_mart_db$Segment=="Consumer" & retail_mart_db$Market=="Africa")
af_corprt<- subset(retail_mart_db, retail_mart_db$Segment=="Corporate" & retail_mart_db$Market=="Africa")
af_homeof<- subset(retail_mart_db, retail_mart_db$Segment=="Home Office" & retail_mart_db$Market=="Africa")

#2. APAC - Asia Pacific/Asia Central [Market Buckets]
apac_consmr<- subset(retail_mart_db, retail_mart_db$Segment=="Consumer" & retail_mart_db$Market=="APAC")
apac_corprt<- subset(retail_mart_db, retail_mart_db$Segment=="Corporate" & retail_mart_db$Market=="APAC")
apac_homeof<- subset(retail_mart_db, retail_mart_db$Segment=="Home Office" & retail_mart_db$Market=="APAC")

#3. Canada - Canada [Market Buckets]
cnda_consmr<- subset(retail_mart_db, retail_mart_db$Segment=="Consumer" & retail_mart_db$Market=="Canada")
cnda_corprt<- subset(retail_mart_db, retail_mart_db$Segment=="Corporate" & retail_mart_db$Market=="Canada")
cnda_homeof<- subset(retail_mart_db, retail_mart_db$Segment=="Home Office" & retail_mart_db$Market=="Canada")

#4. EMEA - Europe, Middle East and Africa [Market Bucket]
emea_consmr<- subset(retail_mart_db, retail_mart_db$Segment=="Consumer" & retail_mart_db$Market=="EMEA")
emea_corprt<- subset(retail_mart_db, retail_mart_db$Segment=="Corporate" & retail_mart_db$Market=="EMEA")
emea_homeof<- subset(retail_mart_db, retail_mart_db$Segment=="Home Office" & retail_mart_db$Market=="EMEA")

#5. EU - European Union [Market Bucket]
eu_consmr<- subset(retail_mart_db, retail_mart_db$Segment=="Consumer" & retail_mart_db$Market=="EU")
eu_corprt<- subset(retail_mart_db, retail_mart_db$Segment=="Corporate" & retail_mart_db$Market=="EU")
eu_homeof<- subset(retail_mart_db, retail_mart_db$Segment=="Home Office" & retail_mart_db$Market=="EU")

#6. LATAM - Latin America [Market Bucket]
ltam_consmr<- subset(retail_mart_db, retail_mart_db$Segment=="Consumer" & retail_mart_db$Market=="LATAM")
ltam_corprt<- subset(retail_mart_db, retail_mart_db$Segment=="Corporate" & retail_mart_db$Market=="LATAM")
ltam_homeof<- subset(retail_mart_db, retail_mart_db$Segment=="Home Office" & retail_mart_db$Market=="LATAM")

#7. US - United States of America [Market Bucket]
usa_consmr<- subset(retail_mart_db, retail_mart_db$Segment=="Consumer" & retail_mart_db$Market=="US")
usa_corprt<- subset(retail_mart_db, retail_mart_db$Segment=="Corporate" & retail_mart_db$Market=="US")
usa_homeof<- subset(retail_mart_db, retail_mart_db$Segment=="Home Office" & retail_mart_db$Market=="US")
#All 51,290 records from the retail_mart_db have been bucketed into the aforementioned 21 Market Buckets.

#In order to select the two most Profitable and Consistently Profitable Market Buckets we must first aggregate the Profit, Sales and Quantity attributes.
#Since our focus is on monthly forecasting we must aggregate the data such that these parameters are grouped for every month.
#The Parameters of Focus are Monthly.Profit, Monthly Sales, Monthly Demand/Quantity and Month.Code [Note: We have transformed Order Month to derive Month.Code attribute]

#The user defined parameter_aggregation function was defined to: 
#[1]Group each of the 21 Market Buckets based on Month.Code.
#[2]Aggregate the grouped market bucket based on Profit, Sales, and Quantity. We will name these parameters as Monthly.Profit, Monthly.Sales, and Monthly Demand respectively.
#[3]We have also computed the Net Profit, Average Monthly Profit and the Coefficient of Variation for Monthly.Profit for each Market Bucket.
parameter_aggregation<- function(input_df)
{
  input_df$Month.Code<- as.numeric(input_df$Month.Code)
  input_df<- input_df %>% group_by(Month.Code) %>% summarise(Monthly.Profit = sum(Profit), Monthly.Sales= sum(Sales), Monthly.Demand= sum(Quantity)) %>% 
    mutate(Net.Profit = sum(Monthly.Profit), Average.Profit = mean(Monthly.Profit), Coeff.Var= (sd(Monthly.Profit)/mean(Monthly.Profit)))
  input_df<- sapply(input_df, function(x) round(x,2))
  return(data.frame(input_df))
}

#Passing each of the 21 market buckets through the parameter_aggregation function
af_consmr<- parameter_aggregation(af_consmr)
af_corprt<- parameter_aggregation(af_corprt)
af_homeof<- parameter_aggregation(af_homeof)
apac_consmr<- parameter_aggregation(apac_consmr)
apac_corprt<- parameter_aggregation(apac_corprt)
apac_homeof<- parameter_aggregation(apac_homeof)
cnda_consmr<- parameter_aggregation(cnda_consmr)
cnda_corprt<- parameter_aggregation(cnda_corprt)
cnda_homeof<- parameter_aggregation(cnda_homeof)
emea_consmr<- parameter_aggregation(emea_consmr)
emea_corprt<- parameter_aggregation(emea_corprt)
emea_homeof<- parameter_aggregation(emea_homeof)
eu_consmr<- parameter_aggregation(eu_consmr)
eu_corprt<- parameter_aggregation(eu_corprt)
eu_homeof<- parameter_aggregation(eu_homeof)
ltam_consmr<- parameter_aggregation(ltam_consmr)
ltam_corprt<- parameter_aggregation(ltam_corprt)
ltam_homeof<- parameter_aggregation(ltam_homeof)
usa_consmr<- parameter_aggregation(usa_consmr)
usa_corprt<- parameter_aggregation(usa_corprt)
usa_homeof<- parameter_aggregation(usa_homeof)

#************************* [Iv] Identifying the Optimal Profit Market Buckets *************************#

#Each of the transformed Market Buckets Contain 7 columns:
#[1]"Month.Code" , [2]"Monthly.Profit" , [3]"Monthly.Sales" , [4]"Monthly.Demand" , [5]"Net.Profit" , [6]"Average.Profit" , [7]"Coeff.Var"
#The Net.Profit, Average.Profit and Coeff.Var [Monthly Profit] will be used to identify the Optimal Market Buckets
#We will create a comparison_df dataframe that contains the Net.Profit, Average.Profit, Coeff.Var [Monthly Profit] and the Market Bucket. 
#To find the two most profitable and consistently profitable segments we need to find two Market Buckets with the Highest Net.Profit and Average.Profit while having the least Coeff.Var [Monthly Profit].
comparison_df<-data.frame(Net.Profit=c(af_consmr[1, 5],af_corprt[1, 5],af_homeof[1, 5],apac_consmr[1, 5],apac_corprt[1, 5],
                        apac_homeof[1, 5],cnda_consmr[1, 5],cnda_corprt[1, 5],cnda_homeof[1, 5],emea_consmr[1, 5],
                        emea_corprt[1, 5],emea_homeof[1, 5],eu_consmr[1, 5],eu_corprt[1, 5],eu_homeof[1, 5],
                        ltam_consmr[1, 5],ltam_corprt[1, 5],ltam_homeof[1, 5],usa_consmr[1, 5],usa_corprt[1, 5],usa_homeof[1, 5]), 
                        
                        Average.Profit=c(af_consmr[1, 6],af_corprt[1, 6],af_homeof[1, 6],apac_consmr[1, 6],apac_corprt[1, 6],apac_homeof[1, 6],
                       cnda_consmr[1, 6],cnda_corprt[1, 6],cnda_homeof[1, 6],emea_consmr[1, 6],emea_corprt[1, 6],emea_homeof[1, 6],
                       eu_consmr[1, 6],eu_corprt[1, 6],eu_homeof[1, 6],ltam_consmr[1, 6],ltam_corprt[1, 6],ltam_homeof[1, 6],
                       usa_consmr[1, 6],usa_corprt[1, 6],usa_homeof[1, 6]), 
                       
                       Coeff.Var=c(af_consmr[1, 7],af_corprt[1, 7],af_homeof[1, 7], apac_consmr[1, 7],apac_corprt[1, 7],apac_homeof[1, 7],
                       cnda_consmr[1, 7],cnda_corprt[1, 7],cnda_homeof[1, 7],emea_consmr[1, 7],emea_corprt[1, 7],emea_homeof[1, 7],
                       eu_consmr[1, 7],eu_corprt[1, 7],eu_homeof[1, 7],ltam_consmr[1, 7],ltam_corprt[1, 7],ltam_homeof[1, 7],
                       usa_consmr[1, 7],usa_corprt[1, 7],usa_homeof[1, 7]),
           
                      Mrkt.Bucket = c("af_consmr","af_corprt","af_homeof","apac_consmr","apac_corprt","apac_homeof","cnda_consmr","cnda_corprt",
                      "cnda_homeof","emea_consmr","emea_corprt","emea_homeof","eu_consmr","eu_corprt","eu_homeof","ltam_consmr","ltam_corprt",
                      "ltam_homeof","usa_consmr","usa_corprt","usa_homeof")) %>% arrange(desc(Net.Profit, Average.Profit), Coeff.Var) 
comparison_df
#From the comparison_df there is a clear margin of clearence segregating the two most profitable and consistently profitable Market Buckets from the rest
#   Net.Profit    Average.Profit    Coeff.Var       Mrkt.Bucket
#1   222817.56        4642.03          0.63         apac_consmr
#2   188687.71        3930.99          0.62         eu_consmr

#Removing all the unnecessary Market Buckets
rm(af_consmr, af_corprt, af_homeof, apac_corprt, apac_homeof, cnda_consmr, cnda_corprt, cnda_homeof, emea_consmr, 
   emea_corprt, emea_homeof, eu_corprt, eu_homeof, ltam_consmr, ltam_corprt, ltam_homeof, usa_consmr, usa_corprt, usa_homeof)

#Therefore, we must build the Demand and Sales Forecasting Model for:
#[1] The Consumer Segment of APAC Market
#[2] The Consumer Segment of EU Market


#************************* [Iv] Building Time Series Model *************************#

#************************* [Iv].1. APAC_CONSUMER *************************#

#[1]. APAC_CONSUMER - Asia Pacific/Asia Central Consumer Market Bucket
#We will create seperate dataframe frames for Demand and Sales. 
#These dataframes will be used to build the time series forecasting model for APAC_Consumer Monthly Demand and Monthly Sales respectively.
demand.apac_consmr<- apac_consmr[,c("Month.Code","Monthly.Demand")]
sales.apac_consmr<- apac_consmr[,c("Month.Code","Monthly.Sales")]
row_count<- nrow(apac_consmr)

#***** APAC_CONSUMER Demand Modeling Section *****#
#We will build two demand forecasting models using the below techniques:
#[1] Classical Decomposition
#[2] Auto ARIMA
#We will then evaluate the two models based on MAPE metric for Forecasts on 6 month out of bag values.
#Therefore, we need to divide the demand.apac_consmr dataframe into 42 rows for model building and 6 rows for testing.
test.demand.apac_consmr<- demand.apac_consmr[(row_count-5):row_count,]
demand.apac_consmr<- demand.apac_consmr[1:(row_count-6),]

demand.apac_consmr.timeser<- ts(demand.apac_consmr[,"Monthly.Demand"])
demand.apac_consmr.timeser

#Let us begin to understand the individual components of our Demand Time Series
#We will decompose the components and view it graphically. Since decomposition cannot happen under the frequency of 1.
#And we are viewing monthly data. It is most logical to look at it in a 12 month/1 year format. So we will set the frequency to 12. And obseve the components of the decomposed timeseries
decomposed.demand.apac_consmr<- ts(demand.apac_consmr[,"Monthly.Demand"], frequency = 12)
decomposed.demand.apac_consmr<- decompose(decomposed.demand.apac_consmr)
decomposed.demand.apac_consmr
plot(decomposed.demand.apac_consmr)
#It is clear from the decomposition that the Demand:
#[1] A linearly increasing trend with a positive slope
#[2] A low amplitude sinusoidal seasonality ranging between -180 to +150 units.
#[3] The Demand Time Series will be and Additive Forecasting Model
#We will now model these components.

#Plotting the Demand Time Series
plot(demand.apac_consmr.timeser, col="red", lwd=2)

#[1] Classical Decomposition Model for Demand Forecasting APAC_Consumer

#Defining a Moving Average Smoothing Function
ts_movavg_smoother<- function(inp_timsr, width)
{
  smothed_timsr<- stats::filter(inp_timsr, filter = rep(1/(2*width+1),(2*width+1)), method = "convolution", sides = 2)
  #In smothed_timesr, the starting and ending records of lenght equal to width are missing therefore we will smooth and impute these missing values
  #Smoothing Left Half
  left_diff<- smothed_timsr[width+2] - smothed_timsr[width+1]
  for(i in seq(from=width, to=1, by=-1))
  {
    smothed_timsr[i]<- smothed_timsr[i+1] - left_diff
  }
  
  #Smoothing Right Half
  row_count<- length(smothed_timsr)
  right_diff<- smothed_timsr[row_count-width] - smothed_timsr[row_count-width-1]
  for(i in seq(from=row_count-width, to=row_count, by=1))
  {
    smothed_timsr[i]<- smothed_timsr[i-1] + right_diff
  }
  
  return(as.vector(smothed_timsr))
}

#Smoothening the demand for APAC_Consumer Demand using the aforementioned ts_movavg_smoother(input_timeseries, width)
smothed_demand.apac_consmr<- ts_movavg_smoother(demand.apac_consmr.timeser, 1)
lines(smothed_demand.apac_consmr, col="blue", lwd=2)
smothed_demand.apac_consmr<- cbind(demand.apac_consmr$Month.Code,smothed_demand.apac_consmr)
smothed_demand.apac_consmr<- data.frame(smothed_demand.apac_consmr)
colnames(smothed_demand.apac_consmr)<- c("Month.Code","Monthly.Demand")

#With the smoothed demand dataframe. Let us begin modeling the global trend aspect
lmfit_demand.apac_consmr <- lm(Monthly.Demand~ sin(0.5*Month.Code)*poly(Month.Code,1)+
                                 cos(0.1*Month.Code)*poly(Month.Code,1)+
                               tan(0.02*Month.Code), data = smothed_demand.apac_consmr)
lmfit_demand.apac_consmr

globcomp_demand.apac_consmr <- predict(lmfit_demand.apac_consmr, data=smothed_demand.apac_consmr$Month.Code)
#We will now plot the global_component globcomp_demand.apac_consmr over the smoothed Demand Time Series.
#Note the Graph Shows: Monthly Demand Time series in Red, Smoothed Monthly Demand in Blue and the Forecasted Global Component in Green
lines(globcomp_demand.apac_consmr, col="green", lwd=2)
accuracy(globcomp_demand.apac_consmr, smothed_demand.apac_consmr$Monthly.Demand)

#Now that we have modeled the Trend and Seasonality let us isolate the Local Component and analyze its ARMA components
loclcomp_demand.apac_consmr<- demand.apac_consmr.timeser - globcomp_demand.apac_consmr

#Now let's plot this local component and analyze it graphically
plot(loclcomp_demand.apac_consmr, col="red")

#Testing acf and pacf plots for this local component to check for weak stationarity
acf(loclcomp_demand.apac_consmr)
pacf(loclcomp_demand.apac_consmr)

#Let us check if this local component can be modeled as AR(p) or MA(q) series.
locfitarma_demand.apac_consmr<- auto.arima(loclcomp_demand.apac_consmr)
locfitarma_demand.apac_consmr
#ARIMA(0,0,0) with zero mean ; This implies that there is no AR(p) or MA(q) series in the local component

#We will test the residual for white noise
resi_clasdec_demand.apac_consmr<- loclcomp_demand.apac_consmr - fitted(locfitarma_demand.apac_consmr)

#Plotting the Residual
plot(resi_clasdec_demand.apac_consmr, col="red")

#Testing the residual for White Noise
adf.test(resi_clasdec_demand.apac_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the global and local components for demand is stationairy.

kpss.test(resi_clasdec_demand.apac_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for demand is stationairy.

#Let us forecast the Demand using classical decomposition model for next 6 months [Month Code: 43,44,45,46,47,48].
fcast_clasdec_demand.apac_consmr<- predict.lm(lmfit_demand.apac_consmr, data.frame(Month.Code=test.demand.apac_consmr$Month.Code))
fcast_clasdec_demand.apac_consmr

#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.apac_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_clasdec_demand.apac_consmr<- accuracy(fcast_clasdec_demand.apac_consmr, test.demand.apac_consmr$Monthly.Demand)
acurcy_clasdec_demand.apac_consmr
mape_clasdec_demand.apac_consmr<-acurcy_clasdec_demand.apac_consmr[5]
mape_clasdec_demand.apac_consmr
#We Obtained the following results on the Demand forecasting accuracy of Classical Decomposition model:
#             ME       RMSE      MAE        MPE       MAPE
#Test set -23.16084  127.4945  107.1754  -8.815864   18.79196

#Finally lets plot the Classical Decomposition Demand forecast along with the Actual Demand for APAC_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Classical Decomposition Demand in Blue.
full_clasdec_demand.apac_consmr<- ts(c(globcomp_demand.apac_consmr, fcast_clasdec_demand.apac_consmr))
plot(ts(apac_consmr[,"Monthly.Demand"]), col="red", lwd=2)
lines(full_clasdec_demand.apac_consmr, col="blue", lwd=2)

#[2] Trying the Auto Arima Model 
atoarima_demand.apac_consmr<- auto.arima(demand.apac_consmr.timeser)
atoarima_demand.apac_consmr
#The auto arima model was of ARIMA(0,1,0) ; This implies that 1 stage differencing was performed.
#sigma^2 estimated as 25366  |  log likelihood=-266.07 |  AIC=534.14  |  AICc=534.24  |  BIC=535.85
tsdiag(atoarima_demand.apac_consmr)
#We will now plot the Original Demand Time Series in Red and the Fitted Auto Arima Model in Blue.
plot(atoarima_demand.apac_consmr$x, col="red", lwd=2)
lines(fitted(atoarima_demand.apac_consmr), col="blue", lwd=2)
#The Demand Auto Arima Model shows a distinct lag/offeset to the right. Implying that it reacts later to fluctuations in Demand.

#Let us test the residual values after the removing the fitted auto arima model for Demand.
#Ideally these residual values must represent white noise.
resi_atoarima_demand.apac_consmr <- demand.apac_consmr.timeser - fitted(atoarima_demand.apac_consmr)
adf.test(resi_atoarima_demand.apac_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the fitted auto arima for demand is stationairy.
kpss.test(resi_atoarima_demand.apac_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the fitted auto arima for demand is stationairy.

#Let us forecast the Demand using for atoarima_demand.apac_consmr next 6 months [Month Code: 43,44,45,46,47,48].
fcast_atoarima_demand.apac_consmr<- predict(atoarima_demand.apac_consmr, n.ahead = 6)
fcast_atoarima_demand.apac_consmr
#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.apac_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_atoarima_demand.apac_consmr<- accuracy(fcast_atoarima_demand.apac_consmr$pred, test.demand.apac_consmr$Monthly.Demand)
acurcy_atoarima_demand.apac_consmr
mape_atoarima_demand.apac_consmr<-acurcy_atoarima_demand.apac_consmr[5]
mape_atoarima_demand.apac_consmr
#We Obtained the following results on the Demand forecasting accuracy of Auto ARIMA model:
#         ME   RMSE     MAE       MPE     MAPE
#Test set 12 174.3722 147.6667 -7.362467 26.24458

#Finally lets plot the Auto Arima forecast along with the Actual Demand for APAC_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Auto Arima Model in Blue.
full_atoarima_demand.apac_consmr<- ts(c(fitted(atoarima_demand.apac_consmr), fcast_atoarima_demand.apac_consmr$pred))
plot(ts(apac_consmr[,"Monthly.Demand"]), col="red", lwd=2)
lines(full_atoarima_demand.apac_consmr, col="blue", lwd=2)


#***** APAC_CONSUMER Sales Modeling Section *****#
#We will build two sales forecasting models using the below techniques:
#[1] Classical Decomposition
#[2] Auto ARIMA
#We will then evaluate the two models based on MAPE metric for Forecasts on 6 month out of bag values.
#Therefore, we need to divide the sales.apac_consmr dataframe into 42 rows for model building and 6 rows for testing.
test.sales.apac_consmr<- sales.apac_consmr[(row_count-5):row_count,]
sales.apac_consmr<- sales.apac_consmr[1:(row_count-6),]

sales.apac_consmr.timeser<- ts(sales.apac_consmr[,"Monthly.Sales"])
sales.apac_consmr.timeser

#Let us begin to understand the individual components of our Sales Time Series
#We will decompose the components and view it graphically. Since decomposition cannot happen under the frequency of 1.
#And we are viewing monthly data. It is most logical to look at it in a 12 month/1 year format. So we will set the frequency to 12. And obseve the components of the decomposed timeseries
decomposed.sales.apac_consmr<- ts(sales.apac_consmr[,"Monthly.Sales"], frequency = 12)
decomposed.sales.apac_consmr<- decompose(decomposed.sales.apac_consmr)
decomposed.sales.apac_consmr
plot(decomposed.sales.apac_consmr)
#It is clear from the decomposition that the Demand:
#[1] A linearly increasing trend with a positive slope
#[2] A high amplitude sinusoidal seasonality ranging between -15000 to +15000 units.
#[3] The Sales Time Series will be and Additive Forecasting Model
#We will now model these components.

#Plotting the Sales Time Series
plot(sales.apac_consmr.timeser, col="red", lwd=2)

#[1] Classical Decomposition Model of APAC Consumer Sales
#Smoothening the monthly sales for APAC_Consumer Sales using the aforementioned ts_movavg_smoother(input_timeseries, width)
smothed_sales.apac_consmr<- ts_movavg_smoother(sales.apac_consmr.timeser, 1)
lines(smothed_sales.apac_consmr, col="blue", lwd=2)
smothed_sales.apac_consmr<- cbind(sales.apac_consmr$Month.Code,smothed_sales.apac_consmr)
smothed_sales.apac_consmr<- data.frame(smothed_sales.apac_consmr)
colnames(smothed_sales.apac_consmr)<- c("Month.Code","Monthly.Sales")

#With the smoothed sales dataframe. Let us begin modeling the global trend aspect
lmfit_sales.apac_consmr <- lm(Monthly.Sales~ sin(0.5*Month.Code)*poly(Month.Code,1)+
                                 cos(0.05*Month.Code)*poly(Month.Code,1)+
                                 tan(0.02*Month.Code), data = smothed_sales.apac_consmr)
lmfit_sales.apac_consmr

globcomp_sales.apac_consmr <- predict(lmfit_sales.apac_consmr, data=smothed_sales.apac_consmr$Month.Code)
#We will now plot the global_component globcomp_sales.apac_consmr over the smoothed Sales Time Series.
#Note the Graph Shows: Monthly Sales Time series in Red, Smoothed Monthly Sales in Blue and the Forecasted Global Component in Green
lines(globcomp_sales.apac_consmr, col="green", lwd=2)
accuracy(globcomp_sales.apac_consmr, smothed_sales.apac_consmr$Monthly.Sales)

#Now that we have modeled the Trend and Seasonality let us isolate the Local Component and analyze its ARMA components
loclcomp_sales.apac_consmr<- sales.apac_consmr.timeser - globcomp_sales.apac_consmr

#Now let's plot this local component and analyze it graphically
plot(loclcomp_sales.apac_consmr, col="red")

#Testing acf and pacf plots for this local component to check for weak stationarity
acf(loclcomp_sales.apac_consmr)
pacf(loclcomp_sales.apac_consmr)

#Let us check if this local component can be modeled as AR(p) or MA(q) series.
locfitarma_sales.apac_consmr<- auto.arima(loclcomp_sales.apac_consmr)
locfitarma_sales.apac_consmr
#ARIMA(0,0,0) with zero mean ; This implies that there is no AR(p) or MA(q) series in the local component

#We will test the residual for white noise
resi_clasdec_sales.apac_consmr<- loclcomp_sales.apac_consmr - fitted(locfitarma_sales.apac_consmr)

#Plotting the Residual
plot(resi_clasdec_sales.apac_consmr, col="red")

#Testing the residual for White Noise
adf.test(resi_clasdec_sales.apac_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the global and local components for sales is stationairy.

kpss.test(resi_clasdec_sales.apac_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for sales is stationairy.

#Let us forecast the Sales using classical decomposition model for next 6 months [Month Code: 43,44,45,46,47,48].
fcast_clasdec_sales.apac_consmr<- predict.lm(lmfit_sales.apac_consmr, data.frame(Month.Code=test.sales.apac_consmr$Month.Code))
fcast_clasdec_sales.apac_consmr

#We will compare the aforementioned forecasted values with Actual Sales from the test.sales.apac_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Sales vs Actual Monthly Sales of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_clasdec_sales.apac_consmr<- accuracy(fcast_clasdec_sales.apac_consmr, test.sales.apac_consmr$Monthly.Sales)
acurcy_clasdec_sales.apac_consmr
mape_clasdec_sales.apac_consmr<-acurcy_clasdec_sales.apac_consmr[5]
mape_clasdec_sales.apac_consmr
#We Obtained the following results on the Sales forecasting accuracy of Classical Decomposition model:
#             ME       RMSE      MAE        MPE       MAPE
#Test set  6515.587  14997.64  12937.14   5.024684  20.83351

#Finally lets plot the Classical Decomposition Sales forecast along with the Actual Sales for APAC_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Classical Decomposition Demand in Blue.
full_clasdec_sales.apac_consmr<- ts(c(globcomp_sales.apac_consmr, fcast_clasdec_sales.apac_consmr))
plot(ts(apac_consmr[,"Monthly.Sales"]), col="red", lwd=2)
lines(full_clasdec_sales.apac_consmr, col="blue", lwd=2)

#[2] Trying the Auto Arima Model 
atoarima_sales.apac_consmr<- auto.arima(sales.apac_consmr.timeser)
atoarima_sales.apac_consmr
#The auto arima model was of ARIMA(0,1,1)  
#This implies that 1 stage differencing was performed and the resulting timeseries was modeled as MA(1).
#The standard error for the MA(1) coefficient is 18.3% of the coefficient. This is relatively high but we will accept it with caution 
#sigma^2 estimated as 174361546  |  log likelihood=-447.11  |  AIC=898.23  |  AICc=898.55  |  BIC=901.66
tsdiag(atoarima_sales.apac_consmr)
#We will now plot the Original Sales Time Series in Red and the Fitted Auto Arima Model in Blue.
plot(atoarima_sales.apac_consmr$x, col="red", lwd=2)
lines(fitted(atoarima_sales.apac_consmr), col="blue", lwd=2)
#The Sales Auto Arima Model shows a poor fit when overlaid on the Actual Sales.
#This implies that the sales forecasting model is not reponding to fluctuations in the Actual Sales on both the high and low end.
#Let us test the residual values after the removing the fitted auto arima model for Sales.
#Ideally these residual values must represent white noise.

resi_atoarima_sales.apac_consmr <- sales.apac_consmr.timeser - fitted(atoarima_sales.apac_consmr)
adf.test(resi_atoarima_sales.apac_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the fitted auto arima for sales is stationairy.
kpss.test(resi_atoarima_sales.apac_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the fitted auto arima for sales is stationairy.

#Let us forecast the sales using for atoarima_sales.apac_consmr next 6 months [Month Code: 43,44,45,46,47,48].
fcast_atoarima_sales.apac_consmr<- predict(atoarima_sales.apac_consmr, n.ahead = 6)
fcast_atoarima_sales.apac_consmr

#We will compare the aforementioned forecasted values with Actual sales from the test.sales.apac_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Sales vs Actual Monthly Sales of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_atoarima_sales.apac_consmr<- accuracy(fcast_atoarima_sales.apac_consmr$pred, test.sales.apac_consmr$Monthly.Sales)
acurcy_atoarima_sales.apac_consmr
mape_atoarima_sales.apac_consmr<-acurcy_atoarima_sales.apac_consmr[5]
mape_atoarima_sales.apac_consmr
#We Obtained the following results on the Sales forecasting accuracy of Auto ARIMA model:
#             ME        RMSE      MAE       MPE       MAPE
#Test set   15848.24  22755.75  18780.19  19.73091  27.68952

#Finally lets plot the Auto Arima forecast along with the Actual Sales for APAC_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Sales Time Series in Red and the Fitted Auto Arima Model in Blue.
full_atoarima_sales.apac_consmr<- ts(c(fitted(atoarima_sales.apac_consmr), fcast_atoarima_sales.apac_consmr$pred))
plot(ts(apac_consmr[,"Monthly.Sales"]), col="red", lwd=2)
lines(full_atoarima_sales.apac_consmr, col="blue", lwd=2)

#************************* [Iv].2. EU_CONSUMER *************************#

#EU_CONSUMER - European Union Consumer Market Bucket
#We will create seperate dataframe frames for Demand and Sales. 
#These dataframes will be used to build the time series forecasting model for EU_Consumer Monthly Demand and Monthly Sales respectively.
demand.eu_consmr<- eu_consmr[,c("Month.Code","Monthly.Demand")]
sales.eu_consmr<- eu_consmr[,c("Month.Code","Monthly.Sales")]
row_count<- nrow(eu_consmr)

#***** EU_CONSUMER Demand Modeling Section *****#
#We will build two demand forecasting models using the below techniques:
#[1] Classical Decomposition
#[2] Auto ARIMA
#We will then evaluate the two models based on MAPE metric for Forecasts on 6 month out of bag values.
#Therefore, we need to divide the demand.eu_consmr dataframe into 42 rows for model building and 6 rows for testing.
test.demand.eu_consmr<- demand.eu_consmr[(row_count-5):row_count,]
demand.eu_consmr<- demand.eu_consmr[1:(row_count-6),]

demand.eu_consmr.timeser<- ts(demand.eu_consmr[,"Monthly.Demand"])
demand.eu_consmr.timeser

#Let us begin to understand the individual components of our Demand Time Series
#We will decompose the components and view it graphically. Since decomposition cannot happen under the frequency of 1.
#And we are viewing monthly data. It is most logical to look at it in a 12 month/1 year format. So we will set the frequency to 12. And obseve the components of the decomposed timeseries
decomposed.demand.eu_consmr<- ts(demand.eu_consmr[,"Monthly.Demand"], frequency = 12)
decomposed.demand.eu_consmr<- decompose(decomposed.demand.eu_consmr)
decomposed.demand.eu_consmr
plot(decomposed.demand.eu_consmr)
#It is clear from the decomposition that the Demand:
#[1] A linearly increasing trend with a positive slope
#[2] A low amplitude sinusoidal seasonality ranging between -150 to +200 units.
#[3] The Demand Time Series will be and Additive Forecasting Model
#We will now model these components.

#Plotting the Demand Time Series
plot(demand.eu_consmr.timeser, col="red", lwd=2)

#Classical Decomposition model for EU Consumer Demand
#Smoothening the demand for EU_Consumer Demand using the aforementioned ts_movavg_smoother(input_timeseries, width)
smothed_demand.eu_consmr<- ts_movavg_smoother(demand.eu_consmr.timeser, 1)
lines(smothed_demand.eu_consmr, col="blue", lwd=2)
smothed_demand.eu_consmr<- cbind(demand.eu_consmr$Month.Code,smothed_demand.eu_consmr)
smothed_demand.eu_consmr<- data.frame(smothed_demand.eu_consmr)
colnames(smothed_demand.eu_consmr)<- c("Month.Code","Monthly.Demand")

#With the smoothed demand dataframe. Let us begin modeling the global trend aspect
lmfit_demand.eu_consmr <- lm(Monthly.Demand~ sin(0.5*Month.Code)*poly(Month.Code,1)+
                                 cos(0.09*Month.Code)*poly(Month.Code,1)+
                                 tan(0.02*Month.Code), data = smothed_demand.eu_consmr)
lmfit_demand.eu_consmr

globcomp_demand.eu_consmr <- predict(lmfit_demand.eu_consmr, data=smothed_demand.eu_consmr$Month.Code)
#We will now plot the global_component globcomp_demand.eu_consmr over the smoothed Demand Time Series.
#Note the Graph Shows: Monthly Demand Time series in Red, Smoothed Monthly Demand in Blue and the Forecasted Global Component in Green
lines(globcomp_demand.eu_consmr, col="green", lwd=2)
accuracy(globcomp_demand.eu_consmr, smothed_demand.eu_consmr$Monthly.Demand)

#Now that we have modeled the Trend and Seasonality let us isolate the Local Component and analyze its ARMA components
loclcomp_demand.eu_consmr<- demand.eu_consmr.timeser - globcomp_demand.eu_consmr

#Now let's plot this local component and analyze it graphically
plot(loclcomp_demand.eu_consmr, col="red")

#Testing acf and pacf plots for this local component to check for weak stationarity
acf(loclcomp_demand.eu_consmr)
pacf(loclcomp_demand.eu_consmr)

#Let us check if this local component can be modeled as AR(p) or MA(q) series.
locfitarma_demand.eu_consmr<- auto.arima(loclcomp_demand.eu_consmr)
locfitarma_demand.eu_consmr
#ARIMA(0,0,0) with zero mean; This implies that there is no AR(p) or MA(q) series in the local component
#sigma^2 estimated as 15119  |  log likelihood=-261.69  |  AIC=525.39  |  AICc=525.49  |  BIC=527.13

#We will test the residual for white noise
resi_clasdec_demand.eu_consmr<- loclcomp_demand.eu_consmr - fitted(locfitarma_demand.eu_consmr)

#Plotting the Residual
plot(resi_clasdec_demand.eu_consmr, col="red")

#Testing the residual for White Noise
adf.test(resi_clasdec_demand.eu_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value=0.02453. 
#This Implies that the residual after extraxting the global and local components for demand is stationairy.

kpss.test(resi_clasdec_demand.eu_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for demand is stationairy.

#Let us forecast the Demand using classical decomposition model for next 6 months [Month Code: 43,44,45,46,47,48].
fcast_clasdec_demand.eu_consmr<- predict.lm(lmfit_demand.eu_consmr, data.frame(Month.Code=test.demand.eu_consmr$Month.Code))
fcast_clasdec_demand.eu_consmr
#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.eu_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_clasdec_demand.eu_consmr<- accuracy(fcast_clasdec_demand.eu_consmr, test.demand.eu_consmr$Monthly.Demand)
acurcy_clasdec_demand.eu_consmr
mape_clasdec_demand.eu_consmr<-acurcy_clasdec_demand.eu_consmr[5]
mape_clasdec_demand.eu_consmr
#We Obtained the following results on the Demand forecasting accuracy of Classical Decomposition model:
#            ME        RMSE      MAE       MPE       MAPE
#Test set -19.25783  189.3226  127.333  -10.38272  21.98432

#Finally lets plot the Classical Decomposition Demand forecast along with the Actual Demand for EU_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Classical Decomposition Demand in Blue.
full_clasdec_demand.eu_consmr<- ts(c(globcomp_demand.eu_consmr, fcast_clasdec_demand.eu_consmr))
plot(ts(eu_consmr[,"Monthly.Demand"]), col="red", lwd=2)
lines(full_clasdec_demand.eu_consmr, col="blue", lwd=2)

#[2] Trying the Auto Arima Model 
atoarima_demand.eu_consmr<- auto.arima(demand.eu_consmr.timeser)
atoarima_demand.eu_consmr
#The auto arima model was of ARIMA(2,1,0) ; This implies that 1 stage differencing was performed.
#Following which it was modeled as an AR(2) model.
#The coefficients of the AR(2) model have a standard error of 16.16% and 20.2% of their respective coefficients. These values are relatively high but we will accept it with caution.
#sigma^2 estimated as 21185  |  log likelihood=-261.9  |  AIC=529.8  |  AICc=530.44  |  BIC=534.94
tsdiag(atoarima_demand.eu_consmr)
#We will now plot the Original Demand Time Series in Red and the Fitted Auto Arima Model in Blue.
plot(atoarima_demand.eu_consmr$x, col="red", lwd=2)
lines(fitted(atoarima_demand.eu_consmr), col="blue", lwd=2)
#The Demand Auto Arima Model shows a slight lag/offeset to the right in response to fluctuations in the Actual Demand

#Let us test the residual values after the removing the fitted auto arima model for Demand.
#Ideally these residual values must represent white noise.
resi_atoarima_demand.eu_consmr <- demand.eu_consmr.timeser - fitted(atoarima_demand.eu_consmr)
adf.test(resi_atoarima_demand.eu_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value=0.04521. 
#This Implies that the residual after extraxting the fitted auto arima for demand is stationairy.
kpss.test(resi_atoarima_demand.eu_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the fitted auto arima for demand is stationairy.

#Let us forecast the Demand using for atoarima_demand.eu_consmr next 6 months [Month Code: 43,44,45,46,47,48].
fcast_atoarima_demand.eu_consmr<- predict(atoarima_demand.eu_consmr, n.ahead = 6)
fcast_atoarima_demand.eu_consmr

#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.eu_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_atoarima_demand.eu_consmr<- accuracy(fcast_atoarima_demand.eu_consmr$pred, test.demand.eu_consmr$Monthly.Demand)
acurcy_atoarima_demand.eu_consmr
mape_atoarima_demand.eu_consmr<-acurcy_atoarima_demand.eu_consmr[5]
mape_atoarima_demand.eu_consmr
#We Obtained the following results on the Demand forecasting accuracy of Auto ARIMA model:
#            ME       RMSE     MAE       MPE       MAPE
#Test set 242.746  316.7626  253.8108  27.53891  30.13319

#Finally lets plot the Auto Arima forecast along with the Actual Demand for EU_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Auto Arima Model in Blue.
full_atoarima_demand.eu_consmr<- ts(c(fitted(atoarima_demand.eu_consmr), fcast_atoarima_demand.eu_consmr$pred))
plot(ts(eu_consmr[,"Monthly.Demand"]), col="red", lwd=2)
lines(full_atoarima_demand.eu_consmr, col="blue", lwd=2)

#***** EU_CONSUMER Sales Modeling Section *****#
#We will build two sales forecasting models using the below techniques:
#[1] Classical Decomposition
#[2] Auto ARIMA
#We will then evaluate the two models based on MAPE metric for Forecasts on 6 month out of bag values.
#Therefore, we need to divide the sales.eu_consmr dataframe into 42 rows for model building and 6 rows for testing.
test.sales.eu_consmr<- sales.eu_consmr[(row_count-5):row_count,]
sales.eu_consmr<- sales.eu_consmr[1:(row_count-6),]

sales.eu_consmr.timeser<- ts(sales.eu_consmr[,"Monthly.Sales"])
sales.eu_consmr.timeser

#Let us begin to understand the individual components of our Sales Time Series
#We will decompose the components and view it graphically. Since decomposition cannot happen under the frequency of 1.
#And we are viewing monthly data. It is most logical to look at it in a 12 month/1 year format. So we will set the frequency to 12. And obseve the components of the decomposed timeseries
decomposed.sales.eu_consmr<- ts(sales.eu_consmr[,"Monthly.Sales"], frequency = 12)
decomposed.sales.eu_consmr<- decompose(decomposed.sales.eu_consmr)
decomposed.sales.eu_consmr
plot(decomposed.sales.eu_consmr)
#It is clear from the decomposition that the Demand:
#[1] A linearly increasing trend with a positive slope
#[2] A high amplitude sinusoidal seasonality ranging between -15000 to +10000 units.
#[3] The Sales Time Series will be and Additive Forecasting Model
#We will now model these components.

#Plotting the Sales Time Series
plot(sales.eu_consmr.timeser, col="red", lwd=2)

#Classical Decomposition model for EU Consumer Sales
#Smoothening the monthly sales for EU_Consumer Sales using the aforementioned ts_movavg_smoother(input_timeseries, width)
smothed_sales.eu_consmr<- ts_movavg_smoother(sales.eu_consmr.timeser, 1)
lines(smothed_sales.eu_consmr, col="blue", lwd=2)
smothed_sales.eu_consmr<- cbind(sales.eu_consmr$Month.Code,smothed_sales.eu_consmr)
smothed_sales.eu_consmr<- data.frame(smothed_sales.eu_consmr)
colnames(smothed_sales.eu_consmr)<- c("Month.Code","Monthly.Sales")

#With the smoothed sales dataframe. Let us begin modeling the global trend aspect
lmfit_sales.eu_consmr <- lm(Monthly.Sales~ sin(0.4*Month.Code)*poly(Month.Code,1)+
                                cos(0.09*Month.Code)*poly(Month.Code,1), data = smothed_sales.eu_consmr)
lmfit_sales.eu_consmr

#Checking global Component
globcomp_sales.eu_consmr <- predict(lmfit_sales.eu_consmr, data=smothed_sales.eu_consmr$Month.Code)
#We will now plot the global_component globcomp_sales.eu_consmr over the smoothed Sales Time Series.
#Note the Graph Shows: Monthly Sales Time series in Red, Smoothed Monthly Sales in Blue and the Forecasted Global Component in Green
lines(globcomp_sales.eu_consmr, col="green", lwd=2)
accuracy(globcomp_sales.eu_consmr, smothed_sales.eu_consmr$Monthly.Sales)

#Now that we have modeled the Trend and Seasonality let us isolate the Local Component and analyze its ARMA components
loclcomp_sales.eu_consmr<- sales.eu_consmr.timeser - globcomp_sales.eu_consmr

#Now let's plot this local component and analyze it graphically
plot(loclcomp_sales.eu_consmr, col="red")

#Testing acf and pacf plots for this local component to check for weak stationarity
acf(loclcomp_sales.eu_consmr)
pacf(loclcomp_sales.eu_consmr)

#Let us check if this local component can be modeled as AR(p) or MA(q) series.
locfitarma_sales.eu_consmr<- auto.arima(loclcomp_sales.eu_consmr)
locfitarma_sales.eu_consmr
#ARIMA(0,0,0) with zero mean ; This implies that there is no AR(p) or MA(q) series in the local component

#We will test the residual for white noise
resi_clasdec_sales.eu_consmr<- loclcomp_sales.eu_consmr - fitted(locfitarma_sales.eu_consmr)

#Plotting the Residual
plot(resi_clasdec_sales.eu_consmr, col="red")

#Testing the residual for White Noise
adf.test(resi_clasdec_sales.eu_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value=0.01311. 
#This Implies that the residual after extraxting the global and local components for sales is stationairy.

kpss.test(resi_clasdec_sales.eu_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for sales is stationairy.

#Let us forecast the Sales using classical decomposition model for next 6 months [Month Code: 43,44,45,46,47,48].
fcast_clasdec_sales.eu_consmr<- predict.lm(lmfit_sales.eu_consmr, data.frame(Month.Code=test.sales.eu_consmr$Month.Code))
fcast_clasdec_sales.eu_consmr
#We will compare the aforementioned forecasted values with Actual Sales from the test.sales.eu_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Sales vs Actual Monthly Sales of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_clasdec_sales.eu_consmr<- accuracy(fcast_clasdec_sales.eu_consmr, test.sales.eu_consmr$Monthly.Sales)
acurcy_clasdec_sales.eu_consmr
mape_clasdec_sales.eu_consmr<-acurcy_clasdec_sales.eu_consmr[5]
mape_clasdec_sales.eu_consmr
#We Obtained the following results on the Sales forecasting accuracy of Classical Decomposition model:
#             ME       RMSE      MAE        MPE       MAPE
#Test set 4172.431  13736.59  10622.04   2.259534   20.73958

#Finally lets plot the Classical Decomposition Sales forecast along with the Actual Sales for EU_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Classical Decomposition Demand in Blue.
full_clasdec_sales.eu_consmr<- ts(c(globcomp_sales.eu_consmr, fcast_clasdec_sales.eu_consmr))
plot(ts(eu_consmr[,"Monthly.Sales"]), col="red", lwd=2)
lines(full_clasdec_sales.eu_consmr, col="blue", lwd=2)

#[2] Trying the Auto Arima Model 
atoarima_sales.eu_consmr<- auto.arima(sales.eu_consmr.timeser)
atoarima_sales.eu_consmr
#The auto arima model was of ARIMA(2,1,0)  
#This implies that 1 stage differencing was performed and the resulting timeseries was modeled as AR(2).
#The standard error for the AR(2) coefficient is 23.2% and 26.7% of the coefficient. This is relatively high but we will accept it with caution 
#sigma^2 estimated as 168564657  |  log likelihood=-445.84  |  AIC=897.67  |  AICc=898.32  |  BIC=902.81
tsdiag(atoarima_sales.eu_consmr)
#We will now plot the Original Sales Time Series in Red and the Fitted Auto Arima Model in Blue.
plot(atoarima_sales.eu_consmr$x, col="red", lwd=2)
lines(fitted(atoarima_sales.eu_consmr), col="blue", lwd=2)
#The Sales Auto Arima Model shows a distinct lag/offset to the right when overlaid on the Actual Sales.
#This implies that the sales forecasting model has a delayed response to fluctuations in the Actual Sales on both the high and low end.
#Let us test the residual values after the removing the fitted auto arima model for Sales.
#Ideally these residual values must represent white noise.

resi_atoarima_sales.eu_consmr <- sales.eu_consmr.timeser - fitted(atoarima_sales.eu_consmr)
adf.test(resi_atoarima_sales.eu_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the fitted auto arima for sales is stationairy.
kpss.test(resi_atoarima_sales.eu_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the fitted auto arima for sales is stationairy.

#Let us forecast the Sales using for atoarima_sales.eu_consmr next 6 months [Month Code: 43,44,45,46,47,48].
fcast_atoarima_sales.eu_consmr<- predict(atoarima_sales.eu_consmr, n.ahead = 6)
fcast_atoarima_sales.eu_consmr

#We will compare the aforementioned forecasted values with Actual sales from the test.sales.eu_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Sales vs Actual Monthly Sales of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_atoarima_sales.eu_consmr<- accuracy(fcast_atoarima_sales.eu_consmr$pred, test.sales.eu_consmr$Monthly.Sales)
acurcy_atoarima_sales.eu_consmr
mape_atoarima_sales.eu_consmr<-acurcy_atoarima_sales.eu_consmr[5]
mape_atoarima_sales.eu_consmr
#We Obtained the following results on the Sales forecasting accuracy of Auto ARIMA model:
#             ME      RMSE      MAE       MPE       MAPE
#Test set  12935.2  19499.13   16687.6   17.678   28.9226

#Finally lets plot the Auto Arima forecast along with the Actual Sales for EU_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Sales Time Series in Red and the Fitted Auto Arima Model in Blue.
full_atoarima_sales.eu_consmr<- ts(c(fitted(atoarima_sales.eu_consmr), fcast_atoarima_sales.eu_consmr$pred))
plot(ts(eu_consmr[,"Monthly.Sales"]), col="red", lwd=2)
lines(full_atoarima_sales.eu_consmr, col="blue", lwd=2)

#***************** [VI] Model Evaluation[MAPE], Model Selection, Forecasting Demand and Sales for the future 6 month Period. **********************#

#We have created the Classical Decomposition and Auto ARIMA models to forecast Demand and Sales of our most Profitable Market Buckets
#We will compare the model evaluations and select the best model based on the Accuracy and MAPE parameters of out of bag test values
#The testdata set contained month codes 43-48. Individual Model Performance is as Shown Below.
future_period<- c(49:54)
future_month<- c("Jan2015", "Feb2015", "Mar2015", "Apr2015", "May2015", "June2015")

#**************** APAC - Asia Pacific/Asia Central-Consumer[Market Buckets] ****************#

#[1]. APAC-CONSUMER MARKET BUCKET [DEMAND]
#A. Classical Decomposition Model Evaluation Result
acurcy_clasdec_demand.apac_consmr
mape_clasdec_demand.apac_consmr
#             ME       RMSE      MAE        MPE       MAPE
#Test set -23.16084  127.4945  107.1754  -8.815864   18.79196
#MAPE=18.79%

#B. Auto ARIMA Model Evaluation Results
acurcy_atoarima_demand.apac_consmr
mape_atoarima_demand.apac_consmr
#         ME   RMSE     MAE       MPE     MAPE
#Test set 12 174.3722 147.6667 -7.362467 26.24458
#MAPE=26.24%

#APAC-CONSUMER MARKET BUCKET [DEMAND] Forecasting Model Evaluation Report
#From the above results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 7.45% on the Auto Arima model. 
#Therefore the Classical Decomposition Model will be used to forecast the demand for the future 6 month Period.
futur_fcast_demand.apac_consmr<- predict.lm(lmfit_demand.apac_consmr, data.frame(Month.Code=future_period))
futur_fcast_demand.apac_consmr<- as.vector(futur_fcast_demand.apac_consmr)
futur_fcast_demand.apac_consmr<- data.frame(future_month, futur_fcast_demand.apac_consmr)
colnames(futur_fcast_demand.apac_consmr)<- c("Future Month", "Demand Forecast")
#APAC Consumer Forecasted Demand is As follows
futur_fcast_demand.apac_consmr

#[2]. APAC-CONSUMER MARKET BUCKET [SALES]
#A. Classical Decomposition Model Evaluation Result
acurcy_clasdec_sales.apac_consmr
mape_clasdec_sales.apac_consmr
#             ME       RMSE      MAE        MPE       MAPE
#Test set  6515.587  14997.64  12937.14   5.024684  20.83351
#MAPE=20.83%


#B. Auto ARIMA Model Evaluation Results
acurcy_atoarima_sales.apac_consmr
mape_atoarima_sales.apac_consmr
#             ME        RMSE      MAE       MPE       MAPE
#Test set   15848.24  22755.75  18780.19  19.73091  27.68952
#MAPE=27.68%

#APAC-CONSUMER MARKET BUCKET [SALES] Forecasting Model Evaluation Report
#From the above results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 6.86% on the Auto Arima model. 
#Therefore the Classical Decomposition Model will be used to forecast the demand for the future 6 month Period.
futur_fcast_sales.apac_consmr<- predict.lm(lmfit_sales.apac_consmr, data.frame(Month.Code=future_period))
futur_fcast_sales.apac_consmr<- as.vector(futur_fcast_sales.apac_consmr)
futur_fcast_sales.apac_consmr<- data.frame(future_month, futur_fcast_sales.apac_consmr)
colnames(futur_fcast_sales.apac_consmr)<- c("Future Month", "Sales Forecast")
#APAC Consumer Forecasted Sales is As follows
futur_fcast_sales.apac_consmr


#**************** EU - European Union-Consumer [Market Bucket] ****************#

#[3]. EU-CONSUMER MARKET BUCKET [DEMAND]
#A. Classical Decomposition Model Evaluation Result
acurcy_clasdec_demand.eu_consmr
mape_clasdec_demand.eu_consmr
#            ME        RMSE      MAE       MPE       MAPE
#Test set -19.25783  189.3226  127.333  -10.38272  21.98432
#MAPE=21.98%

#B. Auto ARIMA Model Evaluation Results
acurcy_atoarima_demand.eu_consmr
mape_atoarima_demand.eu_consmr
#            ME       RMSE     MAE       MPE       MAPE
#Test set 242.746  316.7626  253.8108  27.53891  30.13319
#MAPE=30.13%

#EU-CONSUMER MARKET BUCKET [DEMAND] Forecasting Model Evaluation Report
#From the above results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 8.15% on the Auto Arima model. 
#Therefore the Classical Decomposition Model will be used to forecast the demand for the future 6 month Period
futur_fcast_demand.eu_consmr<- predict.lm(lmfit_demand.eu_consmr, data.frame(Month.Code=future_period))
futur_fcast_demand.eu_consmr<- as.vector(futur_fcast_demand.eu_consmr)
futur_fcast_demand.eu_consmr<- data.frame(future_month, futur_fcast_demand.eu_consmr)
colnames(futur_fcast_demand.eu_consmr)<- c("Future Month", "Demand Forecast")
#EU Consumer Forecasted Demand is As follows
futur_fcast_demand.eu_consmr

#[2]. EU-CONSUMER MARKET BUCKET [SALES]
#A. Classical Decomposition Model Evaluation Result
acurcy_clasdec_sales.eu_consmr
mape_clasdec_sales.eu_consmr
#             ME       RMSE      MAE        MPE       MAPE
#Test set 4172.431  13736.59  10622.04   2.259534   20.73958
#MAPE=20.73%

#B. Auto ARIMA Model Evaluation Results
acurcy_atoarima_sales.eu_consmr
mape_atoarima_sales.eu_consmr
#             ME      RMSE      MAE       MPE       MAPE
#Test set  12935.2  19499.13   16687.6   17.678   28.9226
#MAPE=28.92%

#EU-CONSUMER MARKET BUCKET [SALES] Forecasting Model Evaluation Report
#From the above results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 8.19% on the Auto Arima model. 
#Therefore the Classical Decomposition Model will be used to forecast the demand for the future 6 month Period.
futur_fcast_sales.eu_consmr<- predict.lm(lmfit_sales.eu_consmr, data.frame(Month.Code=future_period))
futur_fcast_sales.eu_consmr<- as.vector(futur_fcast_sales.eu_consmr)
futur_fcast_sales.eu_consmr<- data.frame(future_month, futur_fcast_sales.eu_consmr)
colnames(futur_fcast_sales.eu_consmr)<- c("Future Month", "Sales Forecast")
#APAC Consumer Forecasted Sales is As follows
futur_fcast_sales.eu_consmr

#************************************ End Notes **********************************#
#We have sucessfully forecasted the Demand and Sales Forecasting Model for the target market buckets:
#[1] APAC-CONSUMER
#[2] EU-CONSUMER
#Please Refer attached Presentation file for clear understanding of Forecasting Results

#********************************** Thanks ***************************************#