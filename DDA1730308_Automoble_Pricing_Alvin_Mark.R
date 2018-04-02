##Introduction and Background Information
#Submission by:Alvin Mark Windsor 
#Customer: Geely Auto

#Business Objective: Understand the factors affecting the pricing of cars in America. 
#With an intention of setting up a manufacturing unit and producing cars locally in order to compete with US and European counterparts.

#Goals of Analysis: 
#[1] Which Variables are significant in predicting the price of a car?
#[2] How well these variables describe the price of the car?
#[3] Build a regression model to understand the pricing dynamics of automobiles in the American Market. This will be used to drive business decisions and the car design to cater to specific price levels. 

#Specifics Notes:
#Take only car company name as input for independent variable.	

##Pre-Requisites
#Set Working Directory to local system folder containing the input dataset.
#Importing required packages into the R-Environment [Note: Please install [1] "MASS" [2]"car" and [3] stats packages]
library(dplyr)
library(stringr)
library(ggplot2)
library(stats)
library("MASS")
library("car")

##Importing the pricing dataset and storing it in the dataframe price_auto
price_auto<-read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE, na.strings = c("na","NA"))

#Explaining the meaning of varibles in the dataset
#1.Car_ID:Unique id of each observation (Interger)
#2.Symboling :Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.(Categorical) 
#3.carCompany:Name of car company (Categorical)
#4.fueltype:Car fuel type i.e gas or diesel (Categorical)
#5.aspiration:Aspiration used in a car (Categorical)
#6.doornumber:Number of doors in a car (Categorical)
#7.carbody:body of car (Categorical)
#8.drivewheel:type of drive wheel (Categorical)
#9.enginelocation:Location of car engine (Categorical)
#10.wheelbase:Weelbase of car (Numeric)
#11.carlength:Length of car (Numeric)
#12.carwidth:Width of car (Numeric)
#13.carheight:height of car (Numeric)
#14.curbweight:The weight of a car without occupants or baggage. (Numeric)
#15.enginetype:Type of engine. (Categorical)
#16.cylindernumber:cylinder placed in the car (Categorical)
#17.enginesize:Size of car (Numeric)
#18.fuelsystem:Fuel system of car (Categorical)
#19.boreratio:Boreratio of car (Numeric)
#20.stroke:Stroke or volume inside the engine (Numeric)
#21.compressionratio:compression ratio of car (Numeric)
#22.horsepower:Horsepower (Numeric)
#23.peakrpm:car peak rpm (Numeric)
#24.citympg:Mileage in city (Numeric)
#25.highwaympg:Mileage on highway (Numeric)
#26.price(Dependent variable):Price of car (Numeric)

#Initial Structure of the dataset. Note: We will finally determine the structure of the dataset that we use for model building to ensure that all variables are of numeric datatype
str(price_auto)

##Data Cleaning and Preparation.

#Checking for any missing values
sum(is.na(price_auto))
#Therefore there are no missing values to address in this dataset

#Converting all text inputs to uppercase to avoid any case-sensitive issues
caseconversionfun<- function(input_col)
{
  if(is.character(input_col)==T)
  {
    input_col<- toupper(input_col)
  }
  return(input_col)
}
for(sb in 1:ncol(price_auto))
{
  price_auto[,sb]<- caseconversionfun(price_auto[,sb])
}

#Checking for duplicates in the dataset.
sum(duplicated(price_auto))
#This statement checks if each row/record in the dataset is duplicated. If there are five records having exactly the same values in the dataset this fuction will detect it and return the number of duplicates[for this example it will return 4].
#Since it returns 0 there are no duplicates in the dataset.

#Extracting only the car company name from the carCompany column
comp_name<- str_split_fixed(price_auto$CarName, pattern = " ", n=2)
price_auto$CarName<- comp_name[,1]
rm(comp_name)

#Checking Data Quality of CarName
unique(price_auto$CarName)
#Clearly there are text input errors for example VOLKSWAGEN is written as VW and VOKSWAGEN in certain records
price_auto$CarName <-str_replace_all(price_auto$CarName, pattern = "MAXDA", replacement = "MAZDA")
price_auto$CarName <-str_replace_all(price_auto$CarName, pattern = "PORCSHCE", replacement = "PORSCHE")
price_auto$CarName <-str_replace_all(price_auto$CarName, pattern = "TOYOUTA", replacement = "TOYOTA")
price_auto$CarName <-str_replace_all(price_auto$CarName, pattern = "VOKSWAGEN", replacement = "VOLKSWAGEN")
price_auto$CarName <-str_replace_all(price_auto$CarName, pattern = "VW", replacement = "VOLKSWAGEN")

###EDA for all predictor/dependednt variables and target variable.

##1. Car_ID-This Variable is a duplication of the row names. It's a column that is synonymus to serial number and will therefore be dropped from the dataset.
summary(price_auto$car_ID)
price_auto<- price_auto[,-1]

##2. Symboling-Insurance Risk Rating[Pretty Safe-Risky on a scale of -3 to 3] <Categorical-However due to the normalized scoring of this variable we will treat it as numeric in the model without creating dummy variables>
table(price_auto$symboling)
#From the summary it is clear that there are very few cars with very safe rating. Majority of the cars have a rating of either 0 or +1
symbol_price<- ggplot(data = price_auto, aes(x=as.factor(price_auto$symboling), y=price_auto$price))+ geom_boxplot() + xlab("Symboling-Insurance Risk")+ ylab("Price in USD") + ggtitle("Plt1. Symboling vs. Price")
symbol_price
#The boxplot shows that there is a general decline in the 1st quartile price of the car as it reduces -2 to +1 with the lowest price ranges coming in at +1 symboling. Followed with a slight increase till it reached grade +3.
#The vehicles with the lowest price range [>=75 percentile of the category] have a symboling rating if +1. Although, there are some outliers in the price range.

##3. Car Company Analysis- <Categorical Variable>
table(price_auto$CarName)
#From the frequency table it is clear that there is only one car from the company Mercury. Followed <=3 cars from Alfa-Romero, chevrolet, Jaguar and Renault. 
#Toyota has the most number of cars in the dataset.
company_price<- ggplot(data = price_auto, aes(x=price_auto$CarName, y=price_auto$price))+ geom_boxplot()+ xlab("Car Company Name")+ ylab("Price in USD") + ggtitle("Plt2. Car Company vs. Price")
company_price
#From the price boxplot it is clear that The brands with the most expensive vehicles in the dataset (i.e. >30,000 USD) belong to BUIC, JAGUAR, PORSCHE and BMW.
#Whereas the lower priced cars belong to CHEVROLET

##4.Fuel Type Analysis- <Categorical Variable>
table(price_auto$fueltype)
#There are almost 9x times more gasoline cars than diesel cars
fueltype_price<- ggplot(data = price_auto, aes(x=price_auto$fueltype, y=price_auto$price))+ geom_boxplot()+ xlab("Fuel-Type")+ ylab("Price in USD") + ggtitle("Plt3. Fuel-Type vs. Price")
fueltype_price
#The median price of Gasoline vehicles is lower than that of Diesel Vehicles. However, There seemes to be left skew in the price of Gasoline vehicles as there are a few vehicles that are very highly priced.

ft_price<- ggplot(data = price_auto, aes(price_auto$price, fill=as.factor(price_auto$fueltype)))+ geom_histogram(binwidth = 2500)+ xlab("Price in USD") + ylab("Frequency")+ ggtitle("Plt4. Frequency plot of Price by Fuel Type") +scale_fill_discrete(name = "Fuel Type")
ft_price
#This frequecy histogram confirms the previous insight that the price of Gasoline vehicles is skewed to the left with a few outlier gasoline vehicles very highly priced.

##5. Aspiration- <Categorical Variable>
table(price_auto$aspiration)
#There are almost 4x times more standard aspiration vehicles than Turbo aspirated in the dataset.

aspirate_price<- ggplot(data = price_auto, aes(x=price_auto$aspiration, y=price_auto$price))+ geom_boxplot()+ xlab("Engine Aspiration")+ ylab("Price in USD") + ggtitle("Plt5. Engine Aspiration vs. Price")
aspirate_price
#75th percentile of standard aspirated vehicles have a price lower than the median price of turbo aspirated vehicles. The frequency plot of price of standard aspirated vehicles is skewed to the left.


##6. Number of Doors- <Categorical Variable>
table(price_auto$doornumber)
#There is almost an equal spread of 2 and 4 doored vehicles in the dataset.

doornum_price<- ggplot(data = price_auto, aes(x=price_auto$doornumber, y=price_auto$price))+ geom_boxplot()+ xlab("Number of Doors")+ ylab("Price in USD") + ggtitle("Plt6. Number of Doors vs. Price")
doornum_price
#Two and Four Door vehicles are almost equally priced. There are however some outliers in the price of two-door vehicles. With the most expensive vehicle in the dataset bearing two doors.

##7. Carbody Type- <Categorical Variable>
table(price_auto$carbody)
#Sedans and Hatchbacks contribute to 80% of the vehicles in this dataset.
carbod_price<- ggplot(data = price_auto, aes(x=price_auto$carbody, y=price_auto$price))+ geom_boxplot()+ xlab("Type of car body")+ ylab("Price in USD") + ggtitle("Plt7. Carbody vs. Price")
carbod_price
#Hatchback vehicles have the lowest median price of vehicles in the data set whereas hardtop vehicles have the highest median price.
cb_price<- ggplot(data = price_auto, aes(price_auto$price, fill=as.factor(price_auto$carbody)))+ geom_histogram(binwidth = 2500)+ xlab("Price in USD") + ylab("Frequency")+ ggtitle("Plt8. Frequency plot of Price by Carbody Type") +scale_fill_discrete(name = "Carbody Type")
cb_price
#Majority of the vehicles in the dataset are priced below 25,000 USD and about 90% of sedans and hatchbacks are priced below 25,000 USD.

##8. Drivewheel Type- <Categorical Variable>
table(price_auto$drivewheel)
#Front wheel drive and rear wheel drive vehicles contribute to almost 95% of the vehicles in the dataset.

drivewheel_price<- ggplot(data = price_auto, aes(x=price_auto$drivewheel, y=price_auto$price))+ geom_boxplot()+ xlab("Drivewheel Type")+ ylab("Price in USD") + ggtitle("Plt9. Drivewheel vs. Price")
drivewheel_price
#From the above boxplot it is clear that the price of rearwheel drive vehicles (RWD) is significantly higher than the other two drivewheel categories. With Frontwheel drive (FWD) vehicles having the lowest median price.

##9.Engine Location- <Categorical Variable>
table(price_auto$enginelocation)
#Almost all vehicles in the dataset have engines placed in the front of the vehicle. [This makes the number of records with vehicles with rear engines seemingly insignificant]

engineloc_price<- ggplot(data = price_auto, aes(x=price_auto$enginelocation, y=price_auto$price))+ geom_boxplot()+ xlab("Engine Location")+ ylab("Price in USD") + ggtitle("Plt10. Engine Location vs. Price")
engineloc_price 
#From the above boxplot it is clear that the price of vehicles with rear placed engines is significantly higher than the price of vehicles with front placed engines. However the low sample size of rear engined vehicles is a cause for concern.

##10. Wheelbase Analysis- <Numeric Variable>
summary(price_auto$wheelbase)
boxplot(price_auto$wheelbase)
title(main = "Boxplot of Wheelbase", ylab = "Wheelbase")
#Majority of the dataset have a wheel base between 95-115
quantile(price_auto$wheelbase, seq(0,1,0.01))

#Outlier Treatment: There is a significant jump in the wheelbase between 99-100 percentile therefore we will cap wheelbase at 99th Percentile with 115.544 units.
price_auto$wheelbase[which(price_auto$wheelbase > 115.54)]<- 115.54

#Plotting a Scatterplot
wheelbas_price<- ggplot(data = price_auto, aes(x=price_auto$wheelbase, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Wheelbase")+ ylab("Price in USD") + ggtitle("Plt11. Wheelbase vs. Price")
wheelbas_price
#This scatter plot tells us that the lowest price range is for vehicles having a wheelbase of around 95 following which the price increases with increase in wheelbase.

##11. Carlength Analysis- <Numeric Variable>
summary(price_auto$carlength)
boxplot(price_auto$carlength)
title(main = "Boxplot of Carlenght", ylab = "Carlength")
# Data seems to be evenly spread out with one lower range outlier. However majority vehicles seem to have a carlength between 162-182
quantile(price_auto$carlength, seq(0,1,0.01))
# Outlier Treatment is not required

#Plotting a Scatterplot
carleng_price<- ggplot(data = price_auto, aes(x=price_auto$carlength, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Carlength")+ ylab("Price in USD") + ggtitle("Plt12. Carlength vs. Price")
carleng_price
#There seems to be a general trend increase in the price of the vehicle with higher carlength.

##12. Carwidth Analysis- <Numeric Variable>
summary(price_auto$carwidth)
boxplot(price_auto$carwidth)
title(main = "Boxplot of Carwidth", ylab = "Carwidth")
#Majority of vehicles seem to have a carwidth between 64-68 units. 
quantile(price_auto$carwidth, seq(0,1,0.01))
#No sudden spikes therefore outlier treatment is not required.

#plotting a Scatterplot
carwidth_price<- ggplot(data = price_auto, aes(x=price_auto$carwidth, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Carwidth")+ ylab("Price in USD") + ggtitle("Plt13. Carwidth vs. Price")
carwidth_price
#There seems to be a general trend increase in the price of the vehicle with increase in carwidth.

##13. Carheight Analysis- <Numeric Variable>
summary(price_auto$carheight)
boxplot(price_auto$carheight)
title(main = "Boxplot of Carheight", ylab = "Carheight")
quantile(price_auto$carheight, seq(0,1,0.01))
#Vehicle height seems to be evenly spread with no sudden spikes. Majority of vehicle heights are between 56-56

carheight_price<- ggplot(data = price_auto, aes(x=price_auto$carheight, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Carheight")+ ylab("Price in USD") + ggtitle("Plt14. Carheight vs. Price")
carheight_price
#The plot shows some periodic fluctuation with a low price for vehicles with a height between 51-55. Followed by and increase in price between 55-57.5 then followed by a drop in price.

##14. Curbweight Analysis- <Numeric Variable>
summary(price_auto$curbweight)
boxplot(price_auto$curbweight)
title(main = "Boxplot of Curbweight", ylab = "Curbweight")
quantile(price_auto$curbweight, seq(0,1,0.01))
#Majority of the vehicles seem to have a curb weight between 2200-2700 kg. When observing the spread there is a slight peak in weight between 0-1st percentile. However, we will not treat this as an outlier

curbwt_price<- ggplot(data = price_auto, aes(x=price_auto$curbweight, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Curbweight")+ ylab("Price in USD") + ggtitle("Plt15. Curbweight vs. Price")
curbwt_price
#From the scatter plot it is clear that the price of the vehicle increases with an increase in the curbweight.

##15. Enginetype Analysis- <Catagorical Variable>
table(price_auto$enginetype)
#Vehicles with OHC (overhead head cam) and OHCF engine type contribute to 80% of the vehivles in the dataset.

enginetype_price<- ggplot(data = price_auto, aes(x=price_auto$enginetype, y=price_auto$price))+ geom_boxplot()+ xlab("Engine Type")+ ylab("Price in USD") + ggtitle("Plt16. Engine Type vs. Price")
enginetype_price

##16. Cylinder Number Analysis- <Categorical Variable>
table(price_auto$cylindernumber)
#Vehicles with four and six cylinder engines contribute to 90% of the dataset. There is one record entry for a vehicle having 3 or 12 cylinders respetively

cylnum_price<-ggplot(data = price_auto, aes(x=price_auto$cylindernumber, y=price_auto$price))+ geom_boxplot()+ xlab("Number of Engine Cylinders")+ ylab("Price in USD") + ggtitle("Plt16. Engine Cylinders vs. Price")
cylnum_price
# The median cost of eight cylinder vehicles is higher than other cylinder categories.

##17. Engine Size Analysis- <Numeric Variable>
summary(price_auto$enginesize)
boxplot(price_auto$enginesize)
title(main = "Boxplot of Engine Size", ylab = "Engine Size")
quantile(price_auto$enginesize, seq(0,1,0.01))
#There are significant spikes in the engine size 2-3 percentile, 49-50 percentile and 98-99%. However, the engine size is collinear with number of cylinders and for each discrete value increase in the number of cylinders there might be significant spike in engine size. 
#Due to the aforementioned reason we will chose not to regulate it.

enginesize_price<- ggplot(data = price_auto, aes(x=price_auto$enginesize, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Engine Size")+ ylab("Price in USD") + ggtitle("Plt17. Engine Size vs. Price")
enginesize_price
#From the above scatterplot it is clear that there is a significant increase in price of the vehicle with an incerease in engine size.

##18. Fuel System Analysis- <Categorical Variable>
table(price_auto$fuelsystem)
#MPFI and 2BBL fuel injection systems Contribute to about 70% of the vehicles in the dataset

fuelsys_price<- ggplot(data = price_auto, aes(x=price_auto$fuelsystem, y=price_auto$price))+ geom_boxplot()+ xlab("Fuel-System")+ ylab("Price in USD") + ggtitle("Plt18. Fuel-System vs. Price")
fuelsys_price
#From the boxplot it is clear that vehicles Multi-port Fuel Injection [MPFI] have the highest median price. There are also some outliers on the higher price side having MPFI systems.

##19. Bore-Ratio Analysis- <Numeric Variable>
summary(price_auto$boreratio)
boxplot(price_auto$boreratio)
title(main = "Boxplot of Engine Bore Ratio", ylab = "Bore Ratio")
quantile(price_auto$boreratio, seq(0,1,0.01))
#From the percentile summary and the boxplot it is clear that engine bore ratio is fairly evenly spread out with a majority of vehicles having bore ratio between 3.1-3.6.

borerat_price<- ggplot(data = price_auto, aes(x=price_auto$boreratio, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Engine Bore Ratio")+ ylab("Price in USD") + ggtitle("Plt19. Engine Bore Ratio vs. Price")
borerat_price
#From the scatterplot it is clear that there seems to be a general trend increase in price with an increase in bore-ratio

##20. Stroke Length Analysis- <Numeric Variable>
summary(price_auto$stroke)
boxplot(price_auto$stroke)
title(main = "Boxplot of Engine Stroke Length", ylab = "Stroke Length")
quantile(price_auto$stroke, seq(0,1,0.01))
#There is a significant jump in in stoke length from 1-2 percentile
#Outlier Treatment:
price_auto$stroke[which(price_auto$stroke<2.64)]<-2.64

stroke_price<- ggplot(data = price_auto, aes(x=price_auto$stroke, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Engine Stroke Length")+ ylab("Price in USD") + ggtitle("Plt20. Engine Stroke Length vs. Price")
stroke_price
#There seems to be some price fluctuation with an increase in price for an increase in engine stroke length.

##21. Compression Ratio- <Numeric Variable>
summary(price_auto$compressionratio)
boxplot(price_auto$compressionratio) 
title(main = "Boxplot of Engine Compression Ratio", ylab = "Compression Ratio")
quantile(price_auto$compressionratio, seq(0,1,0.01))
#From the engine compression ratio boxplot and quantile summary it is clear that majority of the datapoints are between 8-9.5. However there is a significant jump in the compression ratio from the 90-91 percentile. Therefore we will cap all outlier values at 10.94
#Outlier Treatment:
price_auto$compressionratio[which(price_auto$compressionratio>10.94)]<-10.94

#Scatter Plot
comprat_price<- ggplot(data = price_auto, aes(x=price_auto$compressionratio, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Engine Compression Ratio")+ ylab("Price in USD") + ggtitle("Plt21. Engine Compression Ratio vs. Price")
comprat_price
#There seems to be some periodicity in the price with respect to compression ratio. However, the lowest prices are belong to engines with a compression ratio between 8.5-9.5

##22. Horsepower Analysis- <Numeric Variable>
summary(price_auto$horsepower)
boxplot(price_auto$horsepower) 
title(main = "Boxplot of Horsepower", ylab = "Horsepower")
quantile(price_auto$horsepower, seq(0,1,0.01))
#The boxplot and the percentile summary show that majority of vehicles have horsepower between 90-120 hp. However there is a significant jump at 97-98% and therefore all outlier values will be capped at 184.0 hp
#Outlier Treatment
price_auto$horsepower[which(price_auto$horsepower > 184.0)]<- 184.0

#Scatter Plot
horsepow_price<- ggplot(data = price_auto, aes(x=price_auto$horsepower, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Vehicle Horsepower")+ ylab("Price in USD") + ggtitle("Plt22. Vehicle Horsepower vs. Price")
horsepow_price
#There seems to be an almost linear increase in price with an increase in horsepower

##23. Peak RPM Analysis- <Numeric Variable>
summary(price_auto$peakrpm)
boxplot(price_auto$peakrpm) 
title(main = "Boxplot of Peak RPM", ylab = "Peak RPM")
quantile(price_auto$peakrpm, seq(0,1,0.01))
#The peak rpm is almost evenly distributed with majority of the dataset having peak rpm between 4800-5600 rpm. However there is a significant spike between 99-100 percentlie. Therefore we will cap all outliers at 6000 rpm
#Outlier Treatment:
price_auto$peakrpm[which(price_auto$peakrpm>6000)]<-6000

#Scatter Plot
peakrpm_price<- ggplot(data = price_auto, aes(x=price_auto$peakrpm, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Vehicle Peak RPM")+ ylab("Price in USD") + ggtitle("Plt23. Vehicle Peak RPM vs. Price")
peakrpm_price
#There seems to be little to no variation in price of the vehicle with peak rpm.[Very low fluctuation in price for change in peak RPM]

##24. City MPG- <Numeric Variable>
summary(price_auto$citympg)
boxplot(price_auto$citympg) 
title(main = "Boxplot of City MPG", ylab = "City MPG")
quantile(price_auto$citympg, seq(0,1,0.01))
#The boxplot and the percentile summary suggests that majority of the vehicles in the dataset have a city mileage between 19-31 mpg. However there is a significant spike at 98-99 percentile. Therefore these values will be capped at 38.00 mpg
#Outlier Treatment
price_auto$citympg[which(price_auto$citympg>38.0)]<-38.0

#Scatter Plot
citympg_price<- ggplot(data = price_auto, aes(x=price_auto$citympg, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Vehicle City MPG")+ ylab("Price in USD") + ggtitle("Plt24. Vehicle City Mileage MPG vs. Price")
citympg_price
#From the scatterplot it is evident that as the city mileage decreases the price of the car increases. They are seemingly neagitivly correlated

##25. Highway MPG- <Numeric Variable>
summary(price_auto$highwaympg)
boxplot(price_auto$highwaympg) 
title(main = "Boxplot of Highway MPG", ylab = "Highway MPG")
quantile(price_auto$highwaympg, seq(0,1,0.01))

#From the boxplot and the percentile summary it is clear that majority of vehicles have a highway mileage between 24-35 mpg. However, there is a spike between 98-99% and therefore these outlier values will be capped at 46.92 mpg
#Outlier Treatment
price_auto$highwaympg[price_auto$highwaympg> 46.92]<- 47.0

#Scatter Plot
highmpg_price<- ggplot(data = price_auto, aes(x=price_auto$highwaympg, y=price_auto$price))+ geom_point()+geom_smooth(method = "loess")+ xlab("Vehicle Highway MPG")+ ylab("Price in USD") + ggtitle("Plt25. Vehicle Highway Mileage MPG vs. Price")
highmpg_price
#From the scatterplot it is evident that as the highway mileage decreases the price of the car increases. They are seemingly neagitivly correlated

##26. Price- <Numeric Variable- Target>
summary(price_auto$price)
boxplot(price_auto$price) 
title(main = "Plt26. Boxplot of Vehicle Price", ylab = "Vehicle Price")
quantile(price_auto$price, seq(0,1,0.01))
#From the boxplot and percentile summary of the Price Variable it is clear that majority of the vehicles in the dataset are priced between 7000-18000 USD. However there are significant outliers on the higher price ranges. There is a significant spike between 98-99 percentile. Therefore these values will be capped off at 36809.60 USD
#Outlier Treatment:Under the instructions of SME: Prof. Neelam Sinha.
price_auto$price[which(price_auto$price>36809.60)]<-36809.60

#EDA for all the 26 attributes in the dataset has been performed. All outliers have been identified and treated accordingly. Next we will focus on creating dummy variables for all categorical attributes.


###Creating Dummy Variables for Categorical Attributes so as to convert the dataset into a form compatible with the regression model.
##Ref: https://learn.upgrad.com/v/course/77/question/60221 posts on the discussion forum suggest that the categorical attributes like symboling, enginetype, cylindernumber, fuelsystem etc should not be further categorized and grouped rather it should be converted to a factor and an appropriate number of dummy variables should be created. I have followed the same principle.

#Using a function "convertfactor" to convert all categorical parameters with binary output into a numeric vector with the values 1 and 0. This will convert these columns into a format suitable to the regression model.
convertfactor<- function(input_string)
{
  input_string<- factor(input_string)
  levels(input_string)<- c(1,0)
  input_string<- as.numeric(levels(input_string))[input_string]
  return(input_string)
}

##1. Fuel Type has two levels Diesel and Gasoline this attribute is passed to the custom function and DIESEL=1 and GAS=0 values are assigned.
price_auto$fueltype<- convertfactor(price_auto$fueltype)
##2. Engine Aspiration is of two types Standard and Turbo. This is passed to the custom function with STD=1 and TURBO=0 values are assigned.
price_auto$aspiration<- convertfactor(price_auto$aspiration)
##3. Doornumber has two levels TWO and Four. This is passed to the custom function and TWO=1 and FOUR=0 values are assigned.
price_auto$doornumber<- convertfactor(price_auto$doornumber)
##4. Engine location has two levels FRONT and REAR. This is passed to the custom function and FRONT=1 and REAR=0 values are assigned.
price_auto$enginelocation<- convertfactor(price_auto$enginelocation) 

##5. Carbody Atrribute has 5 levels. We will now create 4 dummy variable and then attach it to the main dataframe 
table(price_auto$carbody)
price_auto$carbody<- as.factor(price_auto$carbody)
#Now we have converted it to a factor. We will create the dummy variables and store it in a temporary dataframe temp_dum
temp_dum<- data.frame(model.matrix(~carbody, data = price_auto))
temp_dum<- temp_dum[,-1]
price_auto<- price_auto[,-6]
#we will remove the first column from the temp_dum df as it represents the index. We will also remove the original carbody column from the price_auto df and then append the two dataframes.
price_auto<- cbind(price_auto, temp_dum)
rm(temp_dum)

##6. Drivewheel Attribute has 3 levels. We will create 2 dummy variables and attach it to the main dataframe
table(price_auto$drivewheel)
price_auto$drivewheel<- as.factor(price_auto$drivewheel)
#We have converted it to a factor. Now we will create 2 dummy variables and store it in temp_dum dataframe.
temp_dum<- data.frame(model.matrix(~drivewheel, data = price_auto))
temp_dum<- temp_dum[,-1]
price_auto<- price_auto[,-6]
#we will remove the first column from the temp_dum df as it represents the index. We will also remove the original carbody column from the price_auto df and then append the two dataframes.
price_auto<- cbind(price_auto, temp_dum)
rm(temp_dum)

##7. Symboling is a scaled insurance risk rating given to a car. It assigns values based on a scale of -3 to +3 synonymous to Pretty Safe to Risky. In our dataset we have ratings from -2 to +3. Theoretically I could have used the attribute in its native state in the Regression Model.
#However, after discussing this issue with SME Prof. Sinha I decided to convert it to a factor with levels from -2 to +3 as she suggested all categorical variables must be coverted into dummy attributes.
table(price_auto$symboling)
price_auto$symboling<- as.factor(price_auto$symboling)
#We have converted it to a factor. Now we will create 5 dummy variables and store it in temp_dum dataframe.
temp_dum<- data.frame(model.matrix(~symboling, data = price_auto))
temp_dum<- temp_dum[,-1] 
names(temp_dum)<- c("symboling[-1]","symboling[0]", "symboling[+1]", "symboling[+2]", "symboling[+3]")
#Renaming the symboling columns of the temp dataframe for better readability
price_auto<- price_auto[,-1]
price_auto<- cbind(price_auto, temp_dum)
rm(temp_dum)

##8. Car Company has 22 levels with each level representing a car company name. We will create 21 dummy variables and attach it to the maindataframe
table(price_auto$CarName)
price_auto$CarName<- as.factor(price_auto$CarName)
temp_dum<- data.frame(model.matrix(~CarName, data = price_auto))
temp_dum<- temp_dum[,-1]
#Removing the CarName column from the main dataset and appending the 21 dummy variables to the price_auto dataframe.
price_auto<- price_auto[,-1]
price_auto<- cbind(price_auto, temp_dum)
rm(temp_dum)

##9. Car Engine Type has 7 levles with each level representing the type of engine used in the vehicle. We will create 6 dummy variables and attach it to the maindataframe
table(price_auto$enginetype)
price_auto$enginetype<- as.factor(price_auto$enginetype)
temp_dum<- data.frame(model.matrix(~enginetype, data = price_auto))
temp_dum<- temp_dum[,-1]
#Removing the enginetype column from the main dataset and appending the 6 dummy variables to the price_auto dataframe.
price_auto<- price_auto[,-10]
price_auto<- cbind(price_auto, temp_dum)
rm(temp_dum)

##10. Cylinder Number has 7 levels with each level representing the number of cylinders in the engine of the vehicle. We will create 6 dummy variables and attach it to the main dataframe
table(price_auto$cylindernumber)
price_auto$cylindernumber<- as.factor(price_auto$cylindernumber)
temp_dum<- data.frame(model.matrix(~cylindernumber, data = price_auto))
temp_dum<- temp_dum[,-1]
#Removing the cylinder number column from the main dataset and appending the 6 dummy variables to the price_auto dataframe.
price_auto<- price_auto[,-10]
price_auto<- cbind(price_auto, temp_dum)
rm(temp_dum)

##11. Fuel System has 8 levels with each level representing the fuel injection system in the vehicle. We will create 7 dummy variables and attach it to the main dataframe
table(price_auto$fuelsystem)
price_auto$fuelsystem<- as.factor(price_auto$fuelsystem)
temp_dum<- data.frame(model.matrix(~fuelsystem, data = price_auto))
temp_dum<- temp_dum[,-1]
#Removing the fuelsystem column from the main dataset and appending the 7 dummy variables to the price_auto dataframe.
price_auto<- price_auto[,-11]
price_auto<- cbind(price_auto, temp_dum)
rm(temp_dum)


###Derived Metrics 
##1. In the automobile sector the power(hp)/curbweight ratio is an important parameter. Usually vehicles with a high power to weight ratio are performance vehicles and will therefore be priced higher than regular vehicles.
price_auto$powerwtrat<- round(price_auto$horsepower/price_auto$curbweight, 3)

##2. We extracted the car company name from the initial dataset. This can be considered as another derived metric.

##3. There are more provisions to calculate metrics like Torque using bore diameter, stroke length and number of cylinders. But since deriving these metrics will require some domain knowledge. I will refrain from including such parameters.
##4. Re-grouping categorical variables into subgroups is not beneficial accourding to the stackoverflow forum. Therefore we will proceed only with the power to weight ratio metric.


###Model Building
#Now that the input dataset is prepared we will focus on building the regression model to predict the price of the vehicle.
price_auto$curbweight<- as.numeric(price_auto$curbweight)
price_auto$enginesize<- as.numeric(price_auto$enginesize)
str(price_auto)
#All input variables are in numeric datatype and will therefore be in a compatible format for the regression model.

#Setting the seed value for repeatability
set.seed(100)

#Since the total dataset has 205 observations we will split it into testing and training dataset using the 80%-20% rule.
training_index<- sample(1:nrow(price_auto), 0.8*nrow(price_auto))
trng_set<- price_auto[training_index,]
test_set<- price_auto[-training_index,]
#We have successfully split the input dataset into Training: trng_set with 164 records and Testing: test_set with 41 records.

##We will now begin the iterative step of eliminating the non-significant predictor varibles from each successive model till we arrive at the final model.

mod_1<- lm(price~., data = trng_set)
summary(mod_1)
#Variable Selection Stratergy:
#From The first model we have got a fantastic adjusted R-squared Parameter of 0.9634. However there are 69 input parameters to the model with numeorus insignificant variables.
#From a business perspective it will be very hard to deal with a model with so many input parameters. Therefore, We can leverage the stepAIC function as part of the "car" package to initally eliminate a chunk of insignificant variables.
#We will set the direction to "both" as we need both forward and backward variable selection methods. During this step the AIC function makes multiple calls to the model and finally suggests a list of input variables/predictors that it considers significant to the model.
#We will store the output of the stepAIC function in variable aicfilter.
#To summarise we have used stepAIC to eliminate a chunk of insignificant variables then we adopt the backward selection approach.
#Eliminating variables in each stage depending on p-value and VIF.
aicfilter<- stepAIC(mod_1, direction = "both")
aicfilter

#The stepAIC function used above has reduced the mod_1 to 69 input variables to 40 input variables.
#We will assign the ouput model equation of the stepAIC function to mod_2
mod_2<- lm(formula = price ~ aspiration + doornumber + enginelocation + 
             wheelbase + carlength + carwidth + carheight + enginesize + 
             boreratio + compressionratio + horsepower + peakrpm + carbodyHARDTOP + 
             carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
             CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
             CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
             CarNamePORSCHE + CarNameRENAULT + CarNameSAAB + CarNameSUBARU + 
             CarNameTOYOTA + enginetypeL + enginetypeOHC + enginetypeOHCV + 
             enginetypeROTOR + cylindernumberFOUR + fuelsystem2BBL + fuelsystemMPFI + 
             powerwtrat + CarNameJAGUAR + citympg, data = trng_set)
#Now we will check the multicollinearity of the the above input variables using the VIF parameter.
vif(mod_2)
#From the Variable inflation factors of mod_2 it is clear that there are several variables with VIF>10.
#We will now check the significance [p>0.05] of these variables through the summary and decide which variable to remove.
summary(mod_2)
#remove citympg as its VIF=12.9 and the p-value is 0.202 with no stars. Hence we will remove it generate the next model

mod_3<- lm(formula = price ~ aspiration + doornumber + enginelocation + 
             wheelbase + carlength + carwidth + carheight + enginesize + 
             boreratio + compressionratio + horsepower + peakrpm + carbodyHARDTOP + 
             carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
             CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
             CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
             CarNamePORSCHE + CarNameRENAULT + CarNameSAAB + CarNameSUBARU + 
             CarNameTOYOTA + enginetypeL + enginetypeOHC + enginetypeOHCV + 
             enginetypeROTOR + cylindernumberFOUR + fuelsystem2BBL + fuelsystemMPFI + 
             powerwtrat + CarNameJAGUAR, data = trng_set)

vif(mod_3)
summary(mod_3)
#Comparing the variables with high VIF>10 and high p-value [p>0.05]. 
#enginetypeL has a VIF of 15.37 and a p-value of 0.145 therefore we will eliminate enginetypeL in the next model.

mod_4<- lm(formula = price ~ aspiration + doornumber + enginelocation + 
             wheelbase + carlength + carwidth + carheight + enginesize + 
             boreratio + compressionratio + horsepower + peakrpm + carbodyHARDTOP + 
             carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
             CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
             CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
             CarNamePORSCHE + CarNameRENAULT + CarNameSAAB + CarNameSUBARU + 
             CarNameTOYOTA + enginetypeOHC + enginetypeOHCV + 
             enginetypeROTOR + cylindernumberFOUR + fuelsystem2BBL + fuelsystemMPFI + 
             powerwtrat + CarNameJAGUAR, data = trng_set)

vif(mod_4)
summary(mod_4)
#Comparing the variables with high VIF approx.10 and high p-value [p>0.05].
#peakrpm has a VIF of 4.54 and a p-value of 0.0557 therefore we will eliminate peakrpm in the next model.

mod_5<- lm(formula = price ~ aspiration + doornumber + enginelocation + 
             wheelbase + carlength + carwidth + carheight + enginesize + 
             boreratio + compressionratio + horsepower + carbodyHARDTOP + 
             carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
             CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
             CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
             CarNamePORSCHE + CarNameRENAULT + CarNameSAAB + CarNameSUBARU + 
             CarNameTOYOTA + enginetypeOHC + enginetypeOHCV + 
             enginetypeROTOR + cylindernumberFOUR + fuelsystem2BBL + fuelsystemMPFI + 
             powerwtrat + CarNameJAGUAR, data = trng_set)

vif(mod_5)
summary(mod_5)
#Comparing the variables with high VIFapprox. 10 and high p-value [p>0.05].
#fuelsystemMPFI has a VIF of 6.4 and a p-value of 0.108 therefore we will eliminate fuelsystemMPFI in the next model.

mod_6<- lm(formula = price ~ aspiration + doornumber + enginelocation + 
             wheelbase + carlength + carwidth + carheight + enginesize + 
             boreratio + compressionratio + horsepower + carbodyHARDTOP + 
             carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
             CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
             CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
             CarNamePORSCHE + CarNameRENAULT + CarNameSAAB + CarNameSUBARU + 
             CarNameTOYOTA + enginetypeOHC + enginetypeOHCV + 
             enginetypeROTOR + cylindernumberFOUR + fuelsystem2BBL + 
             powerwtrat + CarNameJAGUAR, data = trng_set)
  
vif(mod_6)
summary(mod_6)
##At this stage we can see that there a a number of variables with very high p-value. This means that these variables irrespectie of their VIF will not contribute significantly to the price of the vehicle.
#fuelsystem2BBL has a p-value of 0.508 therefore we will eliminate fuelsystem2BBL in the next model.

mod_7<- lm(formula = price ~ aspiration + doornumber + enginelocation + 
             wheelbase + carlength + carwidth + carheight + enginesize + 
             boreratio + compressionratio + horsepower + carbodyHARDTOP + 
             carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
             CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
             CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
             CarNamePORSCHE + CarNameRENAULT + CarNameSAAB + CarNameSUBARU + 
             CarNameTOYOTA + enginetypeOHC + enginetypeOHCV + 
             enginetypeROTOR + cylindernumberFOUR + 
             powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_7)
summary(mod_7)
#CarNameRENAULT has a p-value of 0.354 therefore we will eliminate CarNameRENAULT in the next model.

mod_8<- lm(formula = price ~ aspiration + doornumber + enginelocation + 
             wheelbase + carlength + carwidth + carheight + enginesize + 
             boreratio + compressionratio + horsepower + carbodyHARDTOP + 
             carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
             CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
             CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
             CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
             CarNameTOYOTA + enginetypeOHC + enginetypeOHCV + 
             enginetypeROTOR + cylindernumberFOUR + 
             powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_8)
summary(mod_8)
#enginetypeOHCV has a p-value of 0.3404 and therefore we will eliminate enginetypeOHCV in the next model.

mod_9<- lm(formula = price ~ aspiration + doornumber + enginelocation + 
             wheelbase + carlength + carwidth + carheight + enginesize + 
             boreratio + compressionratio + horsepower + carbodyHARDTOP + 
             carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
             CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
             CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
             CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
             CarNameTOYOTA + enginetypeOHC + 
             enginetypeROTOR + cylindernumberFOUR + 
             powerwtrat + CarNameJAGUAR, data = trng_set)

vif(mod_9)
summary(mod_9)
#enginetypeROTOR has a p-value of 0.2257 and therefore we will eliminate enginetypeROTOR in the next model

mod_10<- lm(formula = price ~ aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carwidth + carheight + enginesize + 
              boreratio + compressionratio + horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
              CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              cylindernumberFOUR + 
              powerwtrat + CarNameJAGUAR, data = trng_set)

vif(mod_10)
summary(mod_10)
#doornumber has a p-value of 0.1276 and therefore we will eliminate doornumber in the next model

mod_11<- lm(formula = price ~ aspiration + enginelocation + 
              wheelbase + carlength + carwidth + carheight + enginesize + 
              boreratio + compressionratio + horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
              CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              cylindernumberFOUR + 
              powerwtrat + CarNameJAGUAR, data = trng_set)

vif(mod_11)
summary(mod_11)
#cylindernumberFOUR has a p-value of 0.124 therefore we will eliminate cylindernumberFOUR in the next model

mod_12<- lm(formula = price ~ aspiration + enginelocation + 
              wheelbase + carlength + carwidth + carheight + enginesize + 
              boreratio + compressionratio + horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
              CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_12)
summary(mod_12)
#boreratio has a p-value of 0.1369 therefore we will eliminate boreratio in the next model

mod_13<- lm(formula = price ~ aspiration + enginelocation + 
              wheelbase + carlength + carwidth + carheight + enginesize + 
              compressionratio + horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
              CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_13)
summary(mod_13)
#enginesize has a p-value of 0.178 therefore we will eliminate enginesize in the next model

mod_14<- lm(formula = price ~ aspiration + enginelocation + 
              wheelbase + carlength + carwidth + carheight + 
              compressionratio + horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
              CarNameISUZU + CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_14)
summary(mod_14)
#CarNameISUZU has a p-value of 0.1943 therefore we will remove it in the next model

mod_15<- lm(formula = price ~ aspiration + enginelocation + 
              wheelbase + carlength + carwidth + carheight + 
              compressionratio + horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
              CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_15)
summary(mod_15)
#drivewheelRWD has a p-valur of 0.177 therefore we will remove it in the next model

mod_16<- lm(formula = price ~ aspiration + enginelocation + 
              wheelbase + carlength + carwidth + carheight + 
              compressionratio + horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
              CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_16)
summary(mod_16)
##Now that we have eliminated all predictor variables with p-values >0.05 we will now focus on eliminating varibles based on High VIF and high p-value
#carlength has a VIF=13.7 and a p-value of 0.003126 with ** rating therefore we will eliminate carlength in the next model

mod_17<- lm(formula = price ~ aspiration + enginelocation + 
              wheelbase + carwidth + carheight + 
              compressionratio + horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
              CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_17)
summary(mod_17)
#carheight has a VIF=4.409 and a p-value of 0.034 with * rating therefore we will eliminate carheight in the next model

mod_18<- lm(formula = price ~ aspiration + enginelocation + 
              wheelbase + carwidth + 
              compressionratio + horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + CarNameDODGE + 
              CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_18)
summary(mod_18)
#CarNameDODGE has a p-value of 0.11504 with no significance rating therefore we will remove this from the model

mod_19<- lm(formula = price ~ aspiration + enginelocation + 
              wheelbase + carwidth + 
              compressionratio + horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNameMITSUBISHI + CarNamePEUGEOT + CarNamePLYMOUTH + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_19)
summary(mod_19)
#CarNamePLYMOUTH has a p-value of 0.125351 with no significance rating therefore we will remove CarNamePLYMOUTH from the model.

mod_20<- lm(formula = price ~ aspiration + enginelocation + 
              wheelbase + carwidth + 
              compressionratio + horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNameMITSUBISHI + CarNamePEUGEOT + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_20)
summary(mod_20)
#compressionratio  has a p-value of 0.1104 with no significance rating therefore we will remove compressionratio from the model.

mod_21<- lm(formula = price ~ aspiration + enginelocation + 
              wheelbase + carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNameMITSUBISHI + CarNamePEUGEOT + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_21)
summary(mod_21)
#wheelbase has a VIF=7.18 and p-value=0.02149 with a significance rating of *. Therefore we will eliminate wheelbase in our next model.

mod_22<- lm(formula = price ~ aspiration + enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNameMITSUBISHI + CarNamePEUGEOT + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)

vif(mod_22)
summary(mod_22)
#aspiration has a p-value of 0.0751 therefore we will remove it from the next model

mod_23<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNameMITSUBISHI + CarNamePEUGEOT + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_23)
summary(mod_23)
#CarNamePEUGEOT has a VIF=2.27 and p-value=0.053. Therefore we will remove CarNamePEUGEOT from the next model

mod_24<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNameMITSUBISHI + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + enginetypeOHC + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_24)
summary(mod_24)
#enginetypeOHC has a p-value of 0.254 with VIF=2. Therefore we will remove it in the next model

mod_25<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNameMITSUBISHI + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              CarNameTOYOTA + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_25)
summary(mod_25)
#CarNameTOYOTA has a p-value of 0.087. Therefore we will remove it in the next model

mod_26<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNameMITSUBISHI + 
              CarNamePORSCHE + CarNameSAAB + CarNameSUBARU + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_26)
summary(mod_26)
#CarNameSUBARU has a p-value of 0.119. Therefore we will remove it in the next model

mod_27<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNameMITSUBISHI + 
              CarNamePORSCHE + CarNameSAAB + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_27)
summary(mod_27)
#CarNameMITSUBISHI has a p-value of 0.03 and a VIF of 1.04 we will remove this in the next model

mod_28<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNamePORSCHE + CarNameSAAB + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_28)
summary(mod_28)
#CarNameSAAB has a p-value of 0.0125 with no significance rating therefore we will remove it in the next model.

mod_29<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNamePORSCHE + 
              powerwtrat + CarNameJAGUAR, data = trng_set)
vif(mod_29)
summary(mod_29)
#Now we have reached the stage in the model that all predictor parameters have 3* significance. However, we have some significantly high VIF so we will remove these variables
#Betweeen powerwtrat and horsepower both having significantly high VIF we will drop that variable that causes the least drop in R square
#After Trial We we will drop powerwtrat

mod_30<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameAUDI + CarNameBMW + CarNameBUICK + 
              CarNamePORSCHE + 
              CarNameJAGUAR, data = trng_set)
vif(mod_30)
summary(mod_30)
#Drop CarNameAUDI as p-value is 0.0617
mod_31<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
              CarNameBMW + CarNameBUICK + 
              CarNamePORSCHE + 
              CarNameJAGUAR, data = trng_set)
vif(mod_31)
summary(mod_31)
#Drop carbodySEDAN as VIF=8.5
mod_32<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodyWAGON + 
              CarNameBMW + CarNameBUICK + 
              CarNamePORSCHE + 
              CarNameJAGUAR, data = trng_set)
vif(mod_32)
summary(mod_32)
#Drop carbodyWAGON as p-value=0.37
mod_33<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + carbodyHARDTOP + 
              carbodyHATCHBACK + 
              CarNameBMW + CarNameBUICK + 
              CarNamePORSCHE + 
              CarNameJAGUAR, data = trng_set)
vif(mod_33)
summary(mod_33)
#Drop carbodyHARDTOP as p=0.02708
mod_34<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + 
              carbodyHATCHBACK + 
              CarNameBMW + CarNameBUICK + 
              CarNamePORSCHE + 
              CarNameJAGUAR, data = trng_set)

vif(mod_34)
summary(mod_34)
#Drop carbodyHATCHBACK as p-value=0.01548 and significance rating *
mod_35<- lm(formula = price ~ enginelocation + 
              carwidth + 
              horsepower + 
              CarNameBMW + CarNameBUICK + 
              CarNamePORSCHE + 
              CarNameJAGUAR, data = trng_set)

vif(mod_35)
summary(mod_35)

###Model Evaluation
###Finally we have arrived at the final model [mod_35] with R-squared=0.9354 and Adjusted R-Squared=0.9325
#Since both R-Squared and Adjusted R-Squared values are immensly close together this model can be tested for the final model

#1. Using the mod_35 to predict the price of cars in the testing dataset.
test_set$predicted_price<- predict(mod_35, test_set[,-18])
#We are storing the predicted prices of the test dataset in a new column called predicted_price

#Now we will calcuulate the correlation between the predicted_price and the original price of the vehicle. We shall store the results in evaluation_correlation.
evaluation_correlation<- cor(test_set$price, test_set$predicted_price, use = "everything")
evaluation_correlation

#The evaluation_correlation is equal to 0.9519. Which shows that our predicted prices are very close to the actual price.
evaluation_rsquare<- round((evaluation_correlation)^2, 3)
evaluation_rsquare

#Since the evaluation_rsquare is 0.906 stating that our model can account for 90.1% of the variability in prices of the vehicles. We can say that the model is a very good model to predict price of the automobile.

#2.#ref: https://learn.upgrad.com/v/course/77/session/10792/segment/53232 Mrs. UJJYANI MITRA says that we have prove that the error term generated is randomly distributed as white noise. If it is randomly distributed then we can say that there are no more variables to be added to the dataset.
#We calculate error by Actual-Predicted Price
test_set$error_pred<- test_set$price - test_set$predicted_price
#Create a carID variable to assign a random unique ID to each row in the dataset  
test_set$carID<- sample(1:nrow(test_set), nrow(test_set), replace = FALSE)

#Plotting the Error in Prediction versus the Car_ID. This is to show the randomness in the prediction error generated.
error_noise<- ggplot(test_set, aes(x=test_set$carID, y=test_set$error_pred))+geom_point()+ xlab("Car_ID")+ ylab("Predicted Price Error") + ggtitle("Plt27. Plot Showing Random distribution of Predicted Error")
error_noise

#From the Scatter_Plot there is no ternd in the error therefore we can stop our modelling process and assign the final model to mod_35


####Addressing Goals of Analysis and Suggestions: 
#[1] Which Variables are significant in predicting the price of a car?

#From out final prediction model we have identified that enginelocation, carwidth, horsepower, CarNameBMW, CarNameBUICK, CarNamePORSCHE and CarNameJAGUAR are significant in predicting the the price of the car. 
#From this regression model it is clear that the price of the vehicle is not entirely dependedent on the parameters of the vehicle but rather the brand of the vehicles severly impact the price. 
#I have also tried to re-insert every predictor variable from the initial model to check if any more significant variables can be added to the prediction. It was identified that [carbodyHATCHBACK, carlength, carheight, drivewheelRWD when introduced had a 1* significance rating> However, I have removed it to maintain integrity of the model]
#Geely Auto can now invest more time in studying the vehicles and the price point of vehicles from each of these significant car companies and decide their business stratergy.

#[2] How well these variables describe the price of the car?

#From the evaluation_rsquare when the aforementioned significant variables were used to predict the price of the testing dataset. It could account for 90.1% of the variability in the price. This is a good model.

#[3] Build a regression model to understand the pricing dynamics of automobiles in the American Market. This will be used to drive business decisions and the car design to cater to specific price levels. 

# The built and evaluated model is mod_35.
vif(mod_35)
summary(mod_35)