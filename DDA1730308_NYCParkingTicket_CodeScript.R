#Title: New York City Parking Ticket Analysis 

#Coded By: Code Ninjas [1. Alvin Mark Windsor,2. Uday Balerao,3. Anand Agarwal]
#Brief: This Case Study is centered on the concepts of Ingesting and Analyzing big data on the Spark-R platform. Spark will allow us to analyse the full files at high speeds, as opposed to taking a series of random samples that will approximate the population.
#Dataset: The dataset used for analysis consists of the Parking Violation Ticket details issued by the NYC Police Department for the fiscal year of 2015, 2016 and 2017.
#Our Analysis is focussed on performing exploratory analysis to understand the parking violation dataset. We will compare metrics/phenomenon related to Parking Tickets over three different years-2015,2016 and 2017. The Analysis will be performed on the R-studio interface using SparkR package running on an AWS cluster.

#DataSet Access: The dataset is open source and available at Kaggle.com for download using the following links:
#2015 Parking Ticket Data: https://www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2015.csv/2
#2016 Parking Ticket Data: https://www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2016.csv/2
#2017 Parking Ticket Data: https://www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2017.csv/2

#The Data Dictionary for this dataset is as follows: [Field Name--Description]
#[1] Summons_Number--Unique Identifier Of Summons [Primary Key]   
#[2] Plate_Id--Registered Plate Id     
#[3] Registration_State--State Of Plate Registration    
#[4] Plate_Type--Type Of Plate     
#[5] Issue_Date--Issue Date      
#[6] Violation_Code--Type Of Violation     
#[7] Summ_Veh_Body--Vehicle Body Type Written On Summons (Sedan, Etc.)
#[8] Summ_Veh_Make--Make Of Car Written On Summons  
#[9] Issuing_Agency--Issuing Agency Code     
#[10] Street_Code1--Geographical Street Code     
#[11] Street_Code2--Geographical Street Code     
#[12] Street_Code3--Geographical Street Code     
#[13] Vehicle_Expiration_Date--Vehicle Expiration Date On Summons   
#[14] Violation_Location--General Violation Location     
#[15] Violation_Precinct--Precinct Of Violation     
#[16] Issuer_Precinct--Precinct Of Issuance     
#[17] Issuer_Code--Unique Code Identifying Issuing Officer   
#[18] Issuer_Command--Command Of Issuing Officer    
#[19] Issuer_Squad--Issuing Officer'S Squad     
#[20] Violation_Time--Time Violation Occurred     
#[21] Time_First_Observed--Time Of Initial Violation Observation   
#[22] Violation_County--County Of Violation     
#[23] Front_Of_Opposite--Violation In Front Of Or Opposite  
#[24] House_Number--Address Number Of Violation    
#[25] Street_Name--Street Name Of Summons Issued   
#[26] Intersecting_Street--Violation Near Intersecting Street    
#[27] Date_First_Observed--Date Of Initial Violation Observation   
#[28] Law_Section--Section Of Vehicle & Traffic Law  
#[29] Sub_Division--Sub Division On Summons Image   
#[30] Violation_Legal_Code--Type Of Legal Code    
#[31] Days_In_Effect--Days Parking In Effect    
#[32] From_Hours_In_Effect--Start Time Posted For Meter And Sign Violations
#[33] To_Hours_In_Effect--End Time Posted For Meter And Sign Violations
#[34] Vehicle_Color--Car Color Written On Summons   
#[35] Unregistered_Vehicle?--Unregistered Vehicle Indicator     
#[36] Vehicle_Year--Year Of Vehicle Written On Summons  
#[37] Meter_Number--Parking Meter Number For Meter Violations  
#[38] Feet_From--Number Of Feet From Curb For Hydrant Violation
#[39] Violation_Post_Code--Location Post Code Of Violation   
#[40] Violation_Description--Description Of Violation     
#[41] No_Standing_Or_Stopping_Violation--Summons Indictor For No Standing Or Stopping 
#[42] Hydrant_Violation--Summons Indicator For Hydrant Violation   
#[43] Double_Parking_Violation--Summons Indicator For Double Parking Violation  

#Pre-Requisites for Analysis:
#[1] Create a suitable S3 bucket. I created a bucket called amwparkingticketnyc and created a file rawdatafiles to store the initial Dataset.
#[2] Upload the dataset into S3 using the AWS CLI: Create a cluster and access the master node through the CLI. Download the datasets into a suitable directory on the master using the [sudo wget <paste link>] command [Note: We had to use a Chrome Plugin called CurlWget to circumvent the permissions issue on Kaggle.com]. Then using the [sudo aws s3 cp <file name> <bucket name>] command transfer the dataset into the desired S3 bucket.
#[3] Launch the R-Studio interface from the master node and load the SparkR library. Execute the commands on a R-Script file.

#Methodology for Analysis: We will conduct the analysis in 3 stages.
#Stage 1: Data Quality Verification and Cleaning
#Stage 2: Overview and Examining the dataset
#Stage 3: Deriving and Comparing Metrics through Aggregation Tasks.

#Assumptions during Analysis:
#Read the Attached Output file for a very Detailed Explanation of the Assumptions and Data Sanitary Checks Performed prior to Analysis.

#******************** Stage 1: Data Quality Verification and Cleaning ********************#

#1.1. Loading the necessary packages into the R-enviornment
library(SparkR)
library(dplyr)
library(stringr)
library(ggplot2)

#1.2. Initializing the Spark session.
sparkR.session(master='local')

#1.3. Importing the dataset files into specific dataframes
tkt_2015_nycparking <- read.df(path="s3://amwparkingticketnyc/rawdatafiles/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", source= "csv", inferSchema= "true", header= "true")
tkt_2016_nycparking <- read.df(path="s3://amwparkingticketnyc/rawdatafiles/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", source= "csv", inferSchema= "true", header= "true")
tkt_2017_nycparking <- read.df(path="s3://amwparkingticketnyc/rawdatafiles/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", source= "csv", inferSchema= "true", header= "true")

#1.4. Fixing Column Names for the imported Datasets.
#Since Column Names of the dataframes have spaces in between consecutive words it may lead to typos and coding issues. Therefore, we will eliminate all white spaces and seperate consecutive words with "_"
colnames(tkt_2015_nycparking)<- str_trim(colnames(tkt_2015_nycparking), side= "both")
colnames(tkt_2015_nycparking)<- str_replace_all(colnames(tkt_2015_nycparking), pattern=" ", replacement = "_")
colnames(tkt_2015_nycparking)<- str_replace_all(colnames(tkt_2015_nycparking), pattern="\\?", replacement = "")
colnames(tkt_2015_nycparking)

colnames(tkt_2016_nycparking)<- str_trim(colnames(tkt_2016_nycparking), side= "both")
colnames(tkt_2016_nycparking)<- str_replace_all(colnames(tkt_2016_nycparking), pattern=" ", replacement = "_")
colnames(tkt_2016_nycparking)<- str_replace_all(colnames(tkt_2016_nycparking), pattern="\\?", replacement = "")
colnames(tkt_2016_nycparking)

colnames(tkt_2017_nycparking)<- str_trim(colnames(tkt_2017_nycparking), side= "both")
colnames(tkt_2017_nycparking)<- str_replace_all(colnames(tkt_2017_nycparking), pattern=" ", replacement = "_")
colnames(tkt_2017_nycparking)<- str_replace_all(colnames(tkt_2017_nycparking), pattern="\\?", replacement = "")
colnames(tkt_2017_nycparking)

#1.5. Understanding the dimensions and structure of the the Imported Dataset tkt_2015_nycparking
head(tkt_2015_nycparking)
dim(tkt_2015_nycparking)
#2015 Dataset has Rows: 11,809,233 | Columns: 51
printSchema(tkt_2015_nycparking)
#From the Schema and Datatypes of each column there are several non-conforming columns like Issue_Date, Vehicle_Expiration_Date, Violation Time, Date_First_Observed etc.
#These columns need to be filtered and transformed into a suitable format for processing. We will explore any further discrepancies in the individual EDA.


#1.6. Understanding the dimensions and structure of the the Imported Dataset tkt_2016_nycparking
head(tkt_2016_nycparking)
dim(tkt_2016_nycparking)
#2016 Dataset has Rows: 10,626,899 | Columns: 51
printSchema(tkt_2016_nycparking)
#From the Schema and Datatypes of each column there are several non-conforming columns like Issue_Date, Vehicle_Expiration_Date, Violation Time, Date_First_Observed etc.
#These columns need to be filtered and transformed into a suitable format for processing. We will explore any further discrepancies in the individual EDA.


#1.7. Understanding the dimensions and structure of the the Imported Dataset tkt_2017_nycparking
head(tkt_2017_nycparking)
dim(tkt_2017_nycparking)
#2017 Dataset has Rows: 10,803,028 | Columns: 43
printSchema(tkt_2017_nycparking)
#From the Schema and Datatypes of each column there are several non-conforming columns like Issue_Date, Vehicle_Expiration_Date, Violation Time, Date_First_Observed etc.
#These columns need to be filtered and transformed into a suitable format for processing. We will explore any further discrepancies in the individual EDA.

#From Sections 1.5.,1.6. and 1.7. it is clear that the dataframes have varying number of columns. As the columns "Latitude", "Longitude", "Community_Board", "Community_Council", "Census_Tract", "BIN", "BBL" and "NTA" are logged for the fiscal year 2015, 2016 but not for 2017. We will look into the feasibility of removing these columns during the detailed analysis.


#************************#

#1.8. Detailed Data Quality Verification of 2015 NYC Parking Ticket Dataset
#1.8.1. Removing any duplicate rows in the dataset [Two or More rows having the same Summons_Number ]
tkt_2015_nycparking<- dropDuplicates(tkt_2015_nycparking, "Summons_Number")
dim(tkt_2015_nycparking)
# After Removing Duplicate records of 2015 dataset. The dimensions are as Follows: Rows: 10,951,256 | Columns: 51

#1.8.2 Since ticket Issue Date is an critical parameter to assess the quality of the Dataset. Let us check if there are any missing values in the Issue Date Parameter.
createOrReplaceTempView(tkt_2015_nycparking, "tkt_2015_nyc")
CountNull_IssueDate_2015 <- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                        THEN 1
                                        ELSE 0
                                        END) nulls_Issue_Date,
                                        COUNT(*) Num_of_Rows
                                        FROM tkt_2015_nyc")
head(CountNull_IssueDate_2015)
#There are no records with missing Issue Date or Null Issue Date

#1.8.3.1 Converting the Date Paramters[Issue Date, Vehicle Expiration Date and Date First Observed] to a suitable format for Analysis.
tkt_2015_nycparking$Issue_Date <- SparkR::to_date(tkt_2015_nycparking$Issue_Date, 'MM/dd/yyyy')
tkt_2015_nycparking$Vehicle_Expiration_Date <- SparkR::to_date(tkt_2015_nycparking$Vehicle_Expiration_Date, 'yyyyMMdd')
tkt_2015_nycparking$Date_First_Observed <- SparkR::to_date(tkt_2015_nycparking$Date_First_Observed, 'yyyyMMdd')

#1.8.3.2 Let's Understand the Range of ticket Issue Dates Available in the Dataset
createOrReplaceTempView(tkt_2015_nycparking, "tkt_2015_nyc")
Range_Issue_Date_2015 <- SparkR::sql("SELECT min(issue_date)as Min_IssueDate_2015,
                                     max(issue_date)as Max_IssueDate_2015
                                     FROM tkt_2015_nyc")
head(Range_Issue_Date_2015)
# Min_IssueDate_2015 : 1985-07-16
# Max_IssueDate_2015 : 2015-06-30
# The Issue Tickets range between 16th July 1985 to 30th June 2015. Clearly this is Nonconforming. Let us Analyze this parameter closely to decide optimal filter condition.

#1.8.3.3 We Will create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue
tkt_2015_nycparking$Issue_Year <- year(tkt_2015_nycparking$Issue_Date)
tkt_2015_nycparking$Issue_Month <- month(tkt_2015_nycparking$Issue_Date)

#Now let's observe the Distribution of Issue Date.
createOrReplaceTempView(tkt_2015_nycparking, "tkt_2015_nyc")
Grouped_Issue_Date_2015 <- SparkR::sql("SELECT Issue_Year,
                                       Issue_Month,
                                       count(*)as Num_of_Records
                                       FROM tkt_2015_nyc
                                       GROUP BY Issue_Year,
                                       Issue_Month
                                       ORDER BY 1,2")

#Note:Run the Above Command Then Run the dfgrouped_issue_ym_2015 command
dfgrouped_issue_ym_2015 <- data.frame(head(Grouped_Issue_Date_2015, nrow(Grouped_Issue_Date_2015)))
View(dfgrouped_issue_ym_2015)

#1.8.3.4 Subsetting the DataFrame according to the Fiscal Year [Read Assumptions For Justification]
# Considering A Fiscal Year to extend from the July of Pervious Year to June of the Current Year.

tkt_2015_nycparking <- tkt_2015_nycparking[
  tkt_2015_nycparking$Issue_Date >= "2014-07-01" & 
    tkt_2015_nycparking$Issue_Date <= "2015-06-30"]

nrow(tkt_2015_nycparking)
# 10,598,035 records in filtered dataset

#1.8.4 The columns "Latitude", "Longitude", "Community_Board", "Community_Council", "Census_Tract", "BIN", "BBL" and "NTA" are logged for the fiscal year 2015 and 2016 but not for 2017. We will Check the Number of Null values in the aforementioned columns in tkt_2015_nycparking. 
createOrReplaceTempView(tkt_2015_nycparking, "tkt_2015_nyc")
CountNull_ExtraCol_2015 <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                       SUM(CASE WHEN Plate_ID IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Plate_ID,
                                       SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_stand_stop,
                                       SUM(CASE WHEN Hydrant_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Hydrant_Violation,
                                       SUM(CASE WHEN Double_Parking_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Double_Parking_Violation,
                                       SUM(CASE WHEN Latitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Latitude,
                                       SUM(CASE WHEN Longitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Longitude,
                                       SUM(CASE WHEN Community_Board IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Board,
                                       SUM(CASE WHEN Community_Council IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Council,
                                       SUM(CASE WHEN Census_Tract IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Census_Tract,
                                       SUM(CASE WHEN BIN IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BIN,
                                       SUM(CASE WHEN BBL IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BBL,
                                       SUM(CASE WHEN NTA IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_NTA     
                                       FROM tkt_2015_nyc")
head(CountNull_ExtraCol_2015)
#There are 129 records with null registration plate ID which is managable. However, all the additional columns have only null values therefore they will be dropped to standardize the dataset between the years.

#Removing these Null Columns.
tkt_2015_nycparking<- drop(tkt_2015_nycparking, c("No_Standing_or_Stopping_Violation", "Hydrant_Violation", "Double_Parking_Violation", "Latitude","Longitude","Community_Board", "Community_Council","Census_Tract","BIN" , "BBL",  "NTA") )
colnames(tkt_2015_nycparking)

#1.8.5 Fixing the format for the Violation Time [Time First Observed, From Hours in Effect and To Hours in Effect are also converted.]

#We can observe that the string format of the time attributes include only a partial component of AM/PM. Therefore we will append M to the end of each time attribute before converting it into a timestamp.
tkt_2015_nycparking$Concat_M <- "M"
tkt_2015_nycparking$Violation_Time<-concat(tkt_2015_nycparking$Violation_Time,  tkt_2015_nycparking$Concat_M)
tkt_2015_nycparking$Time_First_Observed<- concat(tkt_2015_nycparking$Time_First_Observed, tkt_2015_nycparking$Concat_M)
tkt_2015_nycparking$From_Hours_In_Effect<- concat(tkt_2015_nycparking$From_Hours_In_Effect, tkt_2015_nycparking$Concat_M)
tkt_2015_nycparking$To_Hours_In_Effect<- concat(tkt_2015_nycparking$To_Hours_In_Effect, tkt_2015_nycparking$Concat_M)
tkt_2015_nycparking<- drop(tkt_2015_nycparking, c("Concat_M"))

#Since we are conducting an Analysis with Violation Time we will look into this attribute a littl closer.
#Extracting Violation Hour, Violation Minute and Part of Day.
tkt_2015_nycparking$Violation_Hour <- substr(tkt_2015_nycparking$Violation_Time, 1, 2)
tkt_2015_nycparking$Violation_Minute <- substr(tkt_2015_nycparking$Violation_Time, 4, 5)
tkt_2015_nycparking$Violation_AMPM <- substr(tkt_2015_nycparking$Violation_Time, 6, 7)

#We've observed that there are records that have both 00xxAM as well as 12xxAM. Therefore we will replace all 00xxAM with 12xxAM
tkt_2015_nycparking$Violation_Hour <- regexp_replace(x = tkt_2015_nycparking$Violation_Hour,pattern = "\\00",replacement = "12")

#Concatenating the components into a standardized Violation Time.
tkt_2015_nycparking$Violation_Time <- concat(tkt_2015_nycparking$Violation_Hour, tkt_2015_nycparking$Violation_Minute, tkt_2015_nycparking$Violation_AMPM)

#Converting Violation Time into a TimeStamp
tkt_2015_nycparking$Violation_Time<-to_timestamp(x = tkt_2015_nycparking$Violation_Time, format = "hhmma")

#Converting the other time attributes into a TimeStamp.
tkt_2015_nycparking$Time_First_Observed<- to_timestamp(x= tkt_2015_nycparking$Time_First_Observed, format = "hhmma")
tkt_2015_nycparking$From_Hours_In_Effect<- to_timestamp(x= tkt_2015_nycparking$From_Hours_In_Effect, format = "hhmma")
tkt_2015_nycparking$To_Hours_In_Effect<- to_timestamp(x= tkt_2015_nycparking$To_Hours_In_Effect, format = "hhmma")

#1.8.6 The dimensions of Formatted and Cleaned Dataset that will be used for Analysis:
head(tkt_2015_nycparking)
dim(tkt_2015_nycparking)
#2015 Dataset has Rows: 10,598,035 | Columns: 45
printSchema(tkt_2015_nycparking)
#All attributes are in standardized formats
#************************#

#1.9. Detailed Data Quality Verification of 2016 NYC Parking Ticket Dataset
#1.9.1. Removing any duplicate rows in the dataset [Two or More rows having the same Summons_Number ]
tkt_2016_nycparking<- dropDuplicates(tkt_2016_nycparking, "Summons_Number")
dim(tkt_2016_nycparking)
# After Removing Duplicate records of 2016 dataset. The dimensions are as Follows: Rows: 10,626,899 | Columns: 51

#1.9.2 Since ticket Issue Date is an critical parameter to assess the quality of the Dataset. Let us check if there are any missing values in the Issue Date Parameter.
createOrReplaceTempView(tkt_2016_nycparking, "tkt_2016_nyc")
CountNull_IssueDate_2016 <- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                        THEN 1
                                        ELSE 0
                                        END) nulls_Issue_Date,
                                        COUNT(*) Num_of_Rows
                                        FROM tkt_2016_nyc")
head(CountNull_IssueDate_2016)
#There are no records with missing or null Issue Dates.

#1.9.3.1 Converting the Date Paramters[Issue Date, Vehicle Expiration Date and Date First Observed] to a suitable format for Analysis.
tkt_2016_nycparking$Issue_Date <- SparkR::to_date(tkt_2016_nycparking$Issue_Date, 'MM/dd/yyyy')
tkt_2016_nycparking$Vehicle_Expiration_Date <- SparkR::to_date(cast(tkt_2016_nycparking$Vehicle_Expiration_Date,"string"), 'yyyyMMdd')
tkt_2016_nycparking$Date_First_Observed <- SparkR::to_date(cast(tkt_2016_nycparking$Date_First_Observed,"string"), 'yyyyMMdd')

#1.9.3.2 Let's Understand the Range of ticket Issue Dates Available in the Dataset
createOrReplaceTempView(tkt_2016_nycparking, "tkt_2016_nyc")
Range_Issue_Date_2016 <- SparkR::sql("SELECT min(issue_date)as Min_IssueDate_2016,
                                     max(issue_date)as Max_IssueDate_2016
                                     FROM tkt_2016_nyc")
head(Range_Issue_Date_2016)
# Min_IssueDate_2016 : 1970-04-13
# Max_IssueDate_2016 : 2069-10-02
# The Issue Tickets range between 13th April 1970 to 2nd October 2069. Clearly this is Nonconforming. Let us Analyze this parameter closely to decide optimal filter condition.

#1.9.3.3 We Will create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue
tkt_2016_nycparking$Issue_Year <- year(tkt_2016_nycparking$Issue_Date)
tkt_2016_nycparking$Issue_Month <- month(tkt_2016_nycparking$Issue_Date)

#Now let's observe the Distribution of Issue Date.
createOrReplaceTempView(tkt_2016_nycparking, "tkt_2016_nyc")
Grouped_Issue_Date_2016 <- SparkR::sql("SELECT Issue_Year,
                                       Issue_Month,
                                       count(*)as Num_of_Records
                                       FROM tkt_2016_nyc
                                       GROUP BY Issue_Year,
                                       Issue_Month
                                       ORDER BY 1,2")

#Note:Run the Above Command Then Run the dfgrouped_issue_ym_2016 command

dfgrouped_issue_ym_2016 <- data.frame(head(Grouped_Issue_Date_2016, nrow(Grouped_Issue_Date_2016)))
View(dfgrouped_issue_ym_2016)

#1.9.3.4 Subsetting the DataFrame according to the Fiscal Year [Read Assumptions For Justification]
# Considering A Fiscal Year to extend from the July of Pervious Year to June of the Current Year.

tkt_2016_nycparking <- tkt_2016_nycparking[
  tkt_2016_nycparking$Issue_Date >= "2015-07-01" & 
    tkt_2016_nycparking$Issue_Date <= "2016-06-30"]

nrow(tkt_2016_nycparking)
# 10,396,894 records in the filtered dataset

#1.9.4 The columns "Latitude", "Longitude", "Community_Board", "Community_Council", "Census_Tract", "BIN", "BBL" and "NTA" are logged for the fiscal year 2015 and 2016 but not for 2017. We will Check the Number of Null values in the aforementioned columns in tkt_2016_nycparking. 
createOrReplaceTempView(tkt_2016_nycparking, "tkt_2016_nyc")
CountNull_ExtraCol_2016 <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                       SUM(CASE WHEN Plate_ID IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Plate_ID,
                                       SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_stand_stop,
                                       SUM(CASE WHEN Hydrant_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Hydrant_Violation,
                                       SUM(CASE WHEN Double_Parking_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Double_Parking_Violation,
                                       SUM(CASE WHEN Latitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Latitude,
                                       SUM(CASE WHEN Longitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Longitude,
                                       SUM(CASE WHEN Community_Board IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Board,
                                       SUM(CASE WHEN Community_Council IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Council,
                                       SUM(CASE WHEN Census_Tract IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Census_Tract,
                                       SUM(CASE WHEN BIN IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BIN,
                                       SUM(CASE WHEN BBL IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BBL,
                                       SUM(CASE WHEN NTA IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_NTA     
                                       FROM tkt_2016_nyc")
head(CountNull_ExtraCol_2016)
#There are 20 records with null registration plate ID which is managable. However, all the additional columns have only null values therefore they will be dropped to standardize the dataset between the years.

#Removing these Null Columns.
tkt_2016_nycparking<- drop(tkt_2016_nycparking, c("No_Standing_or_Stopping_Violation", "Hydrant_Violation", "Double_Parking_Violation", "Latitude","Longitude","Community_Board", "Community_Council","Census_Tract","BIN" , "BBL",  "NTA") )
colnames(tkt_2016_nycparking)

#1.9.5 Fixing the format for the Violation Time [Time First Observed, From Hours in Effect and To Hours in Effect are also converted.]

#We can observe that the string format of the time attributes include only a partial component of AM/PM. Therefore we will append M to the end of each time attribute before converting it into a timestamp.
tkt_2016_nycparking$Concat_M <- "M"
tkt_2016_nycparking$Violation_Time<-concat(tkt_2016_nycparking$Violation_Time,  tkt_2016_nycparking$Concat_M)
tkt_2016_nycparking$Time_First_Observed<- concat(tkt_2016_nycparking$Time_First_Observed, tkt_2016_nycparking$Concat_M)
tkt_2016_nycparking$From_Hours_In_Effect<- concat(tkt_2016_nycparking$From_Hours_In_Effect, tkt_2016_nycparking$Concat_M)
tkt_2016_nycparking$To_Hours_In_Effect<- concat(tkt_2016_nycparking$To_Hours_In_Effect, tkt_2016_nycparking$Concat_M)
tkt_2016_nycparking<- drop(tkt_2016_nycparking, c("Concat_M"))

#Since we are conducting an Analysis with Violation Time we will look into this attribute a littl closer.
#Extracting Violation Hour, Violation Minute and Part of Day.
tkt_2016_nycparking$Violation_Hour <- substr(tkt_2016_nycparking$Violation_Time, 1, 2)
tkt_2016_nycparking$Violation_Minute <- substr(tkt_2016_nycparking$Violation_Time, 4, 5)
tkt_2016_nycparking$Violation_AMPM <- substr(tkt_2016_nycparking$Violation_Time, 6, 7)

#We've observed that there are records that have both 00xxAM as well as 12xxAM. Therefore we will replace all 00xxAM with 12xxAM
tkt_2016_nycparking$Violation_Hour <- regexp_replace(x = tkt_2016_nycparking$Violation_Hour,pattern = "\\00",replacement = "12")

#Concatenating the components into a standardized Violation Time.
tkt_2016_nycparking$Violation_Time <- concat(tkt_2016_nycparking$Violation_Hour, tkt_2016_nycparking$Violation_Minute, tkt_2016_nycparking$Violation_AMPM)

#Converting Violation Time into a TimeStamp
tkt_2016_nycparking$Violation_Time<-to_timestamp(x = tkt_2016_nycparking$Violation_Time, format = "hhmma")

#Converting the other time attributes into a TimeStamp.
tkt_2016_nycparking$Time_First_Observed<- to_timestamp(x= tkt_2016_nycparking$Time_First_Observed, format = "hhmma")
tkt_2016_nycparking$From_Hours_In_Effect<- to_timestamp(x= tkt_2016_nycparking$From_Hours_In_Effect, format = "hhmma")
tkt_2016_nycparking$To_Hours_In_Effect<- to_timestamp(x= tkt_2016_nycparking$To_Hours_In_Effect, format = "hhmma")

#1.9.6 The dimensions of Formatted and Cleaned Dataset that will be used for Analysis:
head(tkt_2016_nycparking)
dim(tkt_2016_nycparking)
#2016 Dataset has Rows: 10,396,894 | Columns: 45
printSchema(tkt_2016_nycparking)
#All attributes are in standardized formats

#************************#

#1.10. Detailed Data Quality Verification of 2017 NYC Parking Ticket Dataset
#1.10.1. Removing any duplicate rows in the dataset [Two or More rows having the same Summons_Number ]
tkt_2017_nycparking<- dropDuplicates(tkt_2017_nycparking, "Summons_Number")
dim(tkt_2017_nycparking)
# After Removing Duplicate records of 2017 dataset. The dimensions are as Follows: Rows: 10,803,028 | Columns: 43

#1.10.2 Since ticket Issue Date is an critical parameter to assess the quality of the Dataset. Let us check if there are any missing values in the Issue Date Parameter.
createOrReplaceTempView(tkt_2017_nycparking, "tkt_2017_nyc")
CountNull_IssueDate_2017 <- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                        THEN 1
                                        ELSE 0
                                        END) nulls_Issue_Date,
                                        COUNT(*) Num_of_Rows
                                        FROM tkt_2017_nyc")
head(CountNull_IssueDate_2017)
#There are no records with missing or null Issue Date

#1.10.3.1 Converting the Date Paramters[Issue Date, Vehicle Expiration Date and Date First Observed] to a suitable format for Analysis.
tkt_2017_nycparking$Issue_Date <- SparkR::to_date(tkt_2017_nycparking$Issue_Date, 'MM/dd/yyyy')
tkt_2017_nycparking$Vehicle_Expiration_Date <- SparkR::to_date(cast(tkt_2017_nycparking$Vehicle_Expiration_Date,"string"), 'yyyyMMdd')
tkt_2017_nycparking$Date_First_Observed <- SparkR::to_date(cast(tkt_2017_nycparking$Date_First_Observed,"string"), 'yyyyMMdd')

#1.10.3.2 Let's Understand the Range of ticket Issue Dates Available in the Dataset
createOrReplaceTempView(tkt_2017_nycparking, "tkt_2017_nyc")
Range_Issue_Date_2017 <- SparkR::sql("SELECT min(issue_date)as Min_IssueDate_2017,
                                     max(issue_date)as Max_IssueDate_2017
                                     FROM tkt_2017_nyc")
head(Range_Issue_Date_2017)
# Min_IssueDate_2017 : 1972-03-30
# Max_IssueDate_2017 : 2069-11-19
# The Issue Tickets range between 30th March 1972 to 19th November 2069. Clearly this is Nonconforming. Let us Analyze this parameter closely to decide optimal filter condition.


#1.10.3.3 We Will create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue
tkt_2017_nycparking$Issue_Year <- year(tkt_2017_nycparking$Issue_Date)
tkt_2017_nycparking$Issue_Month <- month(tkt_2017_nycparking$Issue_Date)

#Now let's observe the Distribution of Issue Date.
createOrReplaceTempView(tkt_2017_nycparking, "tkt_2017_nyc")
Grouped_Issue_Date_2017 <- SparkR::sql("SELECT Issue_Year,
                                       Issue_Month,
                                       count(*)as Num_of_Records
                                       FROM tkt_2017_nyc
                                       GROUP BY Issue_Year,
                                       Issue_Month
                                       ORDER BY 1,2")

#Note:Run the Above Command Then Run the dfgrouped_issue_ym_2017 command

dfgrouped_issue_ym_2017 <- data.frame(head(Grouped_Issue_Date_2017, nrow(Grouped_Issue_Date_2017)))
View(dfgrouped_issue_ym_2017)

#1.10.3.4 Subsetting the DataFrame according to the Fiscal Year [Read Assumptions For Justification]
# Considering A Fiscal Year to extend from the 1st July of Pervious Year to 30th June of the Current Year.

tkt_2017_nycparking <- tkt_2017_nycparking[
  tkt_2017_nycparking$Issue_Date >= "2016-07-01" & 
    tkt_2017_nycparking$Issue_Date <= "2017-06-30"]

nrow(tkt_2017_nycparking)
#10,539,563 records after filtering on Issue Date

#1.10.4 The columns "No_Standing_or_Stopping_Violation", "Hydrant_Violation", "Double_Parking_Violation" We will Check the Number of Null values in the aforementioned columns in tkt_2017_nycparking. 
createOrReplaceTempView(tkt_2017_nycparking, "tkt_2017_nyc")
CountNull_ExtraCol_2017 <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                       SUM(CASE WHEN Plate_ID IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Plate_ID,
                                       SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_stand_stop,
                                       SUM(CASE WHEN Hydrant_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Hydrant_Violation,
                                       SUM(CASE WHEN Double_Parking_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Double_Parking_Violation
                                       from tkt_2017_nyc")
head(CountNull_ExtraCol_2017)
#There are 79 records with null registration plate ID which is managable. However, all the additional columns have only null values therefore they will be dropped to standardize the dataset between the years.

#Removing these Null Columns.
tkt_2017_nycparking<- drop(tkt_2017_nycparking, c("No_Standing_or_Stopping_Violation", "Hydrant_Violation", "Double_Parking_Violation"))
colnames(tkt_2017_nycparking)

#1.10.5 Fixing the format for the Violation Time [Time First Observed, From Hours in Effect and To Hours in Effect are also converted.]

#We can observe that the string format of the time attributes include only a partial component of AM/PM. Therefore we will append M to the end of each time attribute before converting it into a timestamp.
tkt_2017_nycparking$Concat_M <- "M"
tkt_2017_nycparking$Violation_Time<-concat(tkt_2017_nycparking$Violation_Time,  tkt_2017_nycparking$Concat_M)
tkt_2017_nycparking$Time_First_Observed<- concat(tkt_2017_nycparking$Time_First_Observed, tkt_2017_nycparking$Concat_M)
tkt_2017_nycparking$From_Hours_In_Effect<- concat(tkt_2017_nycparking$From_Hours_In_Effect, tkt_2017_nycparking$Concat_M)
tkt_2017_nycparking$To_Hours_In_Effect<- concat(tkt_2017_nycparking$To_Hours_In_Effect, tkt_2017_nycparking$Concat_M)
tkt_2017_nycparking<- drop(tkt_2017_nycparking, c("Concat_M"))

#Since we are conducting an Analysis with Violation Time we will look into this attribute a littl closer.
#Extracting Violation Hour, Violation Minute and Part of Day.
tkt_2017_nycparking$Violation_Hour <- substr(tkt_2017_nycparking$Violation_Time, 1, 2)
tkt_2017_nycparking$Violation_Minute <- substr(tkt_2017_nycparking$Violation_Time, 4, 5)
tkt_2017_nycparking$Violation_AMPM <- substr(tkt_2017_nycparking$Violation_Time, 6, 7)

#We've observed that there are records that have both 00xxAM as well as 12xxAM. Therefore we will replace all 00xxAM with 12xxAM
tkt_2017_nycparking$Violation_Hour <- regexp_replace(x = tkt_2017_nycparking$Violation_Hour,pattern = "\\00",replacement = "12")

#Concatenating the components into a standardized Violation Time.
tkt_2017_nycparking$Violation_Time <- concat(tkt_2017_nycparking$Violation_Hour, tkt_2017_nycparking$Violation_Minute, tkt_2017_nycparking$Violation_AMPM)

#Converting Violation Time into a TimeStamp
tkt_2017_nycparking$Violation_Time<-to_timestamp(x = tkt_2017_nycparking$Violation_Time, format = "hhmma")

#Converting the other time attributes into a TimeStamp.
tkt_2017_nycparking$Time_First_Observed<- to_timestamp(x= tkt_2017_nycparking$Time_First_Observed, format = "hhmma")
tkt_2017_nycparking$From_Hours_In_Effect<- to_timestamp(x= tkt_2017_nycparking$From_Hours_In_Effect, format = "hhmma")
tkt_2017_nycparking$To_Hours_In_Effect<- to_timestamp(x= tkt_2017_nycparking$To_Hours_In_Effect, format = "hhmma")

#1.10.6 The dimensions of Formatted and Cleaned Dataset that will be used for Analysis:
head(tkt_2017_nycparking)
dim(tkt_2017_nycparking)
#2017 Dataset has Rows: 10,539,563 | Columns: 45
printSchema(tkt_2017_nycparking)
#All attributes are in standardized formats.

#************************ Stage 2: Overview and Examining the dataset ************************#
#Creating temp views of each table
createOrReplaceTempView(tkt_2015_nycparking, "tkt_2015_nyc")
createOrReplaceTempView(tkt_2016_nycparking, "tkt_2016_nyc")
createOrReplaceTempView(tkt_2017_nycparking, "tkt_2017_nyc")

#************ Stage 2: Q1 ************#
#2.1.1 Find total number of tickets for each year!

Number_of_Tickets<- c(nrow(tkt_2015_nycparking), nrow(tkt_2016_nycparking), nrow(tkt_2017_nycparking))
Year_Labels<- c("FY_2015", "FY_2016", "FY_2017")
tickets_vs_year<- data.frame(Number_of_Tickets, Year_Labels)
tickets_vs_year
ggplot(tickets_vs_year, aes(x=Year_Labels, y=Number_of_Tickets))+ geom_col() + xlab("Fiscal Year") + ylab("Number of Tickets") + ggtitle("Plot1. Fiscal Year vs. Number of Tickets") + geom_text(aes(label=Number_of_Tickets),vjust=-0.3)

#************ Stage 2: Q2 ************#
#2.2. Find out how many unique states the cars which got parking tickets came from!

#2.2.1 Registration State Distribution for 2015
reg_st_2015<- SparkR::sql("SELECT Registration_State, count(*)as Number_of_Tickets 
                          from tkt_2015_nyc 
                          group by Registration_State
                          order by Number_of_Tickets desc")
head((reg_st_2015),nrow(reg_st_2015))

#2.2.2 Registration State Distribution for 2016
reg_st_2016<- SparkR::sql("SELECT Registration_State, count(*)as Number_of_Tickets 
                          from tkt_2016_nyc 
                          group by Registration_State
                          order by Number_of_Tickets desc")
head((reg_st_2016),nrow(reg_st_2016))

#2.2.3 Registration State Distribution for 2017
reg_st_2017<- SparkR::sql("SELECT Registration_State, count(*)as Number_of_Tickets 
                          from tkt_2017_nyc 
                          group by Registration_State
                          order by Number_of_Tickets desc")
head((reg_st_2017),nrow(reg_st_2017))

#2.2.4 Comparison Unique Registration States of cars issued with a Parking Ticket over the FY 2015, 2016 and 2017
Uniq_Reg_State_Count<- c(nrow(reg_st_2015), nrow(reg_st_2016), nrow(reg_st_2017))
Year_Labels<- c("FY_2015", "FY_2016", "FY_2017")
uniqregstate_vs_year<- data.frame(Uniq_Reg_State_Count, Year_Labels)
head(uniqregstate_vs_year)
#Let's see the results on a graph.
ggplot(uniqregstate_vs_year, aes(x=Year_Labels, y=Uniq_Reg_State_Count))+ geom_col() + xlab("Fiscal Year") + ylab("Count of Unique Registration States") + ggtitle("Plot2. Fiscal Year vs. Count of Unique Registration States") + geom_text(aes(label=Uniq_Reg_State_Count),vjust=-0.3)

#************ Stage 2: Q3 ************#
#2.3. Some parking tickets don’t have addresses on them, which is cause for concern. Find out how many such tickets there are!

#2.3.1. Missing Address in 2015
Missing_Address_2015 <- SparkR::sql("SELECT count(*) as Total_Num_Records, 
                                    SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL 
                                    THEN 1 
                                    ELSE 0 END)as Num_Tickets_2015_with_MissingAddress, 
                                    100*SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL
                                    THEN 1 
                                    ELSE 0 
                                    END)/count(*) as Percent_Tickets_2015_with_MissingAddress
                                    from tkt_2015_nyc")
head(Missing_Address_2015)
msingadd_2015<- data.frame(head(Missing_Address_2015))
msingadd_2015$Fiscal_Year<- 2015
colnames(msingadd_2015)<- c("Total_Num_Records", "Count_Tickets_with_MissingAddress", "Percent_Tickets_with_MissingAddress", "Fiscal_Year")


#2.3.2. Missing Address in 2016
Missing_Address_2016 <- SparkR::sql("SELECT count(*) as Total_Num_Records, 
                                    SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL 
                                    THEN 1 
                                    ELSE 0 END)as Num_Tickets_2016_with_MissingAddress, 
                                    100*SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL
                                    THEN 1 
                                    ELSE 0 
                                    END)/count(*) as Percent_Tickets_2016_with_MissingAddress
                                    from tkt_2016_nyc")
head(Missing_Address_2016)
msingadd_2016<- data.frame(head(Missing_Address_2016))
msingadd_2016$Fiscal_Year<- 2016
colnames(msingadd_2016)<- c("Total_Num_Records", "Count_Tickets_with_MissingAddress", "Percent_Tickets_with_MissingAddress", "Fiscal_Year")


#2.3.3. Missing Address in 2017
Missing_Address_2017 <- SparkR::sql("SELECT count(*) as Total_Num_Records, 
                                    SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL 
                                    THEN 1 
                                    ELSE 0 END)as Num_Tickets_2017_with_MissingAddress, 
                                    100*SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL
                                    THEN 1 
                                    ELSE 0 
                                    END)/count(*) as Percent_Tickets_2017_with_MissingAddress
                                    from tkt_2017_nyc")
head(Missing_Address_2017)
msingadd_2017<- data.frame(head(Missing_Address_2017))
msingadd_2017$Fiscal_Year<- 2017
colnames(msingadd_2017)<- c("Total_Num_Records", "Count_Tickets_with_MissingAddress", "Percent_Tickets_with_MissingAddress", "Fiscal_Year")

#2.3.4. Comparison of Missing Addresses
msingadd_combined<- rbind(msingadd_2015, msingadd_2016, msingadd_2017)
msingadd_combined
ggplot(msingadd_combined, aes(x=Fiscal_Year, y=Count_Tickets_with_MissingAddress))+ geom_col() + xlab("Fiscal Year") + ylab("Count of tickets with Missing Address") + ggtitle("Plot3. Fiscal Year vs. Count of Missing Address Tickets") + geom_text(aes(label=Count_Tickets_with_MissingAddress),vjust=-0.3)

#2.3.4B. Percentage of Tickets with Missing Addresses
ggplot(msingadd_combined, aes(x=Fiscal_Year, y=Percent_Tickets_with_MissingAddress))+ geom_col() + xlab("Fiscal Year") + ylab("Percentage of tickets with Missing Address") + ggtitle("Plot3B. Fiscal Year vs. Percentage of Missing Address Tickets") + geom_text(aes(label=Percent_Tickets_with_MissingAddress),vjust=-0.3)

#************************ Stage 3: Deriving and Comparing Metrics through Aggregation Tasks. ************************#

#************ Stage 3: Q1 ************#
#3.1. How often does each violation code occur? (frequency of violation codes - find the top 5)!

#3.1.1 Top Violation Codes for 2015
#2015
violationcd_frequency_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2015_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2015,5)

viocd_2015_top5<- data.frame(head(violationcd_frequency_2015,5))

ggplot(viocd_2015_top5, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot4A. 2015 Top 5 Violation Code vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.1.2 Top Violation Codes for 2016
violationcd_frequency_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2016_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2016,5)

viocd_2016_top5<- data.frame(head(violationcd_frequency_2016,5))

ggplot(viocd_2016_top5, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot4B. 2016 Top 5 Violation Code vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#3.1.3 Top Violation Codes for 2017
violationcd_frequency_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2017_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2017,5)

viocd_2017_top5<- data.frame(head(violationcd_frequency_2017,5))

ggplot(viocd_2017_top5, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot4C. 2017 Top 5 Violation Code vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.1.4 Combined Comparison of Top-5 Violation Codes
viocd_combined<- rbind(viocd_2015_top5, viocd_2016_top5, viocd_2017_top5)
viocd_combined$Year_Labels<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")
ggplot(viocd_combined, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Year_Labels) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot4. Comparison of Top 5 Violation Code vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#************ Stage 3: Q2 ************#
#How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)

#3.2.1.1 Top Vehicle Body Type for 2015
vehbdty_frequency_2015<- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as Frequency_of_Tickets
                                     from tkt_2015_nyc 
                                     group by Vehicle_Body_Type
                                     order by Frequency_of_Tickets desc")
head(vehbdty_frequency_2015,5)

vehbdty_2015_top5<- data.frame(head(vehbdty_frequency_2015,5))

ggplot(vehbdty_2015_top5, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("Plot5A. 2015 Top 5 Vehicle Body Type vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.2.1.2 Top Vehicle Body Type for 2016
vehbdty_frequency_2016<- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as Frequency_of_Tickets
                                     from tkt_2016_nyc 
                                     group by Vehicle_Body_Type
                                     order by Frequency_of_Tickets desc")
head(vehbdty_frequency_2016,5)

vehbdty_2016_top5<- data.frame(head(vehbdty_frequency_2016,5))

ggplot(vehbdty_2016_top5, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("Plot5B. 2016 Top 5 Vehicle Body Type vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.2.1.3 Top Vehicle Body Type for 2017
vehbdty_frequency_2017<- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as Frequency_of_Tickets
                                     from tkt_2017_nyc 
                                     group by Vehicle_Body_Type
                                     order by Frequency_of_Tickets desc")
head(vehbdty_frequency_2017,5)

vehbdty_2017_top5<- data.frame(head(vehbdty_frequency_2017,5))

ggplot(vehbdty_2017_top5, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("Plot5C. 2017 Top 5 Vehicle Body Type vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.2.1.4 Combined Comparison for Top-5 Body types
vehbdty_combined<- rbind(vehbdty_2015_top5, vehbdty_2016_top5, vehbdty_2017_top5)
vehbdty_combined$Year_Labels<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")
ggplot(vehbdty_combined, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Year_Labels) + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("Plot5. Comparison of Top 5 Violation Code vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.2.2.1 Top Vehicle Make for 2015
vehmake_frequency_2015<- SparkR::sql("SELECT Vehicle_Make, count(*)as Frequency_of_Tickets
                                     from tkt_2015_nyc 
                                     group by Vehicle_Make
                                     order by Frequency_of_Tickets desc")
head(vehmake_frequency_2015, 5)

vehmake_2015_top5<- data.frame(head(vehmake_frequency_2015, 5))

ggplot(vehmake_2015_top5, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("Plot6A. 2015 Top 5 Vehicle Make vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.2.2.2 Top Vehicle Make for 2016
vehmake_frequency_2016<- SparkR::sql("SELECT Vehicle_Make, count(*)as Frequency_of_Tickets
                                     from tkt_2016_nyc 
                                     group by Vehicle_Make
                                     order by Frequency_of_Tickets desc")
head(vehmake_frequency_2016, 5)

vehmake_2016_top5<- data.frame(head(vehmake_frequency_2016, 5))

ggplot(vehmake_2016_top5, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("Plot6B. 2016 Top 5 Vehicle Make vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.2.2.3 Top Vehicle Make for 2017
vehmake_frequency_2017<- SparkR::sql("SELECT Vehicle_Make, count(*)as Frequency_of_Tickets
                                     from tkt_2017_nyc 
                                     group by Vehicle_Make
                                     order by Frequency_of_Tickets desc")
head(vehmake_frequency_2017, 5)

vehmake_2017_top5<- data.frame(head(vehmake_frequency_2017, 5))

ggplot(vehmake_2017_top5, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("Plot6C. 2017 Top 5 Vehicle Make vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.2.2.4 Combined Comparison for Top-5 Body Makes
vehmake_combined<- rbind(vehmake_2015_top5, vehmake_2016_top5, vehmake_2017_top5)
vehmake_combined$Year_Labels<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")
ggplot(vehmake_combined, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Year_Labels) + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("Plot6. Comparison of Top 5 Vehicle Make vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#************ Stage 3: Q3 ************#
#A precinct is a police station that has a certain zone of the city under its command. Find the (5 highest) frequencies of:
#Violating Precincts (this is the precinct of the zone where the violation occurred)
#Issuing Precincts (this is the precinct that issued the ticket)

#3.3.1. Violation Precinct vs Frequency of Tickets
#3.3.1.1 Top Violation Precinct for 2015
vioprect_frequency_2015<- SparkR::sql("SELECT Violation_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2015_nyc 
                                      group by Violation_Precinct
                                      order by Frequency_of_Tickets desc")
head(vioprect_frequency_2015,5)

vioprect_2015_top5<- data.frame(head(vioprect_frequency_2015,5))

ggplot(vioprect_2015_top5, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot7A. 2015 Top 5 Violation Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.3.1.2 Top Violation Precinct for 2016
vioprect_frequency_2016<- SparkR::sql("SELECT Violation_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2016_nyc 
                                      group by Violation_Precinct
                                      order by Frequency_of_Tickets desc")
head(vioprect_frequency_2016,5)

vioprect_2016_top5<- data.frame(head(vioprect_frequency_2016,5))

ggplot(vioprect_2016_top5, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot7B. 2016 Top 5 Violation Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.3.1.3 Top Violation Precinct for 2017
vioprect_frequency_2017<- SparkR::sql("SELECT Violation_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2017_nyc 
                                      group by Violation_Precinct
                                      order by Frequency_of_Tickets desc")
head(vioprect_frequency_2017,5)

vioprect_2017_top5<- data.frame(head(vioprect_frequency_2017,5))

ggplot(vioprect_2017_top5, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot7C. 2017 Top 5 Violation Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.3.1.4 Combined Comparison for Violation Precinct
vioprect_combined<- rbind(vioprect_2015_top5,vioprect_2016_top5,vioprect_2017_top5)
vioprect_combined$Year_Labels<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")
ggplot(vioprect_combined, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Year_Labels) + xlab("Violation Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot7. Comparison of Top 5 Violation Precinct vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#3.3.2 Issuer Precinct vs Frequency of Tickets
#3.3.2.1 Top Issuer Precinct for 2015
isuprect_frequency_2015<- SparkR::sql("SELECT Issuer_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2015_nyc 
                                      group by Issuer_Precinct
                                      order by Frequency_of_Tickets desc")
head(isuprect_frequency_2015,5)

isuprect_2015_top5<- data.frame(head(isuprect_frequency_2015,5))

ggplot(isuprect_2015_top5, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot8A. 2015 Top 5 Issuer Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#3.3.2.2 Top Issuer Precinct for 2016
isuprect_frequency_2016<- SparkR::sql("SELECT Issuer_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2016_nyc 
                                      group by Issuer_Precinct
                                      order by Frequency_of_Tickets desc")
head(isuprect_frequency_2016,5)

isuprect_2016_top5<- data.frame(head(isuprect_frequency_2016,5))

ggplot(isuprect_2016_top5, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot8B. 2016 Top 5 Issuer Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.3.2.3 Top Issuer Precinct for 2017
isuprect_frequency_2017<- SparkR::sql("SELECT Issuer_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2017_nyc 
                                      group by Issuer_Precinct
                                      order by Frequency_of_Tickets desc")
head(isuprect_frequency_2017,5)

isuprect_2017_top5<- data.frame(head(isuprect_frequency_2017,5))

ggplot(isuprect_2017_top5, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot8C. 2017 Top 5 Issuer Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.3.2.4 Combined Comparison for Issuer Precinct
isuprect_combined<- rbind(isuprect_2015_top5, isuprect_2016_top5, isuprect_2017_top5)
isuprect_combined$Year_Labels<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")
ggplot(isuprect_combined, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Year_Labels) + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot8. Comparison of Top 5 Issuer Precinct vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#************ Stage 3: Q4 ************#
#Find the violation code frequency across 3 precincts which have issued the most number of tickets - do these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts?

#3.4.1 In Year 2015 [Top Three Issuer Precinct's : 0, 19 and 18]

#Violation Code Distribution in Issuer Precinct 0
one_isuprect_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2015_nyc 
                                where Issuer_Precinct = 0
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(one_isuprect_2015, 5)

one_isuprect_top5_2015<- data.frame(head(one_isuprect_2015, 5))

#Violation Code Distribution in Issuer Precinct 19
two_isuprect_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2015_nyc 
                                where Issuer_Precinct = 19
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(two_isuprect_2015, 5)

two_isuprect_top5_2015<- data.frame(head(two_isuprect_2015, 5))

#Violation Code Distribution in Issuer Precinct 18
three_isuprect_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                  from tkt_2015_nyc 
                                  where Issuer_Precinct = 18
                                  group by Violation_Code, Issuer_Precinct
                                  order by Frequency_of_Tickets desc")
head(three_isuprect_2015,5)

three_isuprect_top5_2015<- data.frame(head(three_isuprect_2015,5))

#Violation Code Distribution in Other Issuer Precincts
other_isuprect_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                  from tkt_2015_nyc 
                                  where Issuer_Precinct NOT IN (0,19,18)
                                  group by Violation_Code
                                  order by Frequency_of_Tickets desc")
head(other_isuprect_2015,5)

other_isuprect_top5_2015<- data.frame(head(other_isuprect_2015,5))
other_isuprect_top5_2015$Issuer_Precinct<- c("Other","Other","Other","Other","Other")

#Combined Violation Code Distribution vs Issuer Precincts in 2015

vioisuprect_2015_combined<- rbind(one_isuprect_top5_2015, two_isuprect_top5_2015, three_isuprect_top5_2015, other_isuprect_top5_2015)

ggplot(vioisuprect_2015_combined, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Issuer_Precinct) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot9A. 2015 Comparison of Violation Code Distribution vs. Top Issuer Precinct") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.4.2 In Year 2016 [Top Three Issuer Precinct's : 0, 19 and 18]

#Violation Code Distribution in Issuer Precinct 0
one_isuprect_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2016_nyc 
                                where Issuer_Precinct = 0
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(one_isuprect_2016, 5)

one_isuprect_top5_2016<- data.frame(head(one_isuprect_2016, 5))

#Violation Code Distribution in Issuer Precinct 19
two_isuprect_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2016_nyc 
                                where Issuer_Precinct = 19
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(two_isuprect_2016, 5)

two_isuprect_top5_2016<- data.frame(head(two_isuprect_2016, 5))

#Violation Code Distribution in Issuer Precinct 18
three_isuprect_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                  from tkt_2016_nyc 
                                  where Issuer_Precinct = 18
                                  group by Violation_Code, Issuer_Precinct
                                  order by Frequency_of_Tickets desc")
head(three_isuprect_2016,5)

three_isuprect_top5_2016<- data.frame(head(three_isuprect_2016,5))

#Violation Code Distribution in Other Issuer Precincts
other_isuprect_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                  from tkt_2016_nyc 
                                  where Issuer_Precinct NOT IN (0,19,18)
                                  group by Violation_Code
                                  order by Frequency_of_Tickets desc")
head(other_isuprect_2016,5)

other_isuprect_top5_2016<- data.frame(head(other_isuprect_2016,5))
other_isuprect_top5_2016$Issuer_Precinct<- c("Other","Other","Other","Other","Other")

#Combined Violation Code Distribution vs Issuer Precincts in 2016

vioisuprect_2016_combined<- rbind(one_isuprect_top5_2016, two_isuprect_top5_2016, three_isuprect_top5_2016, other_isuprect_top5_2016)

ggplot(vioisuprect_2016_combined, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Issuer_Precinct) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot9B. 2016 Comparison of Violation Code Distribution vs. Top Issuer Precinct") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#3.4.3 In Year 2017 [Top Three Issuer Precinct's : 0, 19 and 14]

#Violation Code Distribution in Issuer Precinct 0
one_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2017_nyc 
                                where Issuer_Precinct = 0
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(one_isuprect_2017, 5)

one_isuprect_top5_2017<- data.frame(head(one_isuprect_2017, 5))

#Violation Code Distribution in Issuer Precinct 19
two_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2017_nyc 
                                where Issuer_Precinct = 19
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(two_isuprect_2017, 5)

two_isuprect_top5_2017<- data.frame(head(two_isuprect_2017, 5))

#Violation Code Distribution in Issuer Precinct 14
three_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                  from tkt_2017_nyc 
                                  where Issuer_Precinct = 14
                                  group by Violation_Code, Issuer_Precinct
                                  order by Frequency_of_Tickets desc")
head(three_isuprect_2017,5)

three_isuprect_top5_2017<- data.frame(head(three_isuprect_2017,5))

#Violation Code Distribution in Other Issuer Precincts
other_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                  from tkt_2017_nyc 
                                  where Issuer_Precinct NOT IN (0,19,14)
                                  group by Violation_Code
                                  order by Frequency_of_Tickets desc")
head(other_isuprect_2017,5)

other_isuprect_top5_2017<- data.frame(head(other_isuprect_2017,5))
other_isuprect_top5_2017$Issuer_Precinct<- c("Other","Other","Other","Other","Other")

#Combined Violation Code Distribution vs Issuer Precincts in 2017

vioisuprect_2017_combined<- rbind(one_isuprect_top5_2017, two_isuprect_top5_2017, three_isuprect_top5_2017, other_isuprect_top5_2017)

ggplot(vioisuprect_2017_combined, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Issuer_Precinct) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot9C. 2017 Comparison of Violation Code Distribution vs. Top Issuer Precinct") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#************ Stage 3: Q5 ************#

#You’d want to find out the properties of parking violations across different times of the day:
#We've Already Converted the Violation Time into a standardized timestamp format. 


#******************** Part 1: Violation Bin Group vs Violation Code [Top-3] ********************#

#2015 Dataset: Violation Time Bin vs. Violation Code Analysis
#Find a way to deal with missing values, if any.
null_violat_times_2015<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                     SUM(CASE WHEN Violation_Time is NULL
                                     THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                     100*SUM(CASE WHEN Violation_Time IS NULL
                                     THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2015_ViolationTimeMissing
                                     from tkt_2016_nyc")
head(null_violat_times_2015)
#2015 dataset 0.5812% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

adjusted_tkt_2015_nycparking<- subset(tkt_2015_nycparking, isNotNull(tkt_2015_nycparking$Violation_Time))
adjusted_tkt_2015_nycparking$Violation_Hour <- hour(cast(adjusted_tkt_2015_nycparking$Violation_Time,dataType = "string"))
createOrReplaceTempView(adjusted_tkt_2015_nycparking, "violt_2015")


#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2015 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM violt_2015")

createOrReplaceTempView(violation_hour_bin_2015, "violt_hour_2015_nyc")

hour_bin_tkts_2015 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets,
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  count(*)as Frequency_of_Tickets
                                  FROM violt_hour_2015_nyc
                                  GROUP BY Violation_Hour_Bin,
                                  Violation_Code))
                                  WHERE Rnk <= 3")

df_hour_bin_tkts_2015 <- data.frame(head(hour_bin_tkts_2015, nrow(hour_bin_tkts_2015)))

ggplot(df_hour_bin_tkts_2015, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Violation_Hour_Bin) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot13A. 2015 Comparison of Violation Code Distribution vs. Violation_Hour_Bin") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#2016 Dataset: Violation Time Bin vs. Violation Code Analysis

#Find a way to deal with missing values, if any.
null_violat_times_2016<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                     SUM(CASE WHEN Violation_Time is NULL
                                     THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                     100*SUM(CASE WHEN Violation_Time IS NULL
                                     THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2016_ViolationTimeMissing
                                     from tkt_2016_nyc")
head(null_violat_times_2016)
#2016 dataset 0.6114% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

adjusted_tkt_2016_nycparking<- subset(tkt_2016_nycparking, isNotNull(tkt_2016_nycparking$Violation_Time))
adjusted_tkt_2016_nycparking$Violation_Hour <- hour(cast(adjusted_tkt_2016_nycparking$Violation_Time,dataType = "string"))
createOrReplaceTempView(adjusted_tkt_2016_nycparking, "violt_2016")


#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2016 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM violt_2016")

createOrReplaceTempView(violation_hour_bin_2016, "violt_hour_2016_nyc")

hour_bin_tkts_2016 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets,
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  count(*)as Frequency_of_Tickets
                                  FROM violt_hour_2016_nyc
                                  GROUP BY Violation_Hour_Bin,
                                  Violation_Code))
                                  WHERE Rnk <= 3")

df_hour_bin_tkts_2016 <- data.frame(head(hour_bin_tkts_2016, nrow(hour_bin_tkts_2016)))

ggplot(df_hour_bin_tkts_2016, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Violation_Hour_Bin) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot13B. 2016 Comparison of Violation Code Distribution vs. Violation_Hour_Bin") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#2017 Dataset: Violation Time Bin vs. Violation Code Analysis

#Find a way to deal with missing values, if any.
null_violat_times_2017<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                     SUM(CASE WHEN Violation_Time is NULL
                                     THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                     100*SUM(CASE WHEN Violation_Time IS NULL
                                     THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2017_ViolationTimeMissing
                                     from tkt_2017_nyc")
head(null_violat_times_2017)
#2017 dataset 0.6114% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

adjusted_tkt_2017_nycparking<- subset(tkt_2017_nycparking, isNotNull(tkt_2017_nycparking$Violation_Time))
adjusted_tkt_2017_nycparking$Violation_Hour <- hour(cast(adjusted_tkt_2017_nycparking$Violation_Time,dataType = "string"))
createOrReplaceTempView(adjusted_tkt_2017_nycparking, "violt_2017")


#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2017 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM violt_2017")

createOrReplaceTempView(violation_hour_bin_2017, "violt_hour_2017_nyc")

hour_bin_tkts_2017 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets,
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  count(*)as Frequency_of_Tickets
                                  FROM violt_hour_2017_nyc
                                  GROUP BY Violation_Hour_Bin,
                                  Violation_Code))
                                  WHERE Rnk <= 3")

df_hour_bin_tkts_2017 <- data.frame(head(hour_bin_tkts_2017, nrow(hour_bin_tkts_2017)))

ggplot(df_hour_bin_tkts_2017, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Violation_Hour_Bin) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot13C. 2017 Comparison of Violation Code Distribution vs. Violation_Hour_Bin") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#*******************************************************************************************************#

#Now, try another direction. For the 3 most commonly occurring violation codes, find the most common times of day (in terms of the bins from the previous part)

#******************** Part 2: Violation Code [Top-3] vs. Violation Time Bin Distribution ********************#
#2015 Dataset: Top-3 Violation Code vs. Violation Time Bin Analysis
top_3_violations_2015 <- SparkR::sql("SELECT Violation_Code,
                                     count(*) no_of_tickets
                                     FROM violt_hour_2015_nyc
                                     GROUP BY Violation_Code
                                     ORDER BY no_of_tickets desc")

head(top_3_violations_2015,3)
#Top-3 Violation Code for 2015 are 21, 38 and 14

common_times_2015 <- SparkR::sql("SELECT Violation_Code,
                                 Violation_Hour_Bin,
                                 count(*) no_of_tickets
                                 FROM violt_hour_2015_nyc
                                 WHERE violation_code IN (21,38,14)
                                 GROUP BY Violation_Code, 
                                 Violation_Hour_Bin
                                 ORDER BY Violation_Code, 
                                 Violation_Hour_Bin,
                                 no_of_tickets desc")	

df_common_times_viol_2015 <- data.frame(head(common_times_2015, nrow(common_times_2015)))

ggplot(df_common_times_viol_2015, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col()+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("Plot14A. 2015 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)

#2016 Dataset: Top-3 Violation Code vs. Violation Time Bin Analysis
top_3_violations_2016 <- SparkR::sql("SELECT Violation_Code,
                                     count(*) no_of_tickets
                                     FROM violt_hour_2016_nyc
                                     GROUP BY Violation_Code
                                     ORDER BY no_of_tickets desc")

head(top_3_violations_2016,3)
#Top-3 Violation Codes for 2016 are 21, 36 and 38.

common_times_2016 <- SparkR::sql("SELECT Violation_Code,
                                 Violation_Hour_Bin,
                                 count(*) no_of_tickets
                                 FROM violt_hour_2016_nyc
                                 WHERE violation_code IN (21,36,38)
                                 GROUP BY Violation_Code, 
                                 Violation_Hour_Bin
                                 ORDER BY Violation_Code, 
                                 Violation_Hour_Bin,
                                 no_of_tickets desc")	

df_common_times_viol_2016 <- data.frame(head(common_times_2016, nrow(common_times_2016)))

ggplot(df_common_times_viol_2016, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col()+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("Plot14B. 2016 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)

#2017 Dataset: Top-3 Violation Code vs. Violation Time Bin Analysis
top_3_violations_2017 <- SparkR::sql("SELECT Violation_Code,
                                     count(*) no_of_tickets
                                     FROM violt_hour_2017_nyc
                                     GROUP BY Violation_Code
                                     ORDER BY no_of_tickets desc")

head(top_3_violations_2017,3)
#Top-3 Violation Codes for 2017 are 21, 36 and 38.

common_times_2017 <- SparkR::sql("SELECT Violation_Code,
                                 Violation_Hour_Bin,
                                 count(*) no_of_tickets
                                 FROM violt_hour_2017_nyc
                                 WHERE violation_code IN (21,36,38)
                                 GROUP BY Violation_Code, 
                                 Violation_Hour_Bin
                                 ORDER BY Violation_Code, 
                                 Violation_Hour_Bin,
                                 no_of_tickets desc")	

df_common_times_viol_2017 <- data.frame(head(common_times_2017, nrow(common_times_2017)))

ggplot(df_common_times_viol_2017, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col()+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("Plot14C. 2017 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)

#************ Stage 3: Q6 ************#
# Checking for seasonality in dataset

#3.6.1 Season vs. Frequency Analysis

#2015 Season vs. Frequency Analysis
Season_Binning_2015 <- SparkR::sql("SELECT Summons_Number,
                                   Violation_Code,
                                   CASE WHEN Issue_Month IN (1,2,12)
                                   THEN 'Winter'
                                   WHEN Issue_Month BETWEEN 3 AND 5
                                   THEN 'Spring'
                                   WHEN Issue_Month BETWEEN 6 AND 8
                                   THEN 'Summer'
                                   WHEN Issue_Month BETWEEN 9 AND 12
                                   THEN 'Fall' 
                                   END AS Season
                                   FROM tkt_2015_nyc")
createOrReplaceTempView(Season_Binning_2015, "season_tkt_2015_nyc")

tktseason_2015<- SparkR::sql("SELECT Season,
                             Count(*)as Frequency_of_Tickets
                             FROM season_tkt_2015_nyc
                             GROUP BY Season
                             ORDER BY Frequency_of_Tickets desc")
head(tktseason_2015)

freq_tktseason_2015<- data.frame(head(tktseason_2015))
freq_tktseason_2015$Fiscal_Year<- c(2015,2015,2015,2015)
freq_tktseason_2015

#2016 Season vs. Frequency Analysis
Season_Binning_2016 <- SparkR::sql("SELECT Summons_Number,
                                   Violation_Code,
                                   CASE WHEN Issue_Month IN (1,2,12)
                                   THEN 'Winter'
                                   WHEN Issue_Month BETWEEN 3 AND 5
                                   THEN 'Spring'
                                   WHEN Issue_Month BETWEEN 6 AND 8
                                   THEN 'Summer'
                                   WHEN Issue_Month BETWEEN 9 AND 12
                                   THEN 'Fall' 
                                   END AS Season
                                   FROM tkt_2016_nyc")
createOrReplaceTempView(Season_Binning_2016, "season_tkt_2016_nyc")

tktseason_2016<- SparkR::sql("SELECT Season,
                             Count(*)as Frequency_of_Tickets
                             FROM season_tkt_2016_nyc
                             GROUP BY Season
                             ORDER BY Frequency_of_Tickets desc")
head(tktseason_2016)

freq_tktseason_2016<- data.frame(head(tktseason_2016))
freq_tktseason_2016$Fiscal_Year<- c(2016,2016,2016,2016)
freq_tktseason_2016

#2017 Season vs. Frequency Analysis
Season_Binning_2017 <- SparkR::sql("SELECT Summons_Number,
                                   Violation_Code,
                                   CASE WHEN Issue_Month IN (1,2,12)
                                   THEN 'Winter'
                                   WHEN Issue_Month BETWEEN 3 AND 5
                                   THEN 'Spring'
                                   WHEN Issue_Month BETWEEN 6 AND 8
                                   THEN 'Summer'
                                   WHEN Issue_Month BETWEEN 9 AND 12
                                   THEN 'Fall' 
                                   END AS Season
                                   FROM tkt_2017_nyc")
createOrReplaceTempView(Season_Binning_2017, "season_tkt_2017_nyc")

tktseason_2017<- SparkR::sql("SELECT Season,
                             Count(*)as Frequency_of_Tickets
                             FROM season_tkt_2017_nyc
                             GROUP BY Season
                             ORDER BY Frequency_of_Tickets desc")
head(tktseason_2017)

freq_tktseason_2017<- data.frame(head(tktseason_2017))
freq_tktseason_2017$Fiscal_Year<- c(2017,2017,2017,2017)
freq_tktseason_2017

#Comparison of Season vs. Frequency of Tickets ocer the Years
freq_tktseason_combined<- rbind(freq_tktseason_2015, freq_tktseason_2016, freq_tktseason_2017)

ggplot(freq_tktseason_combined, aes(x= as.factor(Season), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Fiscal_Year) + xlab("Seasons of Year") + ylab("Frequency of Tickets") + ggtitle("Plot11A. Comparison of Seasons vs. Frequency of Tickets between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#3.6.2 Season vs. Violation Code Distribution Analysis

#2015 Season vs. Violation Code Distribution Analysis

season_violation_2015 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_tkt_2015_nyc
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2015 <-  data.frame(head(season_violation_2015, nrow(season_violation_2015)))
df_season_violation_2015

#Seasonwise Violation Code Distribution 2015
ggplot(df_season_violation_2015, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot12A. 2015 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#2016 Season vs. Violation Code Distribution Analysis

season_violation_2016 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_tkt_2016_nyc
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2016 <-  data.frame(head(season_violation_2016, nrow(season_violation_2016)))
df_season_violation_2016

#Seasonwise Violation Code Distribution 2016
ggplot(df_season_violation_2016, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot12B. 2016 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#2017 Season vs. Violation Code Distribution Analysis

season_violation_2017 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_tkt_2017_nyc
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2017 <-  data.frame(head(season_violation_2017, nrow(season_violation_2017)))
df_season_violation_2017

#Seasonwise Violation Code Distribution 2017
ggplot(df_season_violation_2017, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot12C. 2017 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#************ Stage 3: Q7 ************#

#2015
violationcd_frequency_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2015_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2015,3)

fine_top3_2015<- data.frame(head(violationcd_frequency_2015,3))
fine_top3_2015$Fiscal_Year <- c(2015,2015,2015)
fine_top3_2015$Average_Fine_PerTicket<- c(55,50,115)
fine_top3_2015$Total_Fine_Amount<- fine_top3_2015$Frequency_of_Tickets * fine_top3_2015$Average_Fine_PerTicket
fine_top3_2015

#2016
violationcd_frequency_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2016_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2016,3)

fine_top3_2016<- data.frame(head(violationcd_frequency_2016,3))
fine_top3_2016$Fiscal_Year <- c(2016,2016,2016)
fine_top3_2016$Average_Fine_PerTicket<- c(55,50,50)
fine_top3_2016$Total_Fine_Amount<- fine_top3_2016$Frequency_of_Tickets * fine_top3_2016$Average_Fine_PerTicket
fine_top3_2016

#2017
violationcd_frequency_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2017_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2017,3)

fine_top3_2017<- data.frame(head(violationcd_frequency_2017,3))
fine_top3_2017$Fiscal_Year <- c(2017,2017,2017)
fine_top3_2017$Average_Fine_PerTicket<- c(55,50,50)
fine_top3_2017$Total_Fine_Amount<- fine_top3_2017$Frequency_of_Tickets * fine_top3_2017$Average_Fine_PerTicket
fine_top3_2017

fine_top3_combined<- rbind(fine_top3_2015, fine_top3_2016, fine_top3_2017)

ggplot(fine_top3_combined, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Fiscal_Year) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot10A. Comparison of Top 3 Violation Code vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

ggplot(fine_top3_combined, aes(x=as.factor(Violation_Code), y=Total_Fine_Amount))+ geom_col()+ facet_grid(~Fiscal_Year) + xlab("Violation Code") + ylab("Total Fine Amount") + ggtitle("Plot10B. Comparison of Top 3 Violation Code vs Total Fine Amount between Fiscal Years") + geom_text(aes(label=Total_Fine_Amount),vjust=-0.3)

#************************* END of CODE *************************#