#HR Analytics Case Study
#Coded by: Team Code Ninjas 
#Set Working Directory to location on local system containing the HR Analytics dataset
#Brief: Binary classification Model in the domain of HR Analytics

#Importing relevant packages into the R-environment
library(stringr)
library(dplyr)
library(ggplot2)
library(cowplot)
library("car")
library("MASS")
library(caTools)
library(scales)
library(stats)
library(tidyr)
library(ROCR)
library(e1071)
library(caret)

#Steps followed during the execution of this Case Study/Project:
#[I] Business Understanding
#[II] Data Understanding
#[III] Data Exploration and Preparation 
#[IV] Model Building
#[V] Model Evaluation, Interpretation of results and Deployment


##[I] Business Understanding:
#A large company 'XYZ', provides employment for approximately 4000 individuals. However, in recent years it is faced with the challenge of high annual attrition rate of approximately 15%.
#This adversely impacts the business and reputation of the organization in the following ways:
#[1] It causes delay in completion of projects formerly undertaken by employees who later attrition making it challenging to meet timelines. This harms the reputation of the organization leading to loss in customer and market share.
#[2] Investment of resources and manpower to maintain a department focused on sourcing and hiring new talent to fill abruptly vacated positions in the organization.  
#[3] Requirement to train newly acquired talent to perform effectively in the organization. Delays caused due to time allocated for newly acquired talent for acclimatizing in the new work environment.

##Goals of the Case Study
#[1] Determine and understand the driver variables influencing high attrition rate within the organization.
#[2] Build a model to estimate the probability of an employee to attrition.
#[3] Provide actionable insights for inducing changes within the organization so as to curb attrition and encourage employees to stay.

##[II] Data Understanding:
#Importing the datasets into R-environment
general_data<- read.csv("general_data.csv", stringsAsFactors = FALSE, na.strings = c("NA","na"))
employee_survey<- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE, na.strings = c("NA","na"))
manager_survey<- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE, na.strings = c("NA","na"))
in_punch<- read.csv("in_time.csv", stringsAsFactors = FALSE, na.strings = c("NA","na"))
out_punch<- read.csv("out_time.csv", stringsAsFactors = FALSE, na.strings = c("NA","na"))

#There are five tables available for analysis details of which are given below:
#Each record can be traced through a unique EmployeeID which will serve as the primary key for these table.
#[1] general_data<- This contains personal details, demographics, educational data, career relevant information and wages for each employee. 
#[2] employee_survey<- This contains information regarding a few scaled categorical parameters which were filled by the employees within the organization. 
#[3] manager_survey<- This contains information regarding scaled parameters used by the managers within the organization to rate the employees
#[4] in_punch<- This contains time and date information for the year 2015 registered when the employee began work for the day
#[5] out_punch<- This contains time and date information for the year 2015 registered when the employee ended work for the day

#Understanding the structure of the dataset:
str(general_data) #4410 obs. of  24 variables including the target variable Attrition
str(employee_survey) #4410 obs. of  4 variables
str(manager_survey) #4410 obs. of  3 variables
#The structure of in_punch and out_punch reveal that the first column in Employee ID followed by date and time information for the working days between 1st Jan to 19th May 2015.
#They each contain 4410 obs. with 261 days of timestamp data.
colnames(in_punch)[1]<- "EmployeeID"
colnames(out_punch)[1]<- "EmployeeID"

#Refer Data Dictionary given below:
#[1] Age=Age of the employee
#[2] Attrition=Whether the employee left in the previous year or not
#[3] BusinessTravel=How frequently the employees travelled for business purposes in the last year
#[4] Department=Department in company
#[5] DistanceFromHome=Distance from home in kms
#[6] Education=Education Level : 1=Below College | 2=College| 3=Bachelor| 4=Master| 5=Doctor
#[7] EducationField=Field of education
#[8] EmployeeCount=Employee count
#[9] EmployeeNumber=Employee number/id
#[10] EnvironmentSatisfaction=Work Environment Satisfaction Level : 1=Low | 2=Medium | 3=High | 4=Very High
#[11] Gender=Gender of employee
#[12] JobInvolvement=Job Involvement Level
#[13] JobLevel=Job level at company on a scale of 1 to 5
#[14] JobRole=Name of job role in company
#[15] JobSatisfaction=Job Satisfaction Level : 1=Low | 2=Medium | 3=High | 4=Very High
#[16] MaritalStatus=Marital status of the employee
#[17] MonthlyIncome=Monthly income in rupees per month
#[18] NumCompaniesWorked=Total number of companies the employee has worked for
#[19] Over18=Whether the employee is above 18 years of age or not
#[20] PercentSalaryHike=Percent salary hike for last year
#[21] PerformanceRating=Performance rating for last year: 1=Low | 2=Good | 3=Excellent | 4=Outstanding
#[22] RelationshipSatisfaction=Relationship satisfaction level : 1=Low | 2=Medium | 3=High | 4=Very High
#[23] StandardHours=Standard hours of work for the employee
#[24] StockOptionLevel=Stock option level of the employee
#[25] TotalWorkingYears=Total number of years the employee has worked so far
#[26] TrainingTimesLastYear=Number of times training was conducted for this employee last year
#[27] WorkLifeBalance=Work life balance level : 1=Bad | 2=Good | 3=Better | 4=Best
#[28] YearsAtCompany=Total number of years spent at the company by the employee
#[29] YearsSinceLastPromotion=Number of years since last promotion
#[30] YearsWithCurrManager=Number of years under current manager

#Checking if EmployeeID is the primary key for all 5 tables
if(length(unique(employee_survey$EmployeeID))==nrow(employee_survey)){paste("EmployeeID is primary key for employee_survey Table")}
if(length(unique(general_data$EmployeeID))==nrow(general_data)){paste("EmployeeID is primary key for general_data Table")}
if(length(unique(manager_survey$EmployeeID))==nrow(manager_survey)){paste("EmployeeID is primary key for manager_survey Table")}
if(length(unique(in_punch$EmployeeID))==nrow(in_punch)){paste("EmployeeID is primary key for in_punch Table")}
if(length(unique(out_punch$EmployeeID))==nrow(out_punch)){paste("EmployeeID is primary key for out_punch Table")}

setdiff(general_data$EmployeeID, employee_survey$EmployeeID)
setdiff(general_data$EmployeeID, manager_survey$EmployeeID)
setdiff(general_data$EmployeeID, in_punch$EmployeeID)
setdiff(general_data$EmployeeID, out_punch$EmployeeID)
#Since Employee_ID is the primary key across all the tables and the setdiff() returns the value zero implying that the same set of 4410 Employee_ID are present in all the 5 tables.

#Checking for Data Duplication-This step is not required as we have already determined that Employee_ID is the primary key for all the tables and the number of unique Employee_ID is equal to the number of rows in each table:
sum(duplicated(employee_survey))
sum(duplicated(general_data))
sum(duplicated(manager_survey))
sum(duplicated(in_punch))
sum(duplicated(out_punch))

#Checking for Missing Values or NA
sapply(employee_survey, function(x) sum(is.na(x)))
#there are 83 missing values in the employee_survey dataset 
sapply(general_data, function(x) sum(is.na(x)))
#there are 28 missing values in the general_survey dataset specifically in the NumCompaniesWorked and TotalWorkingYears columns.
sapply(manager_survey, function(x) sum(is.na(x)))
#there are no missing values in the manager_survey dataset

##[III] Exploratory Data Analysis and Data Preparation

#We have decided to use the in_punch and out_punch tables to derive attributes with respect to working hours.
#Removing columns from in_punch and out_punch if only NA values are present
#After searching stackoverflow for similar discussions please read the following link for a detailed understanding of how our datachop function performs https://stackoverflow.com/questions/30544282/how-to-remove-columns-with-same-value-in-r
#The datachop function checks all the records of each column for more than 1 unique value, if there is only one unique value or NA it deletes that column from the loan_dataset. The logical(1L) argument specifies that the length function should return a logical value of length 1 integer.
in_punch<- in_punch[vapply(in_punch, function(datachop) length(unique(datachop))>1, logical(1L))]
out_punch<- out_punch[vapply(out_punch, function(datachop) length(unique(datachop))>1, logical(1L))]
#Creating a new dataframe titled worked_hours to store the totalhours worked per-day by the employee
worked_hours<- data.frame("EmployeeID"= in_punch$EmployeeID)
for(sb in 2:ncol(in_punch))
{
  in_punch[,sb]<- as.POSIXct(in_punch[,sb], "%Y-%m-%d %H:%M:%S")
  out_punch[,sb]<- as.POSIXct(out_punch[,sb], "%Y-%m-%d %H:%M:%S")
  worked_hours[,sb]<- round(out_punch[,sb]-in_punch[,sb],1)
  colnames(worked_hours)[sb]<- colnames(out_punch)[sb]
}

#Deriving metrics for Total Number of Leaves, Average working hours per day per year, Total Working Hours and Average Hours worked excess of Standard Hours[i.e 8 hrs.]
worked_hours <- gather(worked_hours, key = "Date", 
                       value = "logged_hours", 2:ncol(worked_hours))

worked_hours$Date <- substr(worked_hours$Date, 2, 1000)
worked_hours$Date <- as.Date(worked_hours$Date, "%Y.%m.%d")
worked_hours$month <- format(worked_hours$Date,"%m")

emp_logging <- worked_hours %>% group_by(EmployeeID) %>% 
  summarise(Tot.logged.hours = sum(logged_hours, na.rm = TRUE),
            Avg.logged.hours = round(mean(logged_hours, na.rm = TRUE),2),
            Tot.leaves.taken = sum(is.na(logged_hours)),
            Tot.excess.logged = sum(logged_hours - 8, na.rm = TRUE),
            Avg.excess.logged = round(mean(logged_hours - 8, na.rm = TRUE),2))
str(emp_logging)

#Merging the employee_survey, general_data, manager_survey based and emp_logging on Enployee ID and title it main_data
main_data<- merge(general_data, employee_survey, by="EmployeeID", all = FALSE)
main_data<- merge(main_data, manager_survey, by="EmployeeID", all = FALSE)
main_data<- merge(main_data, emp_logging, by="EmployeeID", all = FALSE)
rm(employee_survey)
rm(manager_survey)
rm(general_data)
rm(in_punch)
rm(out_punch)
rm(worked_hours)
rm(emp_logging)

#We will convert all character attribute records to upper case to avoid any case sensititve inconsistancies and data entry discrepancies.
#The case conversion will be done using a custom defined function caseconversionfun
caseconversionfun<- function(col_input)
{
  if(is.character(col_input)==T)
  {
    col_input<- toupper(col_input)
  }
  return(col_input)
}

for(sb in 1:ncol(main_data))
{
  main_data[,sb]<- caseconversionfun(main_data[,sb])
}

#Exploratory Data Analysis of main_data
#Overview of Attrition
overall_status<-ggplot(main_data, aes(x=as.factor(main_data$Attrition), fill= factor(main_data$Attrition)))+  geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels = percent) + 
  labs(title = "Plot1. Attrition Rate of Employees", y = "Percentage of Total Employees", x = "Attrition Status") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Attrition Status")
overall_status


#**************************** EDA of CATEGORICAL ATTRIBUTES ****************************#

#1. BusinessTravel vs Attrition
bus_travel_count<-ggplot(data = main_data, aes(reorder(BusinessTravel, -table(BusinessTravel)[BusinessTravel]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Business Travel Status")+ylab("Count of Employees") + ggtitle("Plot2. BusinessTravel vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
bus_travel_count

bus_travel_attr<-ggplot(data= main_data, aes(x= Attrition,  group=BusinessTravel)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~BusinessTravel) + scale_y_continuous(labels = scales::percent) + xlab("Business Travel wise Attrition")+ylab("Percentage of Employees") + ggtitle("Plot3. Business Travel V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
bus_travel_attr
#Employees who travel rarely have the highest count of attrition. However when looking at the proportion 24.9% employees who travel frequently and 15% of employees who travel rarely are likely to attrition

#2. Department vs Attrition
dept_count<-ggplot(data = main_data, aes(reorder(Department, -table(Department)[Department]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Department")+ylab("Count of Employees") + ggtitle("Plot4. Department vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
dept_count

dept_attr<-ggplot(data= main_data, aes(x= Attrition,  group=Department)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~Department) + scale_y_continuous(labels = scales::percent) + xlab("Department")+ylab("Percentage of Employees") + ggtitle("Plot5. Department V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
dept_attr
#Research & Development has the highest count of attritioned employees while Human Resource has the highest proprtion of Attritioned members in a department

#3. Education Level vs. Attrition
main_data$Education<- as.character(main_data$Education)
main_data$Education<- gsub(pattern = "1", replacement = "BELOW_COLLEGE", main_data$Education)
main_data$Education<- gsub(pattern = "2", replacement = "COLLEGE", main_data$Education)
main_data$Education<- gsub(pattern = "3", replacement = "BACHELOR", main_data$Education)
main_data$Education<- gsub(pattern = "4", replacement = "MASTER", main_data$Education)
main_data$Education<- gsub(pattern = "5", replacement = "DOCTOR", main_data$Education)

edu_count<-ggplot(data = main_data, aes(reorder(Education, -table(Education)[Education]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Education Level")+ylab("Count of Employees") + ggtitle("Plot6. Education_Level vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
edu_count

edu_attr<-ggplot(data= main_data, aes(x= Attrition,  group=Education)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~Education) + scale_y_continuous(labels = scales::percent) + xlab("Education Level")+ylab("Percentage of Employees") + ggtitle("Plot7. Education Level V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
edu_attr
#Employees with education level as Bachelors contribute to the highest count of attritioned employees but employees with a college education level contribute to the highest attrition percentage.

#4. Education Field vs. Attrition
field_edu_count<- ggplot(data = main_data, aes(reorder(EducationField, -table(EducationField)[EducationField]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Field of Education")+ylab("Count of Employees") + ggtitle("Plot8. Field of Education vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
field_edu_count

field_edu_attr<-ggplot(data= main_data, aes(x= Attrition,  group=EducationField)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~EducationField) + scale_y_continuous(labels = scales::percent) + xlab("Field of Education")+ylab("Percentage of Employees") + ggtitle("Plot9. Field of Education V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
field_edu_attr
#Employees with a Life Sciences educational field account for the highest number of Attrition. However, employees with Human resources background accounts for the highest proportion of Attrition [Almost 40% of all employees with HR background Attrition]

#5. Gender vs. Attrition
gender_count<- ggplot(data = main_data, aes(reorder(Gender, -table(Gender)[Gender]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Gender of Employee")+ylab("Count of Employees") + ggtitle("Plot10. Gender vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
gender_count

gender_attr<-ggplot(data= main_data, aes(x= Attrition,  group=Gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~Gender) + scale_y_continuous(labels = scales::percent) + xlab("Gender of Employee")+ylab("Percentage of Employees") + ggtitle("Plot11. Gender V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
gender_attr
#The arrtition rate for Male employees is marginally higher but there seems to be no clear distinction of Atrition Rate between Male and Female.

#6. Job Level vs. Attrition
job_lev_count<- ggplot(data = main_data, aes(reorder(JobLevel, -table(JobLevel)[JobLevel]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Job Level of Employee")+ylab("Count of Employees") + ggtitle("Plot12. Job Level of Employee vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
job_lev_count

job_lev_attr<- ggplot(data= main_data, aes(x= Attrition,  group=JobLevel)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~JobLevel) + scale_y_continuous(labels = scales::percent) + xlab("Job Level of Employee")+ylab("Percentage of Employees") + ggtitle("Plot13. Job Level of Employees V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
job_lev_attr
#Employees with Job Level=2 are more likely to attrition

#7. Job Role vs. Attrition
job_role_count<- ggplot(data = main_data, aes(reorder(JobRole, -table(JobRole)[JobRole]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Job Role of Employee")+ylab("Count of Employees") + ggtitle("Plot14. Job Role of Employee vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
job_role_count

job_role_attr<- ggplot(data= main_data, aes(x= Attrition,  group=JobRole)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~JobRole) + scale_y_continuous(labels = scales::percent) + xlab("Job Role of Employee")+ylab("Percentage of Employees") + ggtitle("Plot15. Job Role of Employees V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
job_role_attr
#Research Director Shows the highest attrition rate of close to 23.7%

#8. Marital Status vs. Attrition
maritial_count<- ggplot(data = main_data, aes(reorder(MaritalStatus, -table(MaritalStatus)[MaritalStatus]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Marital Status of Employee")+ylab("Count of Employees") + ggtitle("Plot16. Marital Status of Employee vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
maritial_count

maritial_attr<- ggplot(data= main_data, aes(x= Attrition,  group=MaritalStatus)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~MaritalStatus) + scale_y_continuous(labels = scales::percent) + xlab("Marital Status of Employee")+ylab("Percentage of Employees") + ggtitle("Plot17. Marital Status of Employees V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
maritial_attr
#Single Employees are significantly more likely to attrition

#9. StockOptionLevel vs. Attrition
main_data$StockOptionLevel<- factor(main_data$StockOptionLevel)
stock_lev_count<- ggplot(data = main_data, aes(reorder(StockOptionLevel, -table(StockOptionLevel)[StockOptionLevel]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Stock Option Level of Employee")+ylab("Count of Employees") + ggtitle("Plot18. Stock Option Level of Employee vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
stock_lev_count
  
stock_lev_attr<- ggplot(data= main_data, aes(x= Attrition,  group=StockOptionLevel)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~StockOptionLevel) + scale_y_continuous(labels = scales::percent) + xlab("Stock Option Level of Employee")+ylab("Percentage of Employees") + ggtitle("Plot19. Stock Option Level of Employees V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
stock_lev_attr
#Employees with stock level 0 and 2 are more likely to Attrition

#10. Work Environment Satisfaction vs. Attrition
main_data$EnvironmentSatisfaction<- as.character(main_data$EnvironmentSatisfaction)
main_data$EnvironmentSatisfaction<- gsub(pattern = "1", replacement = "LOW", main_data$EnvironmentSatisfaction)
main_data$EnvironmentSatisfaction<- gsub(pattern = "2", replacement = "MEDIUM", main_data$EnvironmentSatisfaction)
main_data$EnvironmentSatisfaction<- gsub(pattern = "3", replacement = "HIGH", main_data$EnvironmentSatisfaction)
main_data$EnvironmentSatisfaction<- gsub(pattern = "4", replacement = "VERY_HIGH", main_data$EnvironmentSatisfaction)

env_sat_count<- ggplot(data = main_data, aes(reorder(EnvironmentSatisfaction, -table(EnvironmentSatisfaction)[EnvironmentSatisfaction]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Environment Satisfaction of Employee")+ylab("Count of Employees") + ggtitle("Plot20. Environment Satisfaction of Employee vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
env_sat_count
#25 records with missing Environment Satisfaction values

env_sat_attr<- ggplot(data= main_data, aes(x= Attrition,  group=EnvironmentSatisfaction)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~EnvironmentSatisfaction) + scale_y_continuous(labels = scales::percent) + xlab("Environment Satisfaction of Employee")+ylab("Percentage of Employees") + ggtitle("Plot21. Work Environment Satisfaction Level of Employees V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
env_sat_attr
#Employees with Low Work Enviornment Satisfaction levels have a very high risk of attrition.

#11. Job satisfaction Level vs. Attrition
main_data$JobSatisfaction<- as.character(main_data$JobSatisfaction)
main_data$JobSatisfaction<- gsub(pattern = "1", replacement = "LOW", main_data$JobSatisfaction)
main_data$JobSatisfaction<- gsub(pattern = "2", replacement = "MEDIUM", main_data$JobSatisfaction)
main_data$JobSatisfaction<- gsub(pattern = "3", replacement = "HIGH", main_data$JobSatisfaction)
main_data$JobSatisfaction<- gsub(pattern = "4", replacement = "VERY_HIGH", main_data$JobSatisfaction)

job_sat_count<- ggplot(data = main_data, aes(reorder(JobSatisfaction, -table(JobSatisfaction)[JobSatisfaction]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Job Satisfaction of Employee")+ylab("Count of Employees") + ggtitle("Plot22. Job Satisfaction Level of Employee vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
job_sat_count
#20 records with missing Job Satisfaction values

job_sat_attr<- ggplot(data= main_data, aes(x= Attrition,  group=JobSatisfaction)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~JobSatisfaction) + scale_y_continuous(labels = scales::percent) + xlab("Job Satisfaction of Employee")+ylab("Percentage of Employees") + ggtitle("Plot23. Job Satisfaction Level of Employees V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
job_sat_attr
#Employees with Low Job Satisfaction levels have a very high risk of attrition.

#12. Job Involvement Level vs. Attrition
main_data$JobInvolvement<- as.character(main_data$JobInvolvement)
main_data$JobInvolvement<- gsub(pattern = "1", replacement = "LOW", main_data$JobInvolvement)
main_data$JobInvolvement<- gsub(pattern = "2", replacement = "MEDIUM", main_data$JobInvolvement)
main_data$JobInvolvement<- gsub(pattern = "3", replacement = "HIGH", main_data$JobInvolvement)
main_data$JobInvolvement<- gsub(pattern = "4", replacement = "VERY_HIGH", main_data$JobInvolvement)

job_involve_count<- ggplot(data = main_data, aes(reorder(JobInvolvement, -table(JobInvolvement)[JobInvolvement]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Job Involvement of Employee")+ylab("Count of Employees") + ggtitle("Plot24. Job Involvement Level of Employee vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
job_involve_count

job_involve_attr<- ggplot(data= main_data, aes(x= Attrition,  group=JobInvolvement)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~JobInvolvement) + scale_y_continuous(labels = scales::percent) + xlab("Job Involvement Level of Employee")+ylab("Percentage of Employees") + ggtitle("Plot25. Job Involvement Level of Employees V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
job_involve_attr
#Employees with Low Job Involvement levels have a very high risk of attrition.

#13. Work-Life Balance vs. Attrition
main_data$WorkLifeBalance<- as.character(main_data$WorkLifeBalance)
main_data$WorkLifeBalance<- gsub(pattern = "1", replacement = "BAD", main_data$WorkLifeBalance)
main_data$WorkLifeBalance<- gsub(pattern = "2", replacement = "GOOD", main_data$WorkLifeBalance)
main_data$WorkLifeBalance<- gsub(pattern = "3", replacement = "BETTER", main_data$WorkLifeBalance)
main_data$WorkLifeBalance<- gsub(pattern = "4", replacement = "BEST", main_data$WorkLifeBalance)

work_life_count<- ggplot(data = main_data, aes(reorder(WorkLifeBalance, -table(WorkLifeBalance)[WorkLifeBalance]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Work-Life Balance Level of Employee")+ylab("Count of Employees") + ggtitle("Plot26. Work-Life Balance Level of Employee vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
work_life_count
#38 records with missing Work-Life Balance Levels
work_life_attr<- ggplot(data= main_data, aes(x= Attrition,  group=WorkLifeBalance)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~WorkLifeBalance) + scale_y_continuous(labels = scales::percent) + xlab("Work-Life Balance Level of Employee")+ylab("Percentage of Employees") + ggtitle("Plot27. Work-Life Balance of Employees V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
work_life_attr
#Employees with Bad Work-Life Balance Levels are at a high risk of attrition.

#14. Performance Rating vs. Attrition
main_data$PerformanceRating<- as.character(main_data$PerformanceRating)
main_data$PerformanceRating<- gsub(pattern = "1", replacement = "LOW", main_data$PerformanceRating)
main_data$PerformanceRating<- gsub(pattern = "2", replacement = "GOOD", main_data$PerformanceRating)
main_data$PerformanceRating<- gsub(pattern = "3", replacement = "EXCELLENT", main_data$PerformanceRating)
main_data$PerformanceRating<- gsub(pattern = "4", replacement = "OUTSTANDING", main_data$PerformanceRating)

perf_rat_count<- ggplot(data = main_data, aes(reorder(PerformanceRating, -table(PerformanceRating)[PerformanceRating]), fill=main_data$Attrition)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Performance Rating Level of Employee")+ylab("Count of Employees") + ggtitle("Plot28. Performance Rating Level of Employee vs. Count of Employees")+ scale_y_continuous()+ scale_fill_discrete(name = "Attrition_Status") 
perf_rat_count

perf_rat_attr<- ggplot(data= main_data, aes(x= Attrition,  group=PerformanceRating)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~PerformanceRating) + scale_y_continuous(labels = scales::percent) + xlab("Performance Rating Level of Employee")+ylab("Percentage of Employees") + ggtitle("Plot29. Performance Rating of Employees V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No | 2=Yes")
perf_rat_attr
#Employees with an Outstanding performance rating are more likely to attrition

#**************************** EDA of NUMERICAL VARIABLES ****************************#
#INCLUDING OUTLIER TREATMENT

#1. Age vs. Attrition
summary(main_data$Age)
boxplot(main_data$Age)
quantile(main_data$Age, seq(0,1,0.01))
#Age Parameter does not have any outliers.
age_hist<- ggplot(main_data, aes(x = main_data$Age, fill=main_data$Attrition)) + geom_histogram(binwidth = 2,col = "red")+ xlab("Age of Employee") + ylab("Frequency") + ggtitle("Plot30. Frequency Plot of Age of Employee")+scale_fill_discrete(name = "Attrition_Status")
age_hist

age_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$Age))+ geom_boxplot()+ xlab("Attrition_Status")+ylab("Age of Employee") + ggtitle("Plot31. Age of Employees V/s Attrition Status") 
age_attr
#Younger Employees are more likely to attrition than older ones.

#2. Distance From Home vs. Attrition
summary(main_data$DistanceFromHome)
boxplot(main_data$DistanceFromHome)
quantile(main_data$DistanceFromHome, seq(0,1,0.01))
#Distance From Home does not have any distinct outliers. We will choose not to treat it.
distfrmhome_hist<- ggplot(main_data, aes(x = main_data$DistanceFromHome, fill=main_data$Attrition)) + geom_histogram(binwidth = 2,col = "red")+ xlab("Distance from Home") + ylab("Frequency") + ggtitle("Plot32. Frequency Plot of Distance From Home")+scale_fill_discrete(name = "Attrition_Status")
distfrmhome_hist

distfrmhome_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$DistanceFromHome))+ geom_boxplot()+ xlab("Attrition_Status")+ylab("Distance from Home") + ggtitle("Plot33. Distance from Home of Employees V/s Attrition Status") 
distfrmhome_attr
#No clear distinction betweeen distance from home and Attrition.

#3. Monthly Income vs. Attrition 
summary(main_data$MonthlyIncome)
boxplot(main_data$MonthlyIncome)
#There are outliers on the upper end for Monthly Income
quantile(main_data$MonthlyIncome, seq(0,1,0.01))

#Outlier Treatment: Capping all Monthly incomes above 186437.6/- as 186438/-
main_data$MonthlyIncome[which(main_data$MonthlyIncome>186437.6)]<- 186438

income_hist<- ggplot(main_data, aes(x = main_data$MonthlyIncome, fill=main_data$Attrition)) + geom_histogram(binwidth = 10000,col = "red")+ xlab("Monthly Income of Employee") + ylab("Frequency") + ggtitle("Plot34. Frequency Plot of Monthly Income of Employee")+scale_fill_discrete(name = "Attrition_Status")
income_hist
#Attrition is significant between 20,000-50,000/- monthly income
income_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$MonthlyIncome))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Monthly Income of Employee") + ggtitle("Plot35. Monthly Income of Employees V/s Attrition Status") 
income_attr
#No clear distinction 

#4. Num Companies Worked vs. Attrition
summary(main_data$NumCompaniesWorked)
#19 records with missing Num Companies Worked values
boxplot(main_data$NumCompaniesWorked)
quantile(main_data$NumCompaniesWorked, seq(0,1,0.01), na.rm = TRUE)
#No distinct outliers we will not treat this column
num_comp_hist<- ggplot(data = subset(main_data, is.na(main_data$NumCompaniesWorked)==FALSE), aes(x = as.factor(NumCompaniesWorked), fill=Attrition)) + geom_histogram(stat = "count", col = "red")+ xlab("Number of Companies previously worked In") + ylab("Frequency") + ggtitle("Plot36. Frequency Plot of Number of Companies previously Worked In")+scale_fill_discrete(name = "Attrition_Status")
num_comp_hist
#Employees who have worked in less than 2 companies or more than 8 companies are at a higher risk of attrition
num_comp_attr<- ggplot(data = subset(main_data, is.na(main_data$NumCompaniesWorked)==FALSE), aes(x=as.factor(Attrition), y=NumCompaniesWorked))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Number of Companies previously worked In") + ggtitle("Plot37. Number of Companies previously Worked In V/s Attrition Status") 
num_comp_attr

#5. Percentage Salary Hike vs. Attrition
summary(main_data$PercentSalaryHike)
boxplot(main_data$PercentSalaryHike)
#No distinct outliers in PercentSalaryHike
quantile(main_data$PercentSalaryHike, seq(0,1,0.01))

salary_hike_hist<- ggplot(main_data, aes(x = main_data$PercentSalaryHike, fill=main_data$Attrition)) + geom_histogram(binwidth = 2,col = "red")+ xlab("Salary Hike Percentage of Employee") + ylab("Frequency") + ggtitle("Plot38. Frequency Plot for Salary Hike Percentage of Employee")+scale_fill_discrete(name = "Attrition_Status")
salary_hike_hist

salary_hike_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$PercentSalaryHike))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Salary Hike Percentage of Employee") + ggtitle("Plot39. Salary Hike Percentage of Employee V/s Attrition Status") 
salary_hike_attr
#no clear relation between salary hike and attrition status.

#6. Total Working Years vs. Attrition 
summary(main_data$TotalWorkingYears)
#9 records with missing Total Working Years data
boxplot(main_data$TotalWorkingYears)
#There are Outliers on the Higher Side
quantile(main_data$TotalWorkingYears, seq(0,1,0.01), na.rm = TRUE)
#Outlier Treatment: Capping all Total Working Years >32 years at 32
main_data$TotalWorkingYears[which(main_data$TotalWorkingYears>32)]<- 32

totworkyr_hist<- ggplot(data = subset(main_data, is.na(main_data$TotalWorkingYears)==FALSE), aes(x = TotalWorkingYears, fill=Attrition)) + geom_histogram(binwidth = 2, col = "red")+ xlab("Total Working Years") + ylab("Frequency") + ggtitle("Plot40. Frequency Plot of Total Working Years")+scale_fill_discrete(name = "Attrition_Status")
totworkyr_hist

totworkyr_attr<- ggplot(data = subset(main_data, is.na(main_data$TotalWorkingYears)==FALSE), aes(x=as.factor(Attrition), y=TotalWorkingYears))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Total Working Years") + ggtitle("Plot41. Total Working Years V/s Attrition Status") 
totworkyr_attr
#Employees with Total Working Years <8 years are at a higher risk of attrition

#7. Training Times Last Year vs. Attrition
summary(main_data$TrainingTimesLastYear)
boxplot(main_data$TrainingTimesLastYear)
#There are outliers on both upper and lower ends of the boxplot However we will choose not to treat it as the range is low and to avoid data clumping
quantile(main_data$TrainingTimesLastYear, seq(0,1,0.01))
trntimelyr_hist<- ggplot(main_data, aes(x = main_data$TrainingTimesLastYear, fill=main_data$Attrition)) + geom_histogram(binwidth = 1,col = "red")+ xlab("Number of Times employee was Trained Last Year") + ylab("Frequency") + ggtitle("Plot42. Frequency Plot for Training Times Last Year")+scale_fill_discrete(name = "Attrition_Status")
trntimelyr_hist

trntimelyr_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$TrainingTimesLastYear))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Training frequency of Employee") + ggtitle("Plot43. Training Frequency Last Year V/s Attrition Status") 
trntimelyr_attr
#No distinct difference in training frequency with respect to attrition; both are centered at 3 times.

#8. Years At Company vs. Attrition
summary(main_data$YearsAtCompany)
boxplot(main_data$YearsAtCompany)
#There seem to be significant outliers in the higher values of Years At Company
quantile(main_data$YearsAtCompany, seq(0,1,0.01))
#Outlier Treatment: We will cap Years At Company >25 at 25 [at 98 percentile]
main_data$YearsAtCompany[which(main_data$YearsAtCompany>25)]<- 25

yratcomp_hist<- ggplot(main_data, aes(x = main_data$YearsAtCompany, fill=main_data$Attrition)) + geom_histogram(binwidth = 2,col = "red")+ xlab("Number of Years since joining") + ylab("Frequency") + ggtitle("Plot44. Frequency Plot of Number of years since joining")+scale_fill_discrete(name = "Attrition_Status")
yratcomp_hist

yratcomp_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$YearsAtCompany))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Number of Years since joining") + ggtitle("Plot45. Number of Years since joining V/s Attrition Status") 
yratcomp_attr
#Employees with YearsAtCompany<5 are at a higher risk of attrition

#9. Years Since Last Promotion vs Attrition
summary(main_data$YearsSinceLastPromotion)
boxplot(main_data$YearsSinceLastPromotion)
#There are significant outliers on the upper values of YearsSinceLastPromotion
quantile(main_data$YearsSinceLastPromotion, seq(0,1,0.01))
#Outlier Treatment: Capping all YearsSinceLastPromotion >13 years as 13 years
main_data$YearsSinceLastPromotion[which(main_data$YearsSinceLastPromotion >11)]<-11

yrsinceprom_hist<- ggplot(main_data, aes(x = main_data$YearsSinceLastPromotion, fill=main_data$Attrition)) + geom_histogram(binwidth = 2,col = "red")+ xlab("Number of years since Last Promotion") + ylab("Frequency") + ggtitle("Plot46. Frequency Plot of Number of years since Last Promotion")+scale_fill_discrete(name = "Attrition_Status")
yrsinceprom_hist

yrsinceprom_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$YearsSinceLastPromotion))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Number of years since Last Promotion") + ggtitle("Plot47. Number of years since Last Promotion V/s Attrition Status") 
yrsinceprom_attr
#No Clear distinction between Number of years since Last Promotion and Attrition Status

#10. Years With CurrManager vs. Attrition
summary(main_data$YearsWithCurrManager)
boxplot(main_data$YearsWithCurrManager)
#There are some outliers on the higher values of Years With CurrManager
quantile(main_data$YearsWithCurrManager, seq(0,1,0.01))
#Outlier Treatment: Cappinal all YearsWithCurrManager>14 as 14 years
main_data$YearsWithCurrManager[which(main_data$YearsWithCurrManager>14)]<-14

yrswtcurmgr_hist<- ggplot(main_data, aes(x = main_data$YearsWithCurrManager, fill=main_data$Attrition)) + geom_histogram(binwidth = 2,col = "red")+ xlab("Years worked under current Manager") + ylab("Frequency") + ggtitle("Plot48. Frequency Plot of Years worked under current Manager")+scale_fill_discrete(name = "Attrition_Status")
yrswtcurmgr_hist

yrswtcurmgr_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$YearsWithCurrManager))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Years worked under current Manager") + ggtitle("Plot49. Years worked under current Manager V/s Attrition Status") 
yrswtcurmgr_attr
#Employees with less than 3 Years worked under current Manager are at a higher rate of attrition

#11. Tot.leaves.taken vs. Attrition
summary(main_data$Tot.leaves.taken)
boxplot(main_data$Tot.leaves.taken)
#Number of leaves taken doesn't have significant outliers
quantile(main_data$Tot.leaves.taken, seq(0,1,0.01))

leavestaken_hist<- ggplot(main_data, aes(x = main_data$Tot.leaves.taken, fill=main_data$Attrition)) + geom_histogram(binwidth=2, col = "red")+ xlab("Total Number of Leaves Taken by Employee") + ylab("Frequency") + ggtitle("Plot50. Frequency Plot of Total leaves taken by Employee")+scale_fill_discrete(name = "Attrition_Status")
leavestaken_hist

leavestaken_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$Tot.leaves.taken))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Total Number of Leaves Taken by Employee") + ggtitle("Plot51. Total Number of Leaves Taken by Employee V/s Attrition Status") 
leavestaken_attr
#No clear Distinction but the median number of leaves take by employees who attrition is lower than the median of employees who don't

#12. Tot.logged.hours vs. Attrition
main_data$Tot.logged.hours<- as.numeric(main_data$Tot.logged.hours)
boxplot(main_data$Tot.logged.hours)
#There are significant outliers in the upper region of Tot.logged.hours
quantile(main_data$Tot.logged.hours, seq(0,1,0.01))
#Outlier Treatment: Capping all Tot.logged.hours>2589 as 2590
main_data$Tot.logged.hours[which(main_data$Tot.logged.hours>2589)]<-2590

totloghrs_hist<- ggplot(main_data, aes(x = main_data$Tot.logged.hours, fill=main_data$Attrition)) + geom_histogram(binwidth=100, col = "red")+ xlab("Total Working hours logged by Employee") + ylab("Frequency") + ggtitle("Plot52. Frequency Plot of Total Working hours logged by Employee")+scale_fill_discrete(name = "Attrition_Status")
totloghrs_hist

totloghrs_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$Tot.logged.hours))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Total Working hours logged by Employee") + ggtitle("Plot53. Total Working hours logged by Employee V/s Attrition Status") 
totloghrs_attr
#Median login hours of who have attritioned is greater than the 75th percentile of employees who did not.

#13. Avg.logged.hours vs. Attrition
main_data$Avg.logged.hours<- as.numeric(main_data$Avg.logged.hours)
boxplot(main_data$Avg.logged.hours)
#There seems to be outliers on the higher values of Avg.logged.hours
quantile(main_data$Avg.logged.hours, seq(0,1,0.01))
#Since there is no significant between jump ever percentile we will not cap the values

avgloghrs_hist<- ggplot(main_data, aes(x = main_data$Avg.logged.hours, fill=main_data$Attrition)) + geom_histogram(binwidth=1, col = "red")+ xlab("Average Working hours logged by Employee per Day") + ylab("Frequency") + ggtitle("Plot54. Frequency Plot of Average Working hours logged by Employee")+scale_fill_discrete(name = "Attrition_Status")
avgloghrs_hist

avgloghrs_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$Avg.logged.hours))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Average Working hours logged by Employee per Day") + ggtitle("Plot55. Average Working hours logged by Employee V/s Attrition Status") 
avgloghrs_attr
#Median login hours of who have attritioned is greater than the 75th percentile of employees who did not.

#14. Tot.excess.logged vs. Attrition
main_data$Tot.excess.logged<- as.numeric(main_data$Tot.excess.logged)
boxplot(main_data$Tot.excess.logged)
#There seem to be some outliers in the upper region of Tot.excess.logged
quantile(main_data$Tot.excess.logged, seq(0,1,0.01))
#Outlier Treatment: Capping all Tot.excess.logged > 695.0 at 695.0
main_data$Tot.excess.logged[which(main_data$Tot.excess.logged>695.0)]<-695.0

totexclog_hist<- ggplot(main_data, aes(x = main_data$Tot.excess.logged, fill=main_data$Attrition)) + geom_histogram(binwidth=100, col = "red")+ xlab("Total excess logged hours by Employee") + ylab("Frequency") + ggtitle("Plot56. Frequency Plot of Total excess logged hours by Employee")+scale_fill_discrete(name = "Attrition_Status")
totexclog_hist

totexclog_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$Tot.excess.logged))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Total excess logged hours by Employee") + ggtitle("Plot57. Total excess logged hours by Employee V/s Attrition Status") 
totexclog_attr
#Median excess logged hours of who have attritioned is greater than the 75th percentile of employees who did not.

#15. Avg.excess.logged vs. Attrition
main_data$Avg.excess.logged<- as.numeric(main_data$Avg.excess.logged)
boxplot(main_data$Avg.excess.logged)
quantile(main_data$Avg.excess.logged, seq(0,1,0.01))
#There is no significant jump between percentiles of Avg.excess.logged therefore we will not cap it.

avgexclog_hist<- ggplot(main_data, aes(x = main_data$Avg.excess.logged, fill=main_data$Attrition)) + geom_histogram(binwidth=1, col = "red")+ xlab("Average excess logged hours by Employee") + ylab("Frequency") + ggtitle("Plot58. Frequency Plot of Average excess logged hours by Employee")+scale_fill_discrete(name = "Attrition_Status")
avgexclog_hist

avgexclog_attr<- ggplot(main_data, aes(x=as.factor(main_data$Attrition), y=main_data$Avg.excess.logged))+ geom_boxplot()+ xlab("Attrition Status")+ylab("Average excess logged hours by Employee") + ggtitle("Plot59. Average excess logged hours by Employee V/s Attrition Status") 
avgexclog_attr
#Median excess average logged hours of who have attritioned is greater than the 75th percentile of employees who did not.


#**************************** DATA PREPARATION FOR LOGIT MODEL ****************************#

#Removing the columns which have only one value using the datachop function.
#EmployeeCount, Over18, StandardHours will be removed.
main_data[sapply(main_data, function(datachop) length(unique(datachop)) == 1)] <- NULL

#Missing Value imputation
#finding the columns with atleast 1 NA value
cols_with_na <- colnames(main_data[sapply(main_data, function(x) sum(is.na(x)) > 0)]) 
#So there are 5 columns with the na values

sapply(main_data[cols_with_na], function(x) which(is.na(x)))

#no of rows having na in atleast 1 column
nrow(main_data[!complete.cases(main_data),])
#110 out of 4410 is about 2.5 percent of the data set

#eliminating the rows with NA values in columns
main_without_na <- main_data[complete.cases(main_data),]
#main_na_rows <- main_data[!complete.cases(main_data),]

#percentage of attrition rate with the original data - without deleting the na rows
ggplot(main_data, aes(x=as.factor(main_data$Attrition), fill= factor(main_data$Attrition)))+  geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels = percent) + 
  labs(title = "Plot60. Attrition Rate of Employees With Missing Value Records", y = "Percentage of Total Employees", x = "Attrition Status") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Attrition Status")

#percentage of attrition rate with the data after removing the na rows
ggplot(main_without_na, aes(x=as.factor(main_without_na$Attrition), fill= factor(main_without_na$Attrition)))+  geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels = percent) + 
  labs(title = "Plot62. Attrition Rate of Employees W/O Missing Value Records", y = "Percentage of Total Employees", x = "Attrition Status") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Attrition Status")
#So even after removing the rows with na's there is no bigger difference in the attrition rate.

#ggplot(main_na_rows, aes(x=as.factor(main_na_rows$Attrition), fill= factor(main_na_rows$Attrition)))+  geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels = percent) + 
#  labs(title = "Plot1. Attrition Rate of Employees", y = "Percentage of Total Employees", x = "Attrition Status") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
#  scale_fill_discrete(name = "Attrition Status")

main_data <- main_without_na
rm(main_without_na)

#Transforming categorical variables into dummies
table(main_data$Attrition)
main_data$Attrition <- ifelse(main_data$Attrition == "YES", 1,0)

table(main_data$Gender)
main_data$Gender <- ifelse(main_data$Gender == "MALE", 1,0)

table(main_data$PerformanceRating)
main_data$PerformanceRating <- ifelse(main_data$PerformanceRating == "OUTSTANDING", 1,0)

main_data$Tot.logged.hours <- as.numeric(main_data$Tot.logged.hours)
main_data$Avg.logged.hours <- as.numeric(main_data$Avg.logged.hours)
main_data$Tot.leaves.taken <- as.numeric(main_data$Tot.leaves.taken)
main_data$Tot.excess.logged <- as.numeric(main_data$Tot.excess.logged)
main_data$Avg.excess.logged <- as.numeric(main_data$Avg.excess.logged)

str(main_data)

#Fetching all the character columns
chr_columns <- 
  colnames(main_data[sapply(main_data, function(x) class(x) == 'character')])

#Converting all the character columns to factors
for (i in chr_columns){
  main_data[,i] <- as.factor(main_data[,i])
}

table(main_data$JobLevel)
main_data$JobLevel <- as.factor(main_data$JobLevel)

#Fetching all the factor columns to covert them to dummies
factor_columns <- 
  colnames(main_data[sapply(main_data, function(x) class(x) == 'factor')])

#creating the dummy variables for all the factor columns         
dummies <- data.frame(sapply(main_data[factor_columns], 
                             function(x) data.frame(model.matrix(~x-1,data =main_data[factor_columns]))[,-1]))

#removing all the factor columns from the main data set
main_data[factor_columns] <- NULL
#adding the dummies created to the main data set
main_data <- cbind(main_data, dummies)
rm(dummies)

#Applying the scale function so that all the variables are on one scale
cols_to_scale <- c("Age","DistanceFromHome","MonthlyIncome", "NumCompaniesWorked",
                   "PercentSalaryHike", "TotalWorkingYears","TrainingTimesLastYear",
                   "YearsAtCompany", "YearsSinceLastPromotion","YearsWithCurrManager",
                   "Tot.logged.hours", "Avg.logged.hours","Tot.leaves.taken",
                   "Tot.excess.logged", "Avg.excess.logged")

for(i in cols_to_scale){
  main_data[,i] <- scale(main_data[,i])
}

#Removing EmployeeID in the model_data which will be used for further modelling. We have retained the main_data frame for any reference purposes.
model_data<- main_data[,-1] 

#Structure of Final Prepared Model for Logit Regression
str(model_data)

#Creating Testing and Training Dataset for out Logit Model
set.seed(100)

training_index<- sample.split(model_data$Attrition, SplitRatio = 0.7)

trng_data<- model_data[training_index,]
test_data<- model_data[!training_index,]

#**************************** [IV] LOGIT MODEL BUILDING STAGE ****************************#
#First Model
mod_1<- glm(Attrition~., data = trng_data, family = "binomial")
summary(mod_1)
#Null deviance: 2661.4  on 3009  degrees of freedom
#Residual deviance: 2001.4  on 2950  degrees of freedom
#AIC: 2121.4

#Now using STEP AIC to eliminate insignificant variables
aic_filter<- stepAIC(mod_1, direction = "both")
mod_2<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
             TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
             Tot.logged.hours + Tot.leaves.taken + Tot.excess.logged + 
             BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
             Education.xDOCTOR + EducationField.xLIFE.SCIENCES + EducationField.xMARKETING + 
             EducationField.xMEDICAL + EducationField.xOTHER + EducationField.xTECHNICAL.DEGREE + 
             JobLevel.x2 + JobLevel.x5 + JobRole.xLABORATORY.TECHNICIAN + 
             JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
             JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
             MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
             StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
             JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
             WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + JobInvolvement.xMEDIUM + 
             JobInvolvement.xVERY_HIGH, family = "binomial", data = trng_data)
#Now Checking the output of the Step AIC function for multicollinearity and p-values

summary(mod_2)
vif(mod_2)
#Tot.excess.logged will be eliminated because VIF=2127.796253 and p-value=0.080222

mod_3<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              Tot.logged.hours + Tot.leaves.taken +  
              BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
              Education.xDOCTOR + EducationField.xLIFE.SCIENCES + EducationField.xMARKETING + 
              EducationField.xMEDICAL + EducationField.xOTHER + EducationField.xTECHNICAL.DEGREE + 
              JobLevel.x2 + JobLevel.x5 + JobRole.xLABORATORY.TECHNICIAN + 
              JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
              JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
              MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
              StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
              JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
              WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + JobInvolvement.xMEDIUM + 
              JobInvolvement.xVERY_HIGH, family = "binomial", data = trng_data)
summary(mod_3)
vif(mod_3)
#EducationField.xLIFE.SCIENCES will be eliminated because VIF=8.67 and p-value=2.21E-05

mod_4<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              Tot.logged.hours + Tot.leaves.taken +  
              BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
              Education.xDOCTOR + EducationField.xMARKETING + 
              EducationField.xMEDICAL + EducationField.xOTHER + EducationField.xTECHNICAL.DEGREE + 
              JobLevel.x2 + JobLevel.x5 + JobRole.xLABORATORY.TECHNICIAN + 
              JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
              JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
              MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
              StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
              JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
              WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + JobInvolvement.xMEDIUM + 
              JobInvolvement.xVERY_HIGH, family = "binomial", data = trng_data)
summary(mod_4)
vif(mod_4)
#Now the VIF values for all variables less than 5. Therefore, we will use p-value as a parameter to eliminate insignificant variables
#EducationField.xMEDICAL with p-value=0.220804 will be eliminated

mod_5<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              Tot.logged.hours + Tot.leaves.taken +  
              BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
              Education.xDOCTOR + EducationField.xMARKETING + 
              EducationField.xOTHER + EducationField.xTECHNICAL.DEGREE + 
              JobLevel.x2 + JobLevel.x5 + JobRole.xLABORATORY.TECHNICIAN + 
              JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
              JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
              MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
              StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
              JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
              WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + JobInvolvement.xMEDIUM + 
              JobInvolvement.xVERY_HIGH, family = "binomial", data = trng_data)
summary(mod_5)
vif(mod_5)
#EducationField.xMARKETING with p-value=0.328266 will be eliminated

mod_6<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              Tot.logged.hours + Tot.leaves.taken +  
              BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
              Education.xDOCTOR + 
              EducationField.xOTHER + EducationField.xTECHNICAL.DEGREE + 
              JobLevel.x2 + JobLevel.x5 + JobRole.xLABORATORY.TECHNICIAN + 
              JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
              JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
              MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
              StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
              JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
              WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + JobInvolvement.xMEDIUM + 
              JobInvolvement.xVERY_HIGH, family = "binomial", data = trng_data)
summary(mod_6)
vif(mod_6)
#EducationField.xOTHER with p-value=0.282666 will be eliminated

mod_7<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              Tot.logged.hours + Tot.leaves.taken +  
              BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
              Education.xDOCTOR + 
              EducationField.xTECHNICAL.DEGREE + 
              JobLevel.x2 + JobLevel.x5 + JobRole.xLABORATORY.TECHNICIAN + 
              JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
              JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
              MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
              StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
              JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
              WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + JobInvolvement.xMEDIUM + 
              JobInvolvement.xVERY_HIGH, family = "binomial", data = trng_data)
summary(mod_7)
vif(mod_7)
#EducationField.xTECHNICAL.DEGREE with p-value=0.236652 will be eliminated

mod_8<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              Tot.logged.hours + Tot.leaves.taken +  
              BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
              Education.xDOCTOR + 
              JobLevel.x2 + JobLevel.x5 + JobRole.xLABORATORY.TECHNICIAN + 
              JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
              JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
              MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
              StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
              JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
              WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + JobInvolvement.xMEDIUM + 
              JobInvolvement.xVERY_HIGH, family = "binomial", data = trng_data)
summary(mod_8)
vif(mod_8)
#JobLevel.x5 with p-value=0.191717 will be eliminated

mod_9<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              Tot.logged.hours + Tot.leaves.taken +  
              BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
              Education.xDOCTOR + 
              JobLevel.x2 + JobRole.xLABORATORY.TECHNICIAN + 
              JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
              JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
              MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
              StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
              JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
              WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + JobInvolvement.xMEDIUM + 
              JobInvolvement.xVERY_HIGH, family = "binomial", data = trng_data)
summary(mod_9)
vif(mod_9)
#MaritalStatus.xMARRIED with p-value=0.160189 will be eliminated

mod_10<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + Tot.leaves.taken +  
               BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
               Education.xDOCTOR + 
               JobLevel.x2 + JobRole.xLABORATORY.TECHNICIAN + 
               JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
               JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
               MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
               StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
               JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + JobInvolvement.xMEDIUM + 
               JobInvolvement.xVERY_HIGH, family = "binomial", data = trng_data)
summary(mod_10)
vif(mod_10)
#StockOptionLevel.x3 with p-value=0.12303 will be eliminated

mod_11<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + Tot.leaves.taken +  
               BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
               Education.xDOCTOR + 
               JobLevel.x2 + JobRole.xLABORATORY.TECHNICIAN + 
               JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
               JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
               MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
               JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + JobInvolvement.xMEDIUM + 
               JobInvolvement.xVERY_HIGH, family = "binomial", data = trng_data)
summary(mod_11)
vif(mod_11)
#JobInvolvement.xVERY_HIGH with p-value=0.102565 will be eliminated

mod_12<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + Tot.leaves.taken +  
               BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
               Education.xDOCTOR + 
               JobLevel.x2 + JobRole.xLABORATORY.TECHNICIAN + 
               JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
               JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
               MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
               JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + JobInvolvement.xMEDIUM, family = "binomial", data = trng_data)
summary(mod_12)
vif(mod_12)
#JobInvolvement.xMEDIUM with p-value=0.122508 will be eliminated

mod_13<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + Tot.leaves.taken +  
               BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
               Education.xDOCTOR + 
               JobLevel.x2 + JobRole.xLABORATORY.TECHNICIAN + 
               JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
               JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
               MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
               JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", data = trng_data)
summary(mod_13)
vif(mod_13)
#Education.xDOCTOR with p-value=0.089957 will be eliminated

mod_14<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + Tot.leaves.taken +  
               BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
               JobLevel.x2 + JobRole.xLABORATORY.TECHNICIAN + 
               JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
               JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
               MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
               JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", data = trng_data)
summary(mod_14)
vif(mod_14)
#JobRole.xLABORATORY.TECHNICIAN with p-value=0.055302 will be eliminated

mod_15<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + Tot.leaves.taken +  
               BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
               JobLevel.x2 +JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
               JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
               MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY_HIGH + 
               JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", data = trng_data)
summary(mod_15)
vif(mod_15)
#EnvironmentSatisfaction.xVERY_HIGH with p-value=0.040532 will be eliminated

mod_16<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + Tot.leaves.taken +  
               BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
               JobLevel.x2 +JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
               JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
               MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", data = trng_data)
summary(mod_16)
vif(mod_16)
#JobRole.xRESEARCH.SCIENTIST with p-value=0.041092 will be eliminated

mod_17<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + Tot.leaves.taken +  
               BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
               JobLevel.x2 +JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
               JobRole.xSALES.EXECUTIVE + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", data = trng_data)
summary(mod_17)
vif(mod_17)
#Tot.leaves.taken with p-value=0.035132 will be eliminated

mod_18<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
               JobLevel.x2 +JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
               JobRole.xSALES.EXECUTIVE + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", data = trng_data)
summary(mod_18)
vif(mod_18)
#StockOptionLevel.x1 with p-value=0.032068 will be eliminated

mod_19<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
               JobLevel.x2 +JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
               JobRole.xSALES.EXECUTIVE + MaritalStatus.xSINGLE + 
               EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", data = trng_data)
summary(mod_19)
vif(mod_19)
#JobLevel.x2 with p-value=0.015029 will be eliminated

mod_20<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
               JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
               JobRole.xSALES.EXECUTIVE + MaritalStatus.xSINGLE + 
               EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", data = trng_data)
summary(mod_20)
vif(mod_20)
#BusinessTravel.xTRAVEL_RARELY with p-value=0.002025 and vif=3.525831 will be eliminated

mod_21<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY +JobRole.xMANUFACTURING.DIRECTOR + 
               JobRole.xRESEARCH.DIRECTOR + JobRole.xSALES.EXECUTIVE + MaritalStatus.xSINGLE + 
               EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", data = trng_data)
summary(mod_21)
vif(mod_21)
#JobRole.xSALES.EXECUTIVE with p-value=0.005147 will be eliminated

mod_22<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY +JobRole.xMANUFACTURING.DIRECTOR + 
               JobRole.xRESEARCH.DIRECTOR + MaritalStatus.xSINGLE + EnvironmentSatisfaction.xLOW + 
               JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", data = trng_data)
summary(mod_22)
vif(mod_22)
#JobRole.xRESEARCH.DIRECTOR with p-value=0.016055 will be eliminated

mod_23<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY +JobRole.xMANUFACTURING.DIRECTOR + 
               MaritalStatus.xSINGLE + EnvironmentSatisfaction.xLOW + 
               JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", data = trng_data)
summary(mod_23)
vif(mod_23)
#Trial to remove WorkLifeBalance.xGOOD vif=3.09812 and p-value=1.25E-05

mod_24<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY +JobRole.xMANUFACTURING.DIRECTOR + 
               MaritalStatus.xSINGLE + EnvironmentSatisfaction.xLOW + 
               JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + WorkLifeBalance.xBEST + 
               WorkLifeBalance.xBETTER , family = "binomial", data = trng_data)
summary(mod_24)
vif(mod_24)
#WorkLifeBalance.xBEST with p-value=0.180874  will be eliminated

mod_25<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Tot.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY +JobRole.xMANUFACTURING.DIRECTOR + 
               MaritalStatus.xSINGLE + EnvironmentSatisfaction.xLOW + 
               JobSatisfaction.xLOW + JobSatisfaction.xVERY_HIGH + 
               WorkLifeBalance.xBETTER , family = "binomial", data = trng_data)
summary(mod_25)
vif(mod_25)
#All variables have 3* significance rating
#VIF values for all variables is less than 3
#Test out both mod_23 and mod_25 for evaluation parameters and select the model with the best performace metrics

#**************************** [V] LOGIT MODEL EVALUATION STAGE ****************************#

#Assigning mod_25 as final_model
final_model<- mod_25

#test_predict is a vector to store the forecasted probability of attrition for the test_data using the final_model
test_predict = predict(final_model, type = "response", newdata = test_data[,-2])

#Let's observe the summary of the forcasted probabilities of the test dataset 
summary(test_predict)

#From the test dataset we will encode Attrition=1 as Yes and Attrition=0 as No and store the output in test_actual_attrition
#This test_actual_attrition will be compared with test_cutoff_attrition while evaluating the confusion Matrix
test_actual_attrition <- factor(ifelse(test_data$Attrition==1,"Yes","No"))

#********************** Sensitivity, Specificity and Accuracy Evaluation **********************#

#We have to determine what cutoff probability is to be used to encode the outcome of test_predict as Yes or No.
#We will decide this cutof value based on the optimal accuracy, sensitivity and specificty value using the parameter_optimization function.

parameter_optimization <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_predict >= cutoff, "Yes", "No"))
  conf_mat <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  accuracy_val <- conf_mat$overall[1]
  sensitivity_val <- conf_mat$byClass[1]
  specificity_val <- conf_mat$byClass[2]
  output <- t(as.matrix(c(sensitivity_val, specificity_val, accuracy_val))) 
  colnames(output) <- c("Sensitivity", "Specificity", "Accuracy")
  return(output)
}

#We will create a sequence of 200 cutoff values between 0.01 and 0.80 to be sent as an input to the parameter_optimization function.
cutoff_vector = seq(.01,.80,length=200)
#We have created eval_matrix to store the sensitivity, specificity and accuracy values of each of the 200 input cutoff_vector values from the parameter_optimization function.
eval_mat = matrix(0,200,3)

#Evaluating the sensitivity, specificity and accuracy parameters for each test case.
for(sb in 1:200)
{
  eval_mat[sb,] = parameter_optimization(cutoff_vector[sb])
} 

#Plotting the parameter_values for each cutoff value to determine the optimal cutoff value.
plot(cutoff_vector, eval_mat[,1],xlab="Cutoff_Value",ylab="Parameter_Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff_vector,eval_mat[,2],col="darkgreen",lwd=2)
lines(cutoff_vector,eval_mat[,3],col=4,lwd=2)
title(main = "Plot63. Optimization Parameters")
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#The optimal cutoff value will be the cutoff value for which the difference between Sensitivity and Specificity is minimum.
opt_cutoff <- cutoff_vector[which(abs(eval_mat[,1]-eval_mat[,2])<0.01)]

#Computing the Attrition status of the predicted test_predict vector by using the optimal cutoff value[opt_cutoff]
test_cutoff_attrition <- factor(ifelse(test_predict >= opt_cutoff, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
opt_accuracy <- conf_final$overall[1]
opt_sensitivity <- conf_final$byClass[1]
opt_specificity <- conf_final$byClass[2]

#Storing the optimum parameters in a dataframe optimum_parameters
optimum_parameters<-data.frame(opt_accuracy, opt_sensitivity, opt_specificity, opt_cutoff, row.names = "[final_model]")
optimum_parameters

# Final Model's optimum parameters opt_accuracy=0.7620155 | opt_sensitivity=0.7655502 | opt_specificity=0.7613321 | opt_cutoff=0.1846734
           
#********************** Computing KS Statistic for Test Data **********************#
#Preparing the test_cutoff_attrition and test_actual_attrition for computing the KS Statistic Value
test_cutoff_attr_KS <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attr_KS <- ifelse(test_actual_attrition=="Yes",1,0)

#Predicting values
pred_object_test<- prediction(test_cutoff_attr_KS, test_actual_attr_KS)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

#Computing the Difference
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])

#KS Statistic Value
ks_statistic_max<- max(ks_table_test)
ks_statistic_max
#With a ks_statistic_max of 0.5268823 which is greater than 0.4[40%] our model is viable and justified.

#********************** Gain and Lift Charts **********************#
#Converting test_actual_attrition into a format compatible for Gain and Lift Chart Evaluation
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

#Defining gain_lift_evaluation function to compute gain and lift values for the final model.
#Cutting the 
gain_lift_evaluation<- function(labels,predicted_prob,groups=10) 
  {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>% summarise_at(vars(labels ), funs(total = n(),
              totalresp=sum(., na.rm = TRUE))) %>% mutate(Cumresp = cumsum(totalresp),
              Gain=Cumresp/sum(totalresp)*100, Cumlift=Gain/(bucket*(100/groups))) 
  
  return(gaintable)
}

Attrition_gainlift = gain_lift_evaluation(test_actual_attrition, test_predict, groups = 10)
Attrition_gainlift

#From the Attrition_gainlift table it is clear that within 40% of the test dataset [or within the 4th decile] we are able to grasp 82.3% of all employees who attrition.
#This is a good model considering practical implications. Therefore we will propose final_model as chosen model to predict attrition_status of employees

#********************** Closing Statements **********************#
#Appending the Forecasted Probabilities, Predicted Attrition and Actual Attrition to the test_data frame.
test_actual_attrition <- ifelse(test_actual_attrition==1,"Yes","No")
test_data<- cbind(test_data, test_predict,test_cutoff_attrition, test_actual_attrition)
colnames(test_data)[c(61,62,63)]<- c("ForecastedProb", "PredictedAttrition","ActualAttrition")

#Final Model
summary(final_model)

#Variable Interpretation
# Age has a -ve coefficient implying that younger employees are more likely to attrition when compared to older employees.
# Number of Companies Worked has a +ve coefficient implying that if an employee has previously worked in several companies they are more likely to attrition.
# TotalWorkingYears has a -ve coefficient implying that as the TotalWorkingYears increases the likelihood of attrition will decrease. Senior employees are less likely to attrition.
# TrainingTimesLastYear has a -ve coefficient implying that if employees are sent for frequent training sessions they will be less likely to attrition.
# YearsSinceLastPromotion has a large +ve coefficient implying that if an employee has not been granted a promotion recently they are at a higher risk of attrition.
# YearsWithCurrManager has a -ve coefficient implying that if there is a frequent change in management then there will be an increased risk of attrition.
# Tot.logged.hours has a +ve coefficient and is an interesting parameter that shows if an employee is over-worked or not granted enough leaves he is more likely to attrition.
# BusinessTravel.xTRAVEL_FREQUENTLY has a +ve coefficient implying that an employee is likely to attrition if they are sent on frequent trips for business purposes.
# JobRole.xMANUFACTURING.DIRECTOR has a -ve coefficient implying that they are less likely to attrition.
# MaritalStatus.xSINGLE has a strong +ve coefficient implying that employees who are single are at a higher risk of attrition.
# EnvironmentSatisfaction.xLOW has a strong +ve coefficient implying that employees with a low work environment satisfaction are a very high risk of attrition.
# JobSatisfaction.xLOW has a +ve coefficient implying that employees with a low job satisfaction are at a risk of attrition.
# JobSatisfaction.xVERY_HIGH has a -ve coefficient implying that the employee is less likely to attrition if they feel very satisfied with the job.
# WorkLifeBalance.xBETTER has a -ve coefficient implying that if employees don't have a good work life balance they are more likely to attrition.
