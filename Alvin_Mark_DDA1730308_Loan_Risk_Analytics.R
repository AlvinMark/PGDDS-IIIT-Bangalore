#Gramener Case Study Program File by Code Ninjas
#1. Alvin Mark Windsor
#2. Ravi Kiran Vissa
#3. Abdi Adam
#4. Alfred
Sys.Date()
#Installing relevant packages and importing them to the R-environment

library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(scales)

#Set working directory to local system address containing the LC loan transaction dataset.

#Importing The DataSet to the R-environment and replacing all the blank, NA, na, n/a and N/A entries with a standard NA string.
loan_dataset<- read.csv("loan.csv", stringsAsFactors = FALSE, na.strings = c("", " ", "NA","N/A","na","n/a"))


###Data Understanding and Preparation
#We have imported the loan_dataset into the R-environment. However, there are numerous non-essential arrtibutes[columns] with predominantly NA or 0 values. 
#Let us understand the structure of the attributes in the loan_dataset. From here we will assess the requirements for data preparation and manipulation stage.

#To gain an understanding of the number of missing records in our loan_dataset let us examine it using is.na.data.frame() function.
sum(is.na.data.frame(loan_dataset))
#Majority of columns have only NA records or all the records in the column have only one value. Such columns are not useful for analysis and therefore can be removed using the consise function below.


###Data Clensing and Manipulation
#After searching stackoverflow for similar discussions please read the following link for a detailed understanding of how our datachop function performs https://stackoverflow.com/questions/30544282/how-to-remove-columns-with-same-value-in-r
#The datachop function checks all the records of each column for more than 1 unique value, if there is only one unique value or NA it deletes that column from the loan_dataset. The logical(1L) argument specifies that the length function should return a logical value of length 1 integer.
loan_dataset<- loan_dataset[vapply(loan_dataset, function(datachop) length(unique(datachop))>1, logical(1L))]

#Since the five date attributes in the loandataset are not in standard format as they are in the  Month-Year format as Dec-11 for December-2011 we will resolve all date inconsistencies in this section.
#Checking if date separators are standardized to "-" format in the five date columns {issue_d, earliest_cr_line, last_pymnt_d, next_pymnt_d, last_credit_pull_d}
#Using custom fundateconversion function to convert the non-standard date records to a standardized date object and including a dummy date as 01st of every month to represent it as yyyy-mm-dd format for the aforementioned five date attributes.

fundateconversion<- function(input_date)
{
  str_replace_all(input_date, pattern = "[/]", replacement = "-")
  input_date<- paste("01", input_date, sep = "-")
  input_date<-as.Date(input_date, format = "%d-%b-%y")
  return(input_date)
}

loan_dataset$issue_d<- fundateconversion(loan_dataset$issue_d)
loan_dataset$earliest_cr_line<- fundateconversion(loan_dataset$earliest_cr_line)
loan_dataset$last_pymnt_d<- fundateconversion(loan_dataset$last_pymnt_d)
loan_dataset$next_pymnt_d<- fundateconversion(loan_dataset$next_pymnt_d)
loan_dataset$last_credit_pull_d<- fundateconversion(loan_dataset$last_credit_pull_d)

#From the business perspective we are interested in understanding customer attributes and loan attributes that influence the tendency to default the loan leading to credit loss.
#In such cases we will be interested only in analyzing the loan applications that have either the Charged Off or Fully-Paid loan status. 
#We cannot anticipate the outcome of loans that are currently in process as they may result in either being Fully-Paid or Charged-Off resulting in credit loss. Therefore we will remove all records with the loan_status of Current in the dataset.
#Converting the loan_status attribute to uppercase before deleting rows with loan_status=current in order to eliminate any text case inconsistencies in data entry.
loan_dataset<- loan_dataset[-which(toupper(loan_dataset$loan_status)=="CURRENT"),]

##Further removal of non-essential attributes within the dataset.
#Loan Title is a drilldown of the loan purpose attribute and is specific to the applicants loan needs. Since it is a text heavy column with numerous entries branching from a main group it will not be essential for EDA. We will exlude this column.
#Loan desc is again a text based description of the purpose of the loan. Since text analysis isn't under the purview of this case study we will not consider this attribute as well
#URL leads to a web adress specific to a particular loan application record. It is again a non-essential attribute in analyzing driver attributes to credit loss or fradulant loan applicants. We will disregard this column as well.
loan_dataset<- subset(loan_dataset, select = -c(desc, title, url))

#The attributes tax_liens representing the Number of Tax Liens has only 0 or NA values. If we impute the missing values as 0 it will reduce down to a column with the same value. Therefore it will not yield any valuable insights.
#The attribute chargeoff_within_12_mths representing the number of charge-offs within the past year contains records with 0 or NA values. Therefore it will be disregarded for the analysis.
#The attribute collections_12_mths_ex_med representing number of collections in the past year excluding medical collections contains records with 0 or Na values. Therefore we will disregard this attribute.
loan_dataset<- subset(loan_dataset, select = -c(chargeoff_within_12_mths, tax_liens, collections_12_mths_ex_med))

#The schema for this dataset reveals two primary keys [1] "id" denoting the unique ID assigned by LC to the loan application [2] member_id a unique code representing a loan applicant. Having both these primary keys are redundant we can eliminate the member_id and map all records to the unique LC "id".
#Note no analysis will be done on the basis of the LC id either therefore it can technically be removed. However, we will retain it in the main dataframe for further reference as primary key.
loan_dataset<- subset(loan_dataset, select = -c(member_id))

#The data structure of the int_rate and revol_util are represented as character type due to the presence of the % symbol. We will remove the symbol and represent it as a numerical value representative of the interest percentage. i.e. 10.75% will be converted into 10.75.
loan_dataset$int_rate<- as.numeric(gsub("%", "", loan_dataset$int_rate))
loan_dataset$revol_util<- as.numeric(gsub("%", "", loan_dataset$revol_util))

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

for(sb in 1:ncol(loan_dataset))
{
  loan_dataset[,sb]<- caseconversionfun(loan_dataset[,sb])
}

#The emp_title column representing the job title of the loan applicant contains records with numerous special characters and and non-uniform text discrepancies. Therefore we will try to resolve some of these issues by manually sifting through the dataset and resolving the records with the highest frequency of occourances.
loan_dataset$emp_title<- gsub(pattern = "\\.",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "\\,",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "\\'",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "\\:",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "\\-",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "\\(",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "\\)",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "\\/",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "INC",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "CO",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "LTD",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "LLC",replacement = "",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "&",replacement = "AND",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "UNITED STATES",replacement = "US",loan_dataset$emp_title)
loan_dataset$emp_title<- gsub(pattern = "[[:space:]]",replacement = "",loan_dataset$emp_title)


loan_dataset$emp_title[which(loan_dataset$emp_title=="UNITEDSATESARMY")]<- "ARMY"
loan_dataset$emp_title[which(loan_dataset$emp_title=="USARMY")]<- "ARMY"
loan_dataset$emp_title[which(loan_dataset$emp_title=="DEPARTMENTOFTHEARMY")]<- "ARMY"
loan_dataset$emp_title[which(loan_dataset$emp_title=="DEPTOFTHEARMY")]<- "ARMY"
loan_dataset$emp_title[which(loan_dataset$emp_title=="USAIRFORCE")]<- "USAF"
loan_dataset$emp_title[which(loan_dataset$emp_title=="AIRFORCE")]<- "USAF"
loan_dataset$emp_title[which(loan_dataset$emp_title=="USNAVY")]<- "NAVY"
loan_dataset$emp_title[which(loan_dataset$emp_title=="DEPTOFTHENAVY")]<- "NAVY"
loan_dataset$emp_title[which(loan_dataset$emp_title=="DEPARTMENTOFDEFENSENAVY")]<- "NAVY"
loan_dataset$emp_title[which(loan_dataset$emp_title=="SELFEMPLOYED")]<- "SELF"
loan_dataset$emp_title[which(loan_dataset$emp_title=="UNITEDPARCELSERVICE")]<- "UPS"
loan_dataset$emp_title[which(loan_dataset$emp_title=="USPOSTOFFICE")]<- "USPS"
loan_dataset$emp_title[which(loan_dataset$emp_title=="USPOSTALSERVICE")]<- "USPS"
loan_dataset$emp_title[which(loan_dataset$emp_title=="POSTOFFICE")]<- "USPS"
loan_dataset$emp_title[which(loan_dataset$emp_title=="THEHOMEDEPOT")]<- "HOMEDEPOT"
loan_dataset$emp_title[which(loan_dataset$emp_title=="WELLSFARGOBANK")]<- "WELLSFARGO"
loan_dataset$emp_title[which(loan_dataset$emp_title=="WELLSFARGOHOMEMORTGAGE")]<- "WELLSFARGO"
loan_dataset$emp_title[which(loan_dataset$emp_title=="WALMARTSTORES")]<- "WALMART"
loan_dataset$emp_title[which(loan_dataset$emp_title=="ATANDTMOBILITY")]<- "ATANDT"
loan_dataset$emp_title[which(loan_dataset$emp_title=="VERIZONMMUNICATIONS")]<- "VERIZON"
loan_dataset$emp_title[which(loan_dataset$emp_title=="VERIZONWIRELESS")]<- "VERIZON"
loan_dataset$emp_title[which(loan_dataset$emp_title=="ATANDT")]<- "AT&T"

##Since we have completed Dataframe cleaning here we will create two sub dataframes based on the loan status and begin our EDA.
chargedoff_loan<- subset(loan_dataset, loan_dataset$loan_status=="CHARGED OFF")
fullypaid_loan<- subset(loan_dataset, loan_dataset$loan_status=="FULLY PAID")

###Univariate Analysis Begins Here

###-----------------CATEGORICAL VARIABLE ANALYSIS--------------------------------##
#To understand the current business scenario at LC we will plot the percentage of loans that have been fully paid and the percentage of loans that have lead to credit loss.
overall_status<-ggplot(loan_dataset, aes(x=as.factor(loan_dataset$loan_status), fill= factor(loan_dataset$loan_status)))+  geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels = percent) + 
  labs(title = "Plot1. Charged-off vs. Fully Paid", y = "Percentage relative to Total Number of Fixed Loans", x = "Loan_Status") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Loan_Status")
overall_status

#Analysis of Loan Term versus Count of Charged-Off Loans
loan_term<- ggplot(data = chargedoff_loan, aes(x= chargedoff_loan$term)) + geom_bar(stat="count") + geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Loan_Term")+ylab("Count of Charged-off Applicants") + ggtitle("Plot2. Loan Term versus number of Charged of Loan Applicants")
loan_term

#Analysis of LC Assigned grade versus Count of Charged-Off Loans
lc_grade<- ggplot(data = chargedoff_loan, aes(x= chargedoff_loan$grade)) + geom_bar(aes(fill=chargedoff_loan$grade),stat="count") + geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Loan_Grade")+ylab("Count of Charged-off Applicants") + ggtitle("Plot3. Loan Grade versus number of Charged of Loan Applicants")+scale_fill_discrete(name = "LC Assigned Grade")
lc_grade

#Analysis of LC Assigned sub-grade versus Count of Charged-Off Loans
lc_subgrade<- ggplot(data = chargedoff_loan, aes(x= chargedoff_loan$sub_grade)) + geom_bar(stat="count") + geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Loan_SubGrade")+ylab("Count of Charged-off Applicants") + ggtitle("Plot4. Loan Sub-Grade versus number of Charged of Loan Applicants")
lc_subgrade

#Analysis of Employment Length versus Count of Charged-Off Loans
trial1<- subset(chargedoff_loan, is.na(chargedoff_loan$emp_length)==FALSE)
trial1$emp_length<- gsub("< 1 YEAR","0.5 YEAR", trial1$emp_length)
trial1$emp_length<- gsub("10\\+ YEARS", "BEYOND 10 YEARS", trial1$emp_length)
em_length<- ggplot(data = trial1, aes(x= trial1$emp_length)) + geom_bar(stat="count")+ scale_y_continuous(trans='log2') + geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Employment_Duration")+ylab("Count of Charged-off Applicants") + ggtitle("Plot5. Employment Duration versus number of Charged of Loan Applicants")
em_length

#Analysis of Employment Title/Designation
title_grp <- group_by(filter(chargedoff_loan,is.na(emp_title) == FALSE), emp_title)
title_summary <- title_grp %>%dplyr::summarise(count = n()) %>% arrange(desc(count))  
title_summary <- head(title_summary,11)

emp_designation<- ggplot(title_summary, aes(x=title_summary$emp_title, y=title_summary$count))+geom_col()+ geom_text(aes(label=count),vjust=-0.3) + xlab("Employment_Title/Designation")+ylab("Count of Charged-off Applicants") + ggtitle("Plot6. Employment Title versus number of Charged of Loan Applicants")
emp_designation

#Analysis of Home Ownership on Count of Charged-off Loans.
home_ownership<-ggplot(data = chargedoff_loan, aes(reorder(home_ownership, -table(home_ownership)[home_ownership]))) + geom_bar(stat="count") + scale_y_continuous(trans='log2') + geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Home Ownership")+ylab("Count of Charged-off Applicants") + ggtitle("Plot7. Home Ownership versus number of Charged of Loan Applicants") 
home_ownership

prop_home_ownership<-ggplot(chargedoff_loan, aes(reorder(home_ownership, -table(home_ownership)[home_ownership]), fill= factor(chargedoff_loan$home_ownership)))+  geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels = percent) + 
  labs(title = "Plot8. Charged-off vs. Home Ownership", y = "Percentage relative to Total Number of Charged-Off Loans", x = "Home_Ownership") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Home Ownership")
prop_home_ownership

#Analysis of Verification Status vs Count of Charged-Off Loans.
verification_stat<-ggplot(data = chargedoff_loan, aes(reorder(verification_status, -table(verification_status)[verification_status]))) + geom_bar(stat="count") + geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Applicant_Income_Verification_Status")+ylab("Count of Charged-off Applicants") + ggtitle("Plot9. Income Verification Status versus number of Charged of Loan Applicants") 
verification_stat

#Analysis of Loan_Purpose Vs. Count of Charged-Off Loans.
purpose_loan<- ggplot(data = chargedoff_loan, aes(reorder(purpose, -table(purpose)[purpose]))) + geom_bar(aes(fill=chargedoff_loan$purpose),stat="count") + scale_y_continuous(trans='log2') + geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Loan_Purpose")+ylab("Count of Charged-off Applicants") + ggtitle("Plot10. Loan Purpose versus number of Charged of Loan Applicants") + scale_fill_discrete(name = "Loan_Purpose")
purpose_loan

#Analysis of Applicant Residence State Vs Count of Charged-Off Loans
app_state<- ggplot(data = chargedoff_loan, aes(reorder(addr_state, -table(addr_state)[addr_state]))) + geom_bar(stat="count") + scale_y_continuous(trans='log2') + geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Applicant_State")+ylab("Count of Charged-off Applicants") + ggtitle("Plot11. Applicant State versus number of Charged of Loan Applicants")
app_state

#Analysis of Applicant Zip-code Vs Count od Charged-Off Loans
zip_code<-ggplot(data = chargedoff_loan, aes(reorder(zip_code, zip_code)))  + geom_point(stat="count") +  xlab("Applicant Zip Codes") + ylab("Count of Charged-off Applicants") + ggtitle("Plot12. Applicant Zip Code versus number of Charged of Loan Applicants")
zip_code

###-----------------CONTINIOUS NUMERICAL VARIABLE ANALYSIS--------------------------------##

##Funded Amount and Funded Amount INV
boxplot(loan_dataset$funded_amnt)
summary(loan_dataset$funded_amnt)

boxplot(loan_dataset$funded_amnt_inv)
summary(loan_dataset$funded_amnt_inv)

##Loan Amount Analysis
pdf_loanamt<-ggplot(loan_dataset, aes(x = loan_amnt)) + geom_density(fill="green")+ scale_x_continuous(name = "Loan_Amount") + scale_y_continuous(name = "Probability Density")+ ggtitle("Plot13. Density plot of Loan Amount")
pdf_loanamt
#loan amount boxplot and summary
boxplot(loan_dataset$loan_amnt)
summary(loan_dataset$loan_amnt)
#binned loan amounts
bin_loanamt<-ggplot(loan_dataset, aes(x = loan_amnt)) + geom_histogram(binwidth = 3000, col="red")+xlab("Loan amount bins of $3000") + ylab("Frequency") + ggtitle("Plot14. Frequency Plot of Binned Loan Amounts")
bin_loanamt

##Interest Rate
pdf_intrate<-ggplot(loan_dataset, aes(x = int_rate)) + geom_density(fill="green")+ scale_x_continuous(name = "Interest Rate") + scale_y_continuous(name = "Probability Density")+ ggtitle("Plot15. Density plot of Interest Rate")
pdf_intrate

boxplot(loan_dataset$int_rate)
summary(loan_dataset$int_rate)

bin_intrate<-ggplot(loan_dataset, aes(x = int_rate)) + geom_histogram(binwidth = 1,col = "red")+ xlab("Interest Rate bins of 1%") + ylab("Frequency") + ggtitle("Plot16. Frequency Plot of Binned Interest Rate")
bin_intrate

##Loan_Installment Analysis
pdf_install<-ggplot(loan_dataset, aes(x = installment)) + geom_density(fill="green")+ scale_x_continuous(name = "Monthly Loan Installment") + scale_y_continuous(name = "Probability Density")+ ggtitle("Plot17. Density plot of Monthly Loan Installment")
pdf_install

boxplot(loan_dataset$installment)
summary(loan_dataset$installment)
quantile(loan_dataset$installment, 0.95)

#Addressed Outliers and Generated Binned Plot for only Monthly Installments upto $800
bin_monthinstall<- ggplot( subset(loan_dataset, loan_dataset$installment <= 800), aes(x=installment)) + geom_histogram(binwidth = 50,col = "red")+ xlab("Monthly Loan Installment bins of $50") + ylab("Frequency") + ggtitle("Plot18. Frequency Plot of Binned Monthly Loan Installment")
bin_monthinstall

##Annual Income Analysis
boxplot(loan_dataset$annual_inc)
summary(loan_dataset$annual_inc)
quantile(loan_dataset$annual_inc, 0.95)
boxplot(loan_dataset$annual_inc[which(loan_dataset$annual_inc<=140000)])

#Density Plot After Adressing Annual Income Outliers
pdf_annualinc<-ggplot(data= subset(loan_dataset, loan_dataset$annual_inc<=140000), aes(x = annual_inc)) + geom_density(fill="green")+ scale_x_continuous(name = "Applicants Annual Income") + scale_y_continuous(name = "Probability Density")+ ggtitle("Plot19. Density plot of Applicants Annual Income")
pdf_annualinc

#Binned Annual Income
bin_annualinc<- ggplot(data= subset(loan_dataset, loan_dataset$annual_inc<=140000), aes(x=annual_inc)) + geom_histogram(binwidth = 5000,col = "red")+ xlab("Annual Income in bins of $5000") + ylab("Frequency") + ggtitle("Plot20. Frequency Plot of Binned Annual Income")
bin_annualinc


##DTI Analysis
boxplot(loan_dataset$dti)
summary(loan_dataset$dti)

pdf_dti<-ggplot(loan_dataset, aes(x = dti)) + geom_density(fill="green")+ scale_x_continuous(name = "Debt to Income Percentage [DTI]") + scale_y_continuous(name = "Probability Density")+ ggtitle("Plot21. Density plot of Debt to Income Percentage [DTI]")
pdf_dti

bin_dti<- ggplot(loan_dataset, aes(x = dti)) + geom_histogram(binwidth = 1,col = "red")+ xlab("Debt to Income Percentage bins of 1%") + ylab("Frequency") + ggtitle("Plot22. Frequency Plot of Binned Debt to Interest Percentage [DTI]")
bin_dti

##Revolving Line Utilization Rate
boxplot(loan_dataset$revol_util)
summary(loan_dataset$revol_util)

pdf_revolutirat<-ggplot(loan_dataset, aes(x = revol_util)) + geom_density(fill="green")+ scale_x_continuous(name = "Revolving Line Utilization Rate") + scale_y_continuous(name = "Probability Density")+ ggtitle("Plot23. Density plot of Revolving Line Utilization Rate")
pdf_revolutirat

bin_revolutirat<- ggplot(loan_dataset, aes(x = revol_util)) + geom_histogram(binwidth = 5,col = "red")+ xlab("Revolving Line Utilization Rate in bins of 5%") + ylab("Frequency") + ggtitle("Plot24. Frequency Plot of Binned Revolving Line Utilization Rate")
bin_revolutirat


##OpenCredit Lines Analysis
boxplot(loan_dataset$open_acc)
summary(loan_dataset$open_acc)
quantile(loan_dataset$open_acc, 0.95)

bin_opencredline<- ggplot(data = subset(loan_dataset, loan_dataset$open_acc<=17),aes(x = open_acc)) + geom_histogram(binwidth = 1,col = "red")+ xlab("Number of Open Credit Lines in bins of 1") + ylab("Frequency") + ggtitle("Plot25. Frequency Plot of Binned Number of Open Credit Lines")
bin_opencredline

##Collection_Recovery Fees Analysis
boxplot(loan_dataset$collection_recovery_fee)
summary(loan_dataset$collection_recovery_fee)
quantile(loan_dataset$collection_recovery_fee, 0.95)

#Collection Recovery Fee is not valid for Fully Paid Loans
collrecfee<- ggplot(data = subset(loan_dataset, loan_dataset$collection_recovery_fee<=10), aes(x= loan_status, y= collection_recovery_fee, col= loan_status))+ geom_boxplot()+ggtitle("Plot26. Box Plot Comparison of Collection Revovery Fees vs. Loan Status")
collrecfee

nrow(fullypaid_loan[fullypaid_loan$collection_recovery_fee != 0, ])/nrow(fullypaid_loan)

##Number of Delinquencies over the past 2 years
boxplot(loan_dataset$delinq_2yrs)
summary(loan_dataset$delinq_2yrs)
quantile(loan_dataset$delinq_2yrs,0.98)

#Number of delinquencies of over due payments over the past 2 years vs. Loan Status
numdelinq2yr<- ggplot(data = subset(loan_dataset, loan_dataset$delinq_2yrs<=2), aes(x= loan_status, y= delinq_2yrs, col= loan_status))+ geom_boxplot()+ggtitle("Plot27. Box Plot Comparison of Number of Delinquencies vs. Loan Status")
numdelinq2yr
#Too many records with 0 as number of delinquencies. Therefore we can omit this column
nrow(chargedoff_loan[chargedoff_loan$delinq_2yrs!= 0, ])/nrow(chargedoff_loan)
nrow(fullypaid_loan[fullypaid_loan$delinq_2yrs!= 0, ])/nrow(fullypaid_loan)

##Months since last delinquency Analysis
boxplot(loan_dataset$mths_since_last_delinq)
summary(loan_dataset$mths_since_last_delinq)
quantile(loan_dataset$mths_since_last_delinq, 0.98, na.rm = TRUE)

#Very Similar and therefore not useful for analysis
nummonths_ldelnq<- ggplot(data = subset(loan_dataset, is.na(loan_dataset$mths_since_last_delinq)==FALSE & loan_dataset$mths_since_last_delinq<=100), aes(x= loan_status, y= mths_since_last_delinq, col= loan_status))+ geom_boxplot()+ggtitle("Plot27. Box Plot Comparison of Number of Months Since Last Delinquency vs. Loan Status")
nummonths_ldelnq

##Number of Months Since Last Public Record
boxplot(loan_dataset$mths_since_last_record)
summary(loan_dataset$mths_since_last_record)

nummonths_lrec<- ggplot(data = subset(loan_dataset, is.na(loan_dataset$mths_since_last_record)==FALSE), aes(x= loan_status, y= mths_since_last_record, col= loan_status))+ geom_boxplot()+ggtitle("Plot28. Box Plot Comparison of Number of Months Since Last Record vs. Loan Status")
nummonths_lrec

##Number of derogatory public records Analysis
boxplot(loan_dataset$pub_rec)
summary(loan_dataset$pub_rec)

#Majority between 0-1 records not useful for Analysis
numpubrec_derog<- ggplot(data = loan_dataset, aes(x= loan_dataset$pub_rec)) + geom_bar(aes(fill=as.factor(loan_dataset$loan_status)),stat="count") + geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Number of Derogatory Public Records")+ylab("Count of Derogatory Records") + ggtitle("Plot29. Frequency plot of Public Derogatory Records")+scale_fill_discrete(name = "Loan_Status")
numpubrec_derog

##Number of public record bankruptcies Analysis
boxplot(loan_dataset$pub_rec_bankruptcies)
summary(loan_dataset$pub_rec_bankruptcies)
#Majority between 0-1 records therefore  not useful for Analysis
numpubrec_bankrupt<- ggplot(data = subset(loan_dataset, is.na(loan_dataset$pub_rec_bankruptcies)==F), aes(x= pub_rec_bankruptcies)) + geom_bar(aes(fill=as.factor(loan_status)),stat="count") + geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Number of Public Record Bankruptcies")+ylab("Count of Record Bankruptcies") + ggtitle("Plot30. Frequency plot of Pubic Record Bankruptcies")+scale_fill_discrete(name = "Loan_Status")
numpubrec_bankrupt

##Earliest credit line year Analysis
loan_dataset$earliest_cr_line_year<- format(loan_dataset$earliest_cr_line, "%Y")

boxplot(as.numeric(loan_dataset$earliest_cr_line_year))
summary(as.numeric(loan_dataset$earliest_cr_line_year))
#Clearly there are outliers below the year 1960 and above the year 2020. We will exlude this and view the distribution
bin_earlycrlineyr<-ggplot(data= subset(loan_dataset, between(as.numeric(loan_dataset$earliest_cr_line_year),1970,2020)), aes(x = earliest_cr_line_year)) + geom_histogram(binwidth = 5,col = "red", stat = "count")+ xlab("Earliest Credit Line year in bins of 1") + ylab("Frequency") + ggtitle("Plot31. Frequency Plot of Earliest Credit Line Year")
bin_earlycrlineyr

##Loan Issue date Analysis
loan_dataset$issue_year<- format(loan_dataset$issue_d, "%Y")

boxplot(as.numeric(loan_dataset$issue_year))
summary(as.numeric(loan_dataset$issue_year))

#Plot to view the frequency distribution of Loan Issue Year 
bin_issueyr<-ggplot(loan_dataset, aes(x = issue_year)) + geom_histogram(col = "red", stat = "count")+ xlab("Loan Issue year in bins of 1") + ylab("Frequency")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3) + ggtitle("Plot32. Frequency Plot of Loan Issue Year")
bin_issueyr

##Last Credit Pull Year Analysis
loan_dataset$creditpull_year<- format(loan_dataset$last_credit_pull_d, "%Y")

boxplot(as.numeric(loan_dataset$creditpull_year))
summary(as.numeric(loan_dataset$creditpull_year))

bin_credpullyr<-ggplot(data = subset(loan_dataset, is.na(loan_dataset$creditpull_year)==FALSE), aes(x = creditpull_year)) + geom_histogram(col = "red", stat = "count")+ xlab("Last Credit Pull year in bins of 1") + ylab("Frequency")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3) + ggtitle("Plot33. Frequency Plot of Last Credit Pull Year")
bin_credpullyr

##Last Credit Payment Year Analysis
loan_dataset$last_paymentyr<- format(loan_dataset$last_pymnt_d, "%Y")

boxplot(as.numeric(loan_dataset$last_paymentyr))
summary(as.numeric(loan_dataset$last_paymentyr))

bin_lstpymtyr<-ggplot(data = subset(loan_dataset, is.na(loan_dataset$last_paymentyr)==FALSE), aes(x = last_paymentyr)) + geom_histogram(col = "red", stat = "count")+ xlab("Last Credit Pull year in bins of 1") + ylab("Frequency")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3) + ggtitle("Plot34. Frequency Plot of Last Credit Pull Year")
bin_lstpymtyr

##Revolving Balance Analysis
boxplot(loan_dataset$revol_bal)
summary(loan_dataset$revol_bal)
quantile(loan_dataset$revol_bal, 0.98)
boxplot(subset(loan_dataset$revol_bal, loan_dataset$revol_bal<=60000))

##Total Open Credit Lines Analysis
boxplot(loan_dataset$total_acc)
summary(loan_dataset$total_acc)
quantile(loan_dataset$total_acc, 0.98)
boxplot(subset(loan_dataset$total_acc, loan_dataset$total_acc<=55))

##Number of Inquiries in past 6 months 
boxplot(loan_dataset$inq_last_6mths)
summary(loan_dataset$inq_last_6mths)
quantile(loan_dataset$inq_last_6mths,0.98)

numinq_6mths<- ggplot(data = subset(loan_dataset, loan_dataset$inq_last_6mths<=3), aes(x= inq_last_6mths)) + geom_bar(aes(fill=as.factor(loan_status)),stat="count") + geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Number of Inquiries in Past 6 Months")+ylab("Count of Inquiries") + ggtitle("Plot35. Frequency plot of Number of Inquiries in past 6 months") + scale_fill_discrete(name = "Loan_Status")
numinq_6mths


##Last payment Amount Analysis
quantile(loan_dataset$last_pymnt_amnt,0.98)
boxplot(subset(loan_dataset$last_pymnt_amnt, loan_dataset$last_pymnt_amnt<=18000))

##-----------------------------------------------------------Bi-Variate Categorical Analysis-----------------------------------------------------##

#Bivariate Analysis of Grade vs. Loan Status
grade_status<- ggplot(data= subset(loan_dataset, is.na(loan_dataset$emp_length)==F), aes(x= loan_status,  group=grade)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") + facet_grid(~grade) + scale_y_continuous(labels = scales::percent) + xlab("Grade wise Loan Status")+ylab("Percentage of Applicants") + ggtitle("Plot36. Loan Grade versus Loan Status Analysis") + scale_fill_discrete(name = "Loan_Status")
grade_status

#Bivariate Analysis of Sub Grade vs. Loan Status 
subgrde_status<- ggplot(loan_dataset, aes(x= factor(sub_grade), fill=loan_status )) + geom_bar(position = "fill")  + ggtitle("Plot37. Loan Subgrade versus Loan Status Analysis") + xlab("Loan Sub-Grade")+ylab("Percentage of Applicants") + scale_fill_discrete(name = "Loan_Status")
subgrde_status

#Bivariate Analysis of Loan Purpose vs, Loan Status
purpose_status<- ggplot(data= loan_dataset, aes(x= loan_status,  group=purpose)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") + facet_grid(~purpose) + scale_y_continuous(labels = scales::percent) + xlab("Purpose wise Loan Status")+ylab("Percentage of Applicants") + ggtitle("Plot38. Loan Purpose versus Loan Status Analysis") + scale_fill_discrete(name = "Loan_Status")
purpose_status

#Bivariate Analysis of Home Ownership vs. Loan Status
homeowner_status<- ggplot(data= loan_dataset, aes(x= loan_status,  group=home_ownership)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") + facet_grid(~home_ownership) + scale_y_continuous(labels = scales::percent) + xlab("Home Ownership wise Loan Status")+ylab("Percentage of Applicants") + ggtitle("Plot40. Home Ownership versus Loan Status Analysis") + scale_fill_discrete(name = "Loan_Status")
homeowner_status

#Bivariate Analysis of Applicant's Income Verification vs. Loan Status
incverify_status<- ggplot(data= loan_dataset, aes(x= loan_status,  group=verification_status)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") + facet_grid(~verification_status) + scale_y_continuous(labels = scales::percent) + xlab("Applicant's Income Verification wise Loan Status")+ylab("Percentage of Applicants") + ggtitle("Plot41. Applicant's Income Verification Status versus Loan Status Analysis") + scale_fill_discrete(name = "Loan_Status")
incverify_status

#Bivariate Analysis of Loan Term vs. Loan Status
term_status<- ggplot(data= loan_dataset, aes(x= loan_status,  group=term)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") + facet_grid(~term) + scale_y_continuous(labels = scales::percent) + xlab("Loan Term wise Loan Status")+ylab("Percentage of Applicants") + ggtitle("Plot42. Applicant's Loan Term versus Loan Status Analysis") + scale_fill_discrete(name = "Loan_Status")
term_status

#Bivariate Analysis of Applicant's Residence State vs. Loan Status
resstate_status<-ggplot(loan_dataset, aes(x= factor(loan_dataset$addr_state), fill=loan_status )) + geom_bar(position = "fill") +  ggtitle("Plot43. Applicant Residence State versus Loan Status Analysis") + xlab("Address State")+ylab("Percentage of Applicants") + scale_fill_discrete(name = "Loan_Status")
resstate_status
# NE - Nebraska shows that they are likely to default with over 50% of the loan applicants resulting in credit loss, however the sample size for this state is considerably low [i.e 5]
# NV- Nevada is the next state with about 22-23% of the loan applicants resulting in credit loss

#Bivariate Analysis of Employment Duration vs. Loan Status
loan_dataset$emp_length<- gsub("< 1 YEAR","0.5 YEAR", loan_dataset$emp_length)
loan_dataset$emp_length<- gsub("10\\+ YEARS", "BEYOND 10 YEARS", loan_dataset$emp_length)

empdura_status<- ggplot(data= subset(loan_dataset, is.na(loan_dataset$emp_length)==F), aes(x= loan_status,  group=emp_length)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") + facet_grid(~emp_length) + scale_y_continuous(labels = scales::percent) + xlab("Employment Duration wise Loan Status")+ylab("Percentage of Applicants") + ggtitle("Plot44. Applicant's Employment Duration versus Loan Status Analysis") + scale_fill_discrete(name = "Loan_Status")
empdura_status


#Bivariate Analysis of Employment Title vs. Loan Status
title_grp <- group_by(filter(loan_dataset,is.na(emp_title) == FALSE), emp_title)
title_summary <- title_grp %>%dplyr::summarise(count = n()) %>% arrange(desc(count))  
title_summary <- head(title_summary,10)

#creating a copy of the main dataframe
temp<- loan_dataset
temp1<-temp[which(temp$emp_title %in% title_summary$emp_title), ]

#plot of designation vs loan status
desigtitle_status<-ggplot(temp1, aes(x= loan_status,  group=emp_title)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") + facet_grid(~emp_title) + scale_y_continuous(labels = scales::percent)+ scale_x_discrete(labels=abbreviate) + xlab("Employment Title of Applicant vs. Loan Status")+ylab("Percentage of Applicants") + ggtitle("Plot45. Applicant's Employment Title versus Loan Status Analysis") + scale_fill_discrete(name = "Loan_Status")
desigtitle_status

rm(temp)
rm(temp1)
rm(title_grp)
rm(title_summary)
rm(trial1)

##-----------------------------------------------Continious Bi-Variable Analysis--------------------------------------------------------------------##

#Bi-Variate Loan Amount Analysis
loanamt_status<-ggplot(loan_dataset, aes(x = loan_amnt, fill= as.factor(loan_status))) + geom_histogram(binwidth = 5000)+xlab("Loan amount in bins of $5000") + ylab("Frequency") + ggtitle("Plot46. Frequency plot of Binned Loan Amounts vs Loan Status") + scale_fill_discrete(name = "Loan_Status")
loanamt_status

#Bivariate Analysis of Annual Income~Loan Purpose
annualinc_purp<- ggplot(data= subset(loan_dataset, loan_dataset$annual_inc<=140000), aes(x=annual_inc, fill= as.factor(purpose))) + geom_histogram(binwidth = 10000)+ xlab("Annual Income in bins of $10000") + ylab("Frequency") + ggtitle("Plot47. Frequency Plot of Binned Annual Income vs Loan Purpose") + scale_fill_discrete(name = "Loan_Purpose")
annualinc_purp

#Monthly Installments
mthlyinstall_status<- ggplot( subset(loan_dataset, loan_dataset$installment <= 800), aes(x=installment, fill= as.factor(loan_status))) + geom_histogram(binwidth = 200)+ xlab("Monthly Loan Installment bins of $200") + ylab("Frequency") + ggtitle("Plot48. Frequency Plot of Binned Monthly Loan Installment vs Loan Status") + scale_fill_discrete(name = "Loan_Purpose")
mthlyinstall_status
