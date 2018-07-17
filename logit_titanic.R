library(stats)
library(ggplot2)
library(dplyr)
library(stringr)
library("MASS")
library("car")
library(caTools)
library(e1071)
library(caret)
library(readr)
library(kernlab)
#************************************ Rough Code Draft***************************************#
##Note: Performance Linear Regression to predict Age=0.75119
##Note: Performance Missing Age replaced with median/mean=0.75598
##Trying SVM model in additional script file to check for model improvement.
survival_db<- read.csv("train.csv", stringsAsFactors = FALSE, na.strings = c("", " ", "na", "NA"))
test_db<- read.csv("test.csv", stringsAsFactors = FALSE, na.strings = c("", " ", "na", "NA"))

sapply(survival_db, function(x) sum(is.na(x)))
#Missing Values in input DB
#Age= 177 
#Cabin= 687[Extract Deck]
#Embarked= 2*

sapply(test_db, function(x) sum(is.na(x)))
#Cabin=327[deleted]
#Fare=1
#Age=86

#Lets Round off Age to one digit after the decimal space
survival_db$Age<- round(survival_db$Age, 1)

survival_db$Embarked[which(is.na(survival_db$Embarked))]<- "S"
#62 and 830 have missing values for Embarked Filled with "S"
dump1<-filter(survival_db, Pclass==1 & Fare>=80 & SibSp==0 & Parch==0 & Fare<=90 )
ggplot(survival_db, aes(x=as.factor(survival_db$Embarked)))+geom_bar()
boxplot(survival_db$Fare~survival_db$Embarked) 


#Extracting Ticket Number from the Ticket column
dump2<-data.frame(str_split(survival_db$Ticket, pattern = " ", n=2), stringsAsFactors = FALSE)
dump2<- data.frame(t(dump2))
rownames(dump2)<- 1:nrow(survival_db)
colnames(dump2)[2]<- "Ticket_Number"
survival_db<-cbind(survival_db, dump2$Ticket_Number)
rownames(survival_db)<- 1:nrow(survival_db)
colnames(survival_db)[13]<- "Ticket_Number"
survival_db$Ticket_Number<- as.character(survival_db$Ticket_Number)
survival_db$Ticket_Number<-str_replace(survival_db$Ticket_Number, pattern = "2. ", replacement = "")
survival_db$Ticket_Number[474]<-"541"
survival_db$Ticket_Number<- str_replace(survival_db$Ticket_Number, pattern = "LINE", replacement = "1466287")
survival_db$Ticket_Number<- as.numeric(as.character(survival_db$Ticket_Number))

#Rounding off Fare to one digit
survival_db$Fare<- round(survival_db$Fare, 1)

#Creating the Deck Column
survival_db$Deck<- str_sub(survival_db$Cabin, start = 1L, end = 1L)
survival_db$Deck[which(is.na(survival_db$Deck)==T)]<- "Unknown"

#Extracting Name Title
dump1<- data.frame(str_split(survival_db$Name, pattern = ", ", n=2), stringsAsFactors = FALSE)
dump1<-data.frame(t(dump1))
dump2<- data.frame(str_split(dump1$X2, pattern = " ", n=2), stringsAsFactors = FALSE)
dump2<-data.frame(t(dump2))
rownames(dump2)<- 1:nrow(dump2)
dump2$X1<- as.character(dump2$X1)
dump2$X1[which(dump2$X1=="the")]<- "Countess."
survival_db<- cbind(survival_db, dump2$X1)
colnames(survival_db)[15]<- "Title"
survival_db$Title<- as.character(survival_db$Title)

#Removing un-necessary columns from the dataset
survival_db<- survival_db[,-c(4,9,11)]
rm(dump1)
rm(dump2)

#Computing the total number of family members
survival_db$Family<- survival_db$SibSp + survival_db$Parch

#Regularizing the Title
unique(survival_db$Title)

men<- c("Mr.", "Don.", "Rev.","Major.","Sir.","Col.", "Capt.", "Jonkheer.")
girls<- c("Ms." , "Mlle.")
women<- c("Mrs.", "Mme.","Lady.", "Countess.", "Dona.")

nam_title_reg<- function(inp_str, gender)
{
  if(inp_str %in% men)
  return("Mr.")
  else if(inp_str %in% women)
    return("Mrs.")
  else if(inp_str %in% girls)
    return("Miss.")
  else if(inp_str=="Dr." & gender=="male")
    return("Mr.")
  else if(inp_str=="Dr." & gender=="female")
    return("Mrs.")
  else return(inp_str)
}

for(i in 1:nrow(survival_db))
{
  survival_db$Title[i]<- nam_title_reg(survival_db$Title[i], survival_db$Sex[i])
}

#Creating a Ratio Metric
survival_db$Ratio<- survival_db$Fare/(survival_db$Family+1)
survival_db$Ratio<- round(survival_db$Ratio, 1)

#Building a LM model to derive Missing Age values


#Trying Correlation plots
dabba<- survival_db[,c(1,3,5,6,7,8,10,13,14)]
dabba<- na.omit(dabba)
cor(dabba)


#Trying an LinReg to predict age.
set.seed(85)
jix<- sample(1:nrow(dabba), 0.8*nrow(dabba))
zax_trng<- dabba[jix,]
zwx_test<- dabba[-jix,]
names(zax_trng)

mod_1<-lm(Age~., data=zax_trng) 
summary(mod_1)

mod_2<- stepAIC(mod_1, direction = "both")
summary(mod_2)
vif(mod_2)
#Arrived at the final model Adjusted R-squared:  0.2144

pred_age<- predict(mod_2, zwx_test)
zwx_test<- cbind(zwx_test, pred_age)
zwx_test$pred_age<- round(zwx_test$pred_age, 1)

cor(zwx_test$Age, zwx_test$pred_age)
#0.507 so that's a decent model

#lm(formula = Age ~ Pclass + SibSp + Parch + Ratio, data = zax_trng)
Estimate_age<- predict(mod_2, survival_db)
survival_db<-cbind(survival_db, Estimate_age)
survival_db$Estimate_age[which(survival_db$Estimate_age<=0)]<- median(survival_db$Age, na.rm = TRUE)
survival_db$Estimate_age<- round(survival_db$Estimate_age, 1)
#Imputing Missing Age values

for(i in 1:nrow(survival_db))
{
  if(is.na(survival_db$Age[i])==TRUE)
  {
    survival_db$Age[i]<- survival_db$Estimate_age[i]
  }
}

survival_db<- survival_db[,-15]

#Defining a Measure Age*Pclass
survival_db$Measure<- survival_db$Age*survival_db$Pclass

rm(dabba)
rm(zax_trng)
rm(zwx_test)
#************************************ Understanding the Data ***************************************#

#Structure of the dataset
str(survival_db)

#Checking the number of distinct variables in the dataset
length(unique(survival_db$PassengerId))
unique(survival_db$Survived)
#Converting Survived to a factor variable
survival_db$Survived<- factor(survival_db$Survived)
unique(survival_db$Pclass)
unique(survival_db$Sex)
length(unique(survival_db$SibSp))
length(unique(survival_db$Parch))
unique(survival_db$Embarked)
length(unique(survival_db$Deck))
unique(survival_db$Title)
length(unique(survival_db$Family))

names(survival_db)
#Transfering all factor variables to one table and creating dummies
cat_tab<- survival_db[,c(3,4,9,11,12)]
cat_tab<- data.frame(sapply(cat_tab, factor))
str(cat_tab)
cat_tab<- data.frame(model.matrix(~., data = cat_tab))
cat_tab<- cat_tab[,-1]
#Deleting Categorical Columns
survival_db<- survival_db[,-c(3,4,9,11,12)]

#Scaling the numeric variables
num_tab<- survival_db[,-2]
num_tab<- data.frame(sapply(num_tab, scale))

survival_db<- data.frame(survival_db[,-c(1,3:10)])

#Combining the dataframes
survival_db<- cbind(survival_db, num_tab)
survival_db<- cbind(survival_db, cat_tab)
colnames(survival_db)[1]<- "Survived"

#Splitting the survival_db into test and training dataset
set.seed(85)
index<- sample.split(survival_db$Survived, SplitRatio = 0.8)
trng_data<- survival_db[index,]
test_data<- survival_db[!index,]

names(trng_data)

#************************************ Logit Model Building ***************************************#

logit_mod_1<- glm(Survived~., trng_data, family = "binomial")
summary(logit_mod_1)

logit_mod_2<- stepAIC(logit_mod_1, direction = "both")
summary(logit_mod_2)
vif(logit_mod_2)
#We will now begin elimination based on p-value and vif
#Since all vif values are within range and less than 3 we will eliminate based on p-value
#We will remove TitleMrs. from the next model as p-value=0.1115 

logit_mod_3<- glm(formula = Survived ~ Age + SibSp + Parch + Ticket_Number + 
                    Pclass2 + Pclass3 + EmbarkedS + DeckB + DeckD + DeckE + TitleMr., family = "binomial", data = trng_data)
summary(logit_mod_3)
vif(logit_mod_3)

#We will remove Ticket Number from the next model as p-value=0.121622
logit_mod_4<- glm(formula = Survived ~ Age + SibSp + Parch + Pclass2 + Pclass3 + EmbarkedS + DeckB 
                  + DeckD + DeckE + TitleMr., family = "binomial", data = trng_data)
summary(logit_mod_4)
vif(logit_mod_4)

#We will remove EmbarkedS from the next model as p-value=0.120394
logit_mod_5<- glm(formula = Survived ~ Age + SibSp + Parch + Pclass2 + Pclass3 + DeckB 
                  + DeckD + DeckE + TitleMr., family = "binomial", data = trng_data)
summary(logit_mod_5)
vif(logit_mod_5)

#We will remove Parch from the next model as p-value=0.052237
logit_mod_6<- glm(formula = Survived ~ Age + SibSp + Pclass2 + Pclass3 + DeckB 
                  + DeckD + DeckE + TitleMr., family = "binomial", data = trng_data)
summary(logit_mod_6)
vif(logit_mod_6)
#logit_mod_6 is a contender for the final model let us remove a few more varibles followed by cross validation for all models

#We will remove DeckB from the next model as p-value=0.033777
logit_mod_7<- glm(formula = Survived ~ Age + SibSp + Pclass2 + Pclass3 +
                    DeckD + DeckE + TitleMr., family = "binomial", data = trng_data)
summary(logit_mod_7)
vif(logit_mod_7)

#We will remove DeckD from the next model as p-value=0.055407
logit_mod_8<- glm(formula = Survived ~ Age + SibSp + Pclass2 + Pclass3 +
                    DeckE + TitleMr., family = "binomial", data = trng_data)
summary(logit_mod_8)
vif(logit_mod_8)

#We will remove DeckE from the next model as p-value=0.059870
logit_mod_9<- glm(formula = Survived ~ Age + SibSp + Pclass2 + Pclass3 +
                    TitleMr., family = "binomial", data = trng_data)
summary(logit_mod_9)
vif(logit_mod_9)
#logit_mod_9 is the optimal reduced model with 5 predictor variables all of which are highly significant for predicting the Survival of the passengers

#************************************ Patrameter Optimization ***************************************#

#Assigning the reduced logit model to final_model for parameter optimization
final_model<- logit_mod_9

#Generating predicted survival probability for the segmented test data
survival_prob<- predict(final_model, type = "response", test_data[,-1])
survival_prob<- as.numeric(survival_prob)
str(survival_prob)
summary(survival_prob)

#Storing the Actual Survived labels of segmented test data into new vector- test_actual_survival
test_actual_survival<- factor(ifelse(test_data$Survived==1, "Yes","No"))
test_actual_survival

#Now we will define a custom parameter optimization function to find the optimal cutoff_probability

parameter_optimization <- function(cutoff_prob) 
{
  predicted_survival <- factor(ifelse(survival_prob >= cutoff_prob, "Yes", "No"))
  conf_mat <- confusionMatrix(predicted_survival, test_actual_survival, positive = "Yes")
  accuracy_val <- conf_mat$overall[1]
  sensitivity_val <- conf_mat$byClass[1]
  specificity_val <- conf_mat$byClass[2]
  output <- t(as.matrix(c(sensitivity_val, specificity_val, accuracy_val))) 
  colnames(output) <- c("Sensitivity", "Specificity", "Accuracy")
  return(output)
}

#We will create a sequence of 100 cutoff values between 0.01 and 0.80 to be sent as an input to the parameter_optimization function.
cutoff_vector = seq(.01,.80,length=100)
#We have created eval_matrix to store the sensitivity, specificity and accuracy values of each of the 200 input cutoff_vector values from the parameter_optimization function.
eval_mat = matrix(0,100,3)

#Evaluating the sensitivity, specificity and accuracy parameters for each test case.
for(sb in 1:100)
{
  eval_mat[sb,] = parameter_optimization(cutoff_vector[sb])
} 

#Plotting the parameter_values for each cutoff value to determine the optimal cutoff value.
plot(cutoff_vector, eval_mat[,1],xlab="Cutoff_Prob",ylab="Parameter_Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff_vector,eval_mat[,2],col="darkgreen",lwd=2)
lines(cutoff_vector,eval_mat[,3],col=4,lwd=2)
title(main = "Plot3. Optimization Parameters")
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#The optimal cutoff value will be the cutoff value for which the difference between Sensitivity and Specificity is minimum.
opt_cutoff_mat <- cutoff_vector[which(abs(eval_mat[,1]-eval_mat[,2])==min(abs(eval_mat[,1]-eval_mat[,2])))]
opt_cutoff<- opt_cutoff_mat[1]
#Computing the Attrition status of the predicted test_predict vector by using the optimal cutoff value[opt_cutoff]
predicted_survival <- factor(ifelse(survival_prob >= opt_cutoff, "Yes", "No"))
conf_final <- confusionMatrix(predicted_survival, test_actual_survival, positive = "Yes")
opt_accuracy <- conf_final$overall[1]
opt_sensitivity <- conf_final$byClass[1]
opt_specificity <- conf_final$byClass[2]

#Storing the optimum parameters in a dataframe optimum_parameters
optimum_parameters<-data.frame(opt_accuracy, opt_sensitivity, opt_specificity, opt_cutoff, row.names = "[final_model]")
optimum_parameters
#Final Model Performance on test_subset. We will now prepare the test_db data and submit our prediction
#                 opt_accuracy	opt_sensitivity	opt_specificity	opt_cutoff		  Model
#[final_model]    0.7808989	      0.7794118	       0.7818182	   0.4409091		logit_mod_9


#************************************ Preparing Test Database ***************************************#

sapply(test_db, function(x) sum(is.na(x)))
#Cabin=327[deleted]
#Fare=1
#Age=86

#Rounding off Age to one decimal space.
test_db$Age<- round(test_db$Age, 1)

#There is one missing value for Fare in our Dataset. We will impute the missing value with the median
#This will not impact the performance as Fare is not a significant parameter to our logit model. But it is required to impute the records with missing age
summary(test_db$Fare)
which(is.na(test_db$Fare)==TRUE)
test_db[153,]
dump1<- filter(test_db, test_db$Pclass==3, test_db$Embarked=="S")
summary(dump1$Fare)
#Clearly the data is skewed. We will impute the missing fare with the median value
test_db$Fare[which(is.na(test_db$Fare)==TRUE)]<- median(test_db$Fare, na.rm = TRUE)
test_db$Fare<- round(test_db$Fare,1)

#Removing Irrelevant columns from the Test Data
test_db<- test_db[,-c(8,10,11)]

#Creating the Family Column
test_db$Family<- test_db$SibSp + test_db$Parch

#Creating the Ratio Column
test_db$Ratio<- test_db$Fare/(test_db$Family+1)

#Using the Linear Regression Model to Predict the age of the records with the missing Age
Estimate_age<- predict(mod_2, test_db)
test_db<- cbind(test_db, Estimate_age)
test_db$Estimate_age[which((test_db$Estimate_age<=0)==TRUE)]<- median(test_db$Age, na.rm = TRUE)
test_db$Estimate_age<- round(test_db$Estimate_age, 1)

#Imputing missing Age with the Estimate Age
for(i in 1:nrow(test_db))
{
  if(is.na(test_db$Age[i])==TRUE)
  {
    test_db$Age[i]<- test_db$Estimate_age[i]
  }
}

#Now we will perform the Name Title Based Extraction
dump1<- data.frame(str_split(test_db$Name, pattern = ", ", n=2), stringsAsFactors = FALSE)
dump1<-data.frame(t(dump1))
dump2<- data.frame(str_split(dump1$X2, pattern = " ", n=2), stringsAsFactors = FALSE)
dump2<-data.frame(t(dump2))
rownames(dump2)<- 1:nrow(dump2)
dump2$X1<- as.character(dump2$X1)

unique(dump2$X1)
which(dump2$X1=="Dona.")
dump2[415,]
dump1[415,]
test_db[415,]
#From Here we know that "Dona." is a potuguese title given to women. It is equivalent to Mrs.
#There we have added this title in the women vector

test_db<- cbind(test_db, dump2$X1)
colnames(test_db)[12]<- "Title"
test_db$Title<- as.character(test_db$Title)

#Regularizing the Name Titles
for(i in 1:nrow(test_db))
{
  test_db$Title[i]<- nam_title_reg(test_db$Title[i], test_db$Sex[i])
}

unique(test_db$Title)

#We will now remove all columns that are irrelevant to the final logit model
test_db<- test_db[,-c(3,4,7,8,9,10,11)]

#Creating dummy variables for the categorical variables
cat_tab<- test_db[,c(2,5)]
cat_tab<- data.frame(sapply(cat_tab, factor))
cat_tab<- data.frame(model.matrix(~., cat_tab))
cat_tab<- cat_tab[,-1]

num_tab<- test_db[,c(3,4)]
num_tab<- data.frame(sapply(num_tab, scale))

test_db<- cbind(test_db$PassengerId,num_tab, cat_tab)
colnames(test_db)[1]<- "PassengerId"
#Dataset is prepared. We will now Predict the survival probability using the logit model optimized with the training model.
survival_prob<- predict(final_model, type = "response", test_db[,-1])
survival_prob<- as.numeric(survival_prob)
str(survival_prob)
summary(survival_prob)

#Now we will extract the predicted labels
predicted_survival <- factor(ifelse(survival_prob >= opt_cutoff, 1, 0))
test_db<-cbind(test_db, predicted_survival)
colnames(test_db)[9]<-"Survived"
#Now we will create a submission file
amw_logit_titanic<- test_db[,c(1,9)]
write.csv(amw_logit_titanic, "amw_logit_titanic_sub.csv", row.names = FALSE)
