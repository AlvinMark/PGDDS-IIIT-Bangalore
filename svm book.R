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
##Trying SVM model from code line 435
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
unique(survival_db$Deck)
which(survival_db$Deck=="T")
survival_db[340,]
#Clearly ther is only one instance with which the Deck is "T", we will replace this record with "Unknown" to maintain uniformity with test dataset
survival_db$Deck[which(survival_db$Deck=="T")]<- "Unknown"

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


#Deleting PassengerID, Fare/Ticket Number
survival_db<- survival_db[,-c(2,7)]

#Splitting the survival_db into test and training dataset
set.seed(85)
index<- sample.split(survival_db$Survived, SplitRatio = 0.8)
trng_data<- survival_db[index,]
test_data<- survival_db[!index,]

names(trng_data)

#************************************ Building SVM Model ***************************************#

#Lets Try our linear svm model
linear_SVM_mod <- ksvm(Survived~.,
                       data=trng_data,
                       scale=FALSE,
                       kernel="vanilladot")
#Please Note: There will be a warning message [In .local(x, ...) : Variable(s) `' constant. Cannot scale data.]
#This warning message is due to scaling issues as most values are nearing zero and have low variability in each column. It can be ignored.

linear_SVM_mod

#Predicting Labels
linmod_predicted_label<- predict(linear_SVM_mod, test_data)

#Evaluating ConfusionMatrix
lin_conf_mat<- confusionMatrix(linmod_predicted_label, test_data$Survived)
lin_conf_mat

#Trying Polynomial SVM---------> This lead to no improvement over the Linear SVM model. Trying the RBF model
poly_SVM_mod <- ksvm(Survived~.,data=trng_data, scale=FALSE, kernel="polydot")
poly_SVM_mod

#Predicting Labels
polymod_predicted_label<- predict(poly_SVM_mod, test_data)

#Evaluating ConfusionMatrix
poly_conf_mat<- confusionMatrix(polymod_predicted_label, test_data$Survived)
poly_conf_mat

#Trying RBF Mod
RBF_nonlin_SVM_mod <- ksvm(Survived~.,
                           data=trng_data,
                           scale=FALSE,
                           kernel="rbfdot")
#Please Note: There will be a warning message [In .local(x, ...) : Variable(s) `' constant. Cannot scale data.]
#This warning message is due to scaling issues as most values are nearing zero and have low variability in each column. It can be ignored.

RBF_nonlin_SVM_mod

#Predicting Labels
RBFmod_predicted_label<- predict(RBF_nonlin_SVM_mod, test_data)

#Evaluating ConfusionMatrix
RBFmod_conf_mat<- confusionMatrix(RBFmod_predicted_label, test_data$Survived)
RBFmod_conf_mat

#Since RBF mod for SVM gave better results we will proceed with tuning the HyperParamters of RBF mod.
#************************************ Hyperparameter Tuning for RBF SVM_Model ***************************************#

#[5.3]Creating the Cross Validation Control Table
cross_valid_control <- trainControl(method = "cv", number = 5, verboseIter=TRUE)

#Setting the cross validation training set evaluation parameter as Accuracy
eval_parameter <- "Accuracy"

#Setting Seed Value for sampling Training Data Used for Cross Validation
set.seed(85)

#Creating HyperParameter Grid to test Sigma Values: [3.2207e-2, 4.2207e-2, 5.2207e-2] and Cost Values: [1, 2, 3]
hyperpar_grid <- expand.grid(.sigma = c(4.2971e-2, 5.2971e-2, 6.2971e-2),.C=c( 1, 2, 3))

#Evaluating the Three Fold HyperParameter Tuning on the basis of Accuracy [evaluation parameter]
RBF_nonlin_tuned_mod.fit<- train(Survived~.,
                                 data=trng_data,
                                 method="svmRadial",
                                 metric=eval_parameter,
                                 tuneGrid=hyperpar_grid,
                                 trControl=cross_valid_control)

#[5.4]Interpretting the results of the 3-fold cross-validation for HyperParameter Tuning
RBF_nonlin_tuned_mod.fit

#Plotting the Results of model Accuracy for each combination of sigma and cost
plot(RBF_nonlin_tuned_mod.fit)


#Using tuned parameters to buid the final RBF model
RBF_nonlin_final_mod <- ksvm(Survived~.,
                             data=survival_db,
                             kernel="rbfdot",
                             scale=FALSE,
                             C=2,
                             kpar=list(sigma=4.2971e-2))

RBF_nonlin_final_mod

#Final RBF model's performance
RBFfinmod_test_predict_labels <- predict(RBF_nonlin_final_mod, test_data)
RBFfinmod_test_conf_mat<- confusionMatrix(RBFfinmod_test_predict_labels, test_data$Survived)
RBFfinmod_test_conf_mat


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

#Creating the Deck Column
test_db$Deck<- str_sub(test_db$Cabin, start = 1L, end = 1L)
test_db$Deck[which(is.na(test_db$Deck)==TRUE)]<- "Unknown"


#Extracting the Title from the name column
dump1<- data.frame(str_split(test_db$Name, pattern = ", ", n=2), stringsAsFactors = FALSE)
dump1<-data.frame(t(dump1))
dump2<- data.frame(str_split(dump1$X2, pattern = " ", n=2), stringsAsFactors = FALSE)
dump2<-data.frame(t(dump2))
rownames(dump2)<- 1:nrow(dump2)
dump2$X1<- as.character(dump2$X1)
unique(dump2$X1)
#From Here we know that "Dona." is a potuguese title given to women. It is equivalent to Mrs.
#There we have added this title in the women vector
test_db<- cbind(test_db, dump2$X1)
colnames(test_db)[13]<- "Title"
test_db$Title<- as.character(test_db$Title)

for(i in 1:nrow(test_db))
{
  test_db$Title[i]<- nam_title_reg(test_db$Title[i], test_db$Sex[i])
}
unique(test_db$Title)

#Creating the Family Size Metric
test_db$Family<- test_db$SibSp + test_db$Parch


#Creating the Ratio Metric
test_db$Ratio<- test_db$Fare/(test_db$Family+1)
test_db$Ratio<- round(test_db$Ratio,1)

#Removing the Name, Ticket and Cabin columns from the dataset as these columns are not relavant for out analysis
test_db<- test_db[, -c(3,8,10)]

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

#Defining the Measure Column
test_db$Measure<- test_db$Age * test_db$Pclass

#At this Stage we Have successfully adressed all the missing values and feature engineering stages of the Test Dataset

#We Will now prepare the dataset so that it is compatible with our SVM model
#To prepare the dataset we will create dummy variables for the categorical variables and scale all numerical values


#creating dummy variables for the categorical variables
cat_tab<- test_db[,c(2,3,8:10)]
cat_tab<- data.frame(sapply(cat_tab, factor))
cat_tab<- data.frame(model.matrix(~., cat_tab))
#Deck T is clearly missing in the dataset
cat_tab<- cat_tab[,-1]

num_tab<- test_db[,c(4:7,11,12,14)]
num_tab<- sapply(num_tab, scale)

test_db<- cbind(test_db$PassengerId,num_tab, cat_tab)
colnames(test_db)[1]<- "PassengerId"
PassID<- test_db$PassengerId
test_db<- test_db[,-1]


#Using tuned RBF Model to predict survival labels for the test_db dataset
#Predicting the Survival Labels
RBFfinmod_test_predict_labels <- predict(RBF_nonlin_final_mod, test_db)
test_db<- cbind(PassID, test_db, RBFfinmod_test_predict_labels)
colnames(test_db)[1]<- "PassengerId"
colnames(test_db)[24]<-"Survived"

#Submitting the Predictions
amw_svmrbf_titanic<- test_db[,c(1,24)]
write.csv(amw_svmrbf_titanic, "amw_svmrbf_titanic_sub.csv", row.names = FALSE)
