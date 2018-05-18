#Handwritten Digit Recognition using SVM Classifier on MNIST dataset.
#Coded By Alvin Mark Windsor [CodeNinja] on 21st April 2018. 
#PGDDS Roll Number DDA1730308.
#Outline of the Assignement Stages:
#[1] Business Understanding
#[2] Data Understanding
#[3] Data Preparation
#[4] Model Building
#4.1. Linear SVM
#4.2. Gaussian Radial Basis kernel SVM [RBF Model]
#[5] Hyperparameter Tuning using Cross Validation
#[6] Final Model Evaluation and Interpretation

#Please Note: Cross Validation Stage requires Significant Computing Resources and has an elapsed run time of 40 minutes.

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> [1] Business Understanding <<<<<<<<<<<<<<<<<<<<<<<<<<<<<#

#MNIST Database stands for the Modified National Institute of Standards and Technology. It is a curated database of handwritten digits used for computer vision, pattern recognition and to train Machine Learning Algorithms.
#The database contains images of handwritten digits each with varying writing styles. 
#Training Dataset has 60,000 images of handwritten digits while Testing Dataset has 10,000 images of handwritten digits.
#Each image is normalized to a 28rowsx28column pixel matrix. The dataset generated from these images has 784 attributes with each attribute representing one element of the pixel matrix.
#Each record of the 784 attributes is filled in with a value between 0-255 both inclusive, these values are representative of the ink density of the corresponding pixel from the image.
#Pixels are organized row-wise. Pixel values are 0 to 255. 0 means background (white), 255 means foreground (black). 
#The Objective of this assignent is to implement SVM algorithm to create a multiclassifier to distinguish these numbers based on the pixel information.
#We will also conduct hyperparameter tuning using 3-fold cross validation.
#For a detailed understanding of the dataset please refer to the following website: http://yann.lecun.com/exdb/mnist/

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> [1.1] Modeling Assumptions <<<<<<<<<<<<<<<<<<<<<<<<<<<<<#

#[1] The testing and training data has already been centered on a 28x28 pixel format with each pixel containing ink density evaluated on a scale of 0-255. We will normalize this pixel information using an explicit scale function.
#[2] A sample is drawn from the training dataset to train the SVM algorithm as the native dataset is extensive and would require high computational resources.
#[3] The training sample drawn such that it is double the size of the testing dataset. This is done so as to reduce magnitude of training error and provide sufficient samples for training the SVM algorithm.
#[4] The training sample is drawn such that it accomodates for 33.33% of the original training dataset. It is assumed that this 33.33% covers all the variants and styles of encoded handwriiten digits.
#[5] It is assumed that the training sample covers majority of the differnt styles of handwritten digits and covers most of the encoded information in the testing dataset.
#[6] Before Proceeding to model building stage we will temporarily merge both testing and training samples with the help of rbind operation in order to eliminate columns with Zero Variance [That is a column in which all records are the same value].
#[7] We will test linear kernel and one RBF kernel model with default parameters to select the kind of model to be used for HyperParameter Tuning. Evaluation of the initial model's performance is done based on the confusionMatrix with test data.
#[8] HyperParameter Tuning will be done on the basis of 3-fold cross validation.
#[9] Please Note: I have tried several combinations of Sigma and Cost values based during modeling phase and have selected a range of values 3x3 to be show as part of the algorithm. 
#[10] The SVM model with the Tuned Hyperparameters was tested on both training and testing data as to show the comparison in Results. The interpretation of the results and Hyperparameters is shown at the final segment of the code.

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> [2] Data Understanding <<<<<<<<<<<<<<<<<<<<<<<<<<<<<#

#[2.1]Importing relevant libraries into the R-enviornment
library(kernlab)
library(caret)
library(caTools)
library(ggplot2)
library(readr)
library(dplyr)
library(stats)

#[2.2]Importing the MNIST test and train dataset into the R-environment
in_train_db<- read.csv("mnist_train.csv", header = FALSE)
in_test_db<- read.csv("mnist_test.csv", header = FALSE)

#[2.3]Both input test and train datasets do not have column names. We will rename the first column of both datasets as "Digit"
#"Digit" is the target variable and is the output label of the pixel information
colnames(in_train_db)[1]<- "Digit"
colnames(in_test_db)[1]<- "Digit"

#[2.4]Checking the dimensions of the in_train_db and the in_test_db datasets
dim(in_train_db)
#Training database has 60000 records with each record having 785 attributes.
#Each of the 60000 records corresponds to the pixel information of 1 handwritten digit. 
#Since each digit is captured in a 28rowsx28columns image there are 28x28=784attributes that correspond to pixel information and 1 attribute containg the label of the Digit.

#Similarly the dimensions of the test dataset are:
dim(in_test_db)
#Testing database has 10000 records with each record having 785 attributes. The interpretation of the attributes is similar to the explaination given above.

#Checking the first few records of the training data
head(in_train_db)
#This shows us the 28x28 pixel information for the digit 5.

#[2.5] Inspecting the Structure of the Testing and Training Dataset to check if Any different Datatypes are present in the dataset.
str(in_train_db)
in_train_db[, lapply(in_train_db[,-1], is.numeric) == FALSE] 
#This Code segment checks all the columns of the training dataset for datatypes other than numeric
#All 784 predictor attributes corresponding to a 60000 Digit are of integer datatype

#Similarly we will check the Testing Dataset
str(in_test_db)
in_test_db[, lapply(in_test_db[,-1], is.numeric) == FALSE] 
#All 784 predictor attributes corresponding to a 10000 Digit are of integer datatype

#[2.6]Inspecting the Dataset for Outliers, If any values in the dataset are lesser than 0 or greater than 255.
sapply(in_train_db[,-1], min)
min(sapply(in_train_db[,-1], min))
#Min Value in Dataset is 0
sapply(in_train_db[,-1], max)
max(sapply(in_train_db[,-1], max))
#Max Value is the 255 in the dataset
#The pixel information has numeric values ranging from 0 to 255[Both inclusive]
#No pixel values exceed the range [0-255] in either Test or Train Data. [I have not included the code for test data as it is not the focus of this stage.]

#[2.7]Checking if the Training Database contains any rare event occourances or if any Digit Label is under-represented
digi_count<- ggplot(data = in_train_db, aes(x=as.factor(Digit))) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Digit Labels")+ylab("Count of Labels") + ggtitle("Plot1. Count of Digit Labels")+ scale_y_continuous()
digi_count
#All the Digits 0-9 are homogeneous in occourance with a variability of only 2% between the most frequent digit 1 and the least frequent digit 5.
#There are no rare event occourances in our training dataset

#[2.8]Inspecting for Missing Values or Missing records in both testing and training Dataset.
sapply(in_train_db, function(x) sum(is.na(x)))
max(sapply(in_train_db, function(x) sum(is.na(x))))
#Therefore there are No Missing values in the Training Dataset

#Similarly Checking for NA values in the Test Dataset
sapply(in_test_db, function(x) sum(is.na(x)))
max(sapply(in_test_db, function(x) sum(is.na(x))))
#There are no missing values in the testing dataset as well

#[2.9]We have completed all data understanding and data quality checks for the training and testing dataset. 
#We will not proceed with EDA for the Training Dataset as each column represents pixel density related measure on a predefined scale. Performing EDA on such attributes will not render any useful insights

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> [3] Data Preparation <<<<<<<<<<<<<<<<<<<<<<<<<<<<<#

#Modeling Assumptions[6]: Before Proceeding to model building stage we will temporarily merge both testing and training samples with the help of rbind operation in order to eliminate columns with Zero Variance [That is a column in which all records are the same value].
#Since our Testing and Training Dataset each have 785 dimensions we will focus on eleminating irrelevant dimensions with insignificant variability.
#We will now focus on reducing the dimentionality of the dataset. However, to ensure uniformity in the dimentions being eliminated we will combine both test and training datasets and conduct feature reduction.
#We will combine both datasets into a main dataframe titled main_db. We will later split it into test and training dataset using the train_index measure.
train_index<- nrow(in_train_db)
main_db<- rbind(in_train_db, in_test_db)

#[3.1] Dimentionality Reduction of Merged Dataset and Scaling

#This local function "datachop" Checks each column of the combined dataset and removes columns with zero variability or columns with only one unique value.
main_db<- main_db[vapply(main_db, function(datachop) length(unique(datachop))>1, logical(1L))]
#65 columns have been eliminated as they had only one unique value and zero variance therefore it will not contribute to modelbuilding

#We have eliminated all columns with zero variance. However, the dimentionality of the data is still significantly high. 
#After Reading several online formuns and websites. I have understood that Principal Component Analysis for dimentionality reduction will not be justified in our dataset due to the pre-requisite hard conditions.
#PCA is Requires the distribution of the attributes to be normal. However, our dataset does not support this claim.
#PCA depends on correlation and variance between attributes. However, the Percentage of uniqueness between columns is in the range of 0.28% and 36.57% therefore the data is too sparsely populated for PCA to work effectively.
#Refer the following links for further information.
#Link1: https://medium.com/data-design/how-to-not-be-dumb-at-applying-principal-component-analysis-pca-6c14de5b3c9d
#Link2: https://en.wikipedia.org/wiki/Principal_component_analysis#Limitations

#Data Pruning Methods
#Since it is not advisable to use PCA we can try other methods to reduce dimensions. One such method is to remove features with near Zero Variance.
#As features with near Zero Variance will not contribute effectively to the behavior of the decision boundary along that dimention.
#Removing Columns With Near Zero Variance
#In the caret package there is a function called nearZeroVar() that checks the dataset for columns with near zero variance
#Read more about nearZeroVar() by using ?nearZeroVar
nzv_main_db<- nearZeroVar(main_db, saveMetrics = TRUE)

#Let us understand the Percentage Range of Unique values in merged dataset
range(nzv_main_db$percentUnique)
#Uniqueness Ranges from 0.285% to 36.57% which is relaticely low,

#Lets understand a few entries in the nzv_main_db
head(nzv_main_db)

#From this output it is clear that the evaluation parameter nvz has the following interpretation.
#nzv=a vector of logicals for whether the predictor is a near zero variance predictor. Therefore we will remove all predictors with nvz parameter equal to TRUE
dim(nzv_main_db[nzv_main_db$nzv==FALSE,])
main_db<- main_db[c(rownames(nzv_main_db[nzv_main_db$nzv==FALSE, ]))]

#We have essentially removed 68.1% of intial dimentionsof intial dimentions [or removed 534 dimentions] by removing all attributes with near Zero Variance
#This will significantly improve computational feasibility and time during HyperParameter Tuning

#Scaling the Pixel information features of the combined dataset. Note: This step is optional as the algorithm performs the same without this step as well.
main_db[,2:ncol(main_db)]<- sapply(main_db[,-1], scale)


#[3.2]Splitting Back into Test and Train Datasets
in_train_db<- main_db[1:train_index,]
test_data<- main_db[(train_index+1):nrow(main_db), ]


#[3.3]Converting the Digit column in Both Datasets to Factor Type
test_data$Digit<- factor(test_data$Digit)
in_train_db$Digit<- factor(in_train_db$Digit)

#[3.4]Sampling of the Training Dataset.
# Refer Modeling Assumptions:
#[2] A sample is drawn from the training dataset to train the SVM algorithm as the native dataset is extensive and would require high computational resources.
#[3] The training sample drawn such that it is double the size of the testing dataset. This is done so as to reduce magnitude of training error and provide sufficient samples for training the SVM algorithm.
#[4] The training sample is drawn such that it samples for 33.33% of the original training dataset[2xTesting Sample= 20,000 and 20,000/60,000=0.3333 | 20,000 records is 33.33% of the original training dataset]. It is assumed that this 33.33% covers all the variants and styles of encoded handwriiten digits.
#[5] It is assumed that the training sample covers majority of the differnt styles of handwritten digits and covers most of the encoded information in the testing dataset.

#Setting Seed Value to Ensure Repeatability of Results
set.seed(100)

train_sample_index <- sample.split(in_train_db$Digit,SplitRatio = 0.3333)
trng_data <- in_train_db[train_sample_index,]
# The sampled Training Data [trng_data] has 19,997 observations from the original training dataset. 

#[3.5] Inspecting the Sampled Training Data For any Bias or Heterogeneity [Rare Even Cases]
digi_freq<- ggplot(data = trng_data, aes(x=Digit)) + geom_bar(stat="count", position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-0.3)+ xlab("Sampled Training Digit Labels")+ylab("Count of Labels") + ggtitle("Plot2. Frequency Plot of Sampled Training Digit Labels")+ scale_y_continuous()
digi_freq
#The sampled Digit Labels seem to be fairly evenly distributed. No rare event cases or bias. Variability in frequency of data labels are around 2.2%

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> [4] Model Building <<<<<<<<<<<<<<<<<<<<<<<<<<<<<#

#Refer Modeling Assumption:
#[7] We will test linear kernel and one RBF kernel model with default parameters to select the kind of model to be used for HyperParameter Tuning. Evaluation of the initial model's performance is done based on the confusionMatrix with test data.

#[4.1] Building Linear SVM Model with Default Parameter Settings
linear_SVM_mod <- ksvm(Digit~.,
                       data=trng_data,
                       scale=FALSE,
                       kernel="vanilladot")
#Please Note: There will be a warning message [In .local(x, ...) : Variable(s) `' constant. Cannot scale data.]
#This warning message is due to scaling issues as most values are nearing zero and have low variability in each column. It can be ignored.

linear_SVM_mod
#Default Parameters Penalty for Misclassification Cost [C=1]
#Number of support vectors influencing decision boundary = 4682

#[4.2] Evaluating Default Linear SVM Model by predicting labels of Test Data
#Predicting Labels of Test data with the help of Trained Model [linear_SVM_mod]
linmod_predicted_label<- predict(linear_SVM_mod, test_data)

#Evaluating ConfusionMatrix
lin_conf_mat<- confusionMatrix(linmod_predicted_label, test_data$Digit)
lin_conf_mat

#[4.3] Interpretting Results of linear_SVM_mod on Test Dataset
#Overall Model Accuracy= 0.8985 [89.85%] | 95% CI: (0.8924, 0.9044)
#Sensitivity [Min-Max] Range [ 0.8265-0.9789 ] or [82.65%-97.89%]
#Specificity [Min-Max] Range [ 0.9816-0.9932 ] or [98.16%-99.32%]

#Polynomial Non-linear SVM Model
#Note: To save on computing time I have removed the polynomial SVM Model as it's performance is not higher than RBF.
#Ploynomial Non-linear SVM Model Definition
#poly_SVM_mod <- ksvm(Digit~.,data=trng_data, scale=FALSE,kernel="polydot")
#Ploynomial Model with Default Parameters renders an Overall Accuracy of 89.86%
#Therefore, we will skip Plynomial SVM Model and jump straight to the RBF model with default parameters.

#[4.4] We will build an RBF model with a non-linear boundary surface with default parameters to check for any improvement in performance.
#Gaussian Radial Basis kernel Model: 
#Please Note: Computing Time for this model will roughly take 4-5 minutes.
RBF_nonlin_SVM_mod <- ksvm(Digit~.,
                           data=trng_data,
                           scale=FALSE,
                           kernel="rbfdot")
#Please Note: There will be a warning message [In .local(x, ...) : Variable(s) `' constant. Cannot scale data.]
#This warning message is due to scaling issues as most values are nearing zero and have low variability in each column. It can be ignored.

RBF_nonlin_SVM_mod
#Default Parameters Penalty for Misclassification Cost [C=1]
#Default Hyperparameter Sigma =  2.2177e-03 [0.002217713]
#Number of support vectors influencing decision boundary = 6132
#Training Error: 0.021653 

#[4.5] Evaluating default non-linear RBF SVM model with Testing Data
RBFmod_predicted_label<- predict(RBF_nonlin_SVM_mod, test_data)

#Evaluating ConfusionMatrix
RBFmod_conf_mat<- confusionMatrix(RBFmod_predicted_label, test_data$Digit)
RBFmod_conf_mat

#[4.6] Interpretting Results of RBF_nonlin_SVM_mod on Test Dataset
#Overall Model Accuracy= 0.9652 [96.52%] | 95% CI: (0.9614, 0.9687)
#Sensitivity [Min-Max] Range [ 0.9395-0.9903 ] or [93.95%-99.03%]
#Specificity [Min-Max] Range [ 0.9945-0.9977 ] or [99.45%-99.77%]


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> [4.7] Default Parameter Model Interpretation <<<<<<<<<<<<<<<<<<<<<<<<<<<<<#

#There is a significant increase in the Accuracy, Sensitivity and Specificity of the Gaussian Radial Basis kernel Model with default parameters when compared to the Linear SVM Model.
#The performance improvement with the Gaussian Radial Basis kernel Model is roughly.
#Overall Accuracy Improvement: +7.42%
#Minimum Sensitivity Improvement: +13.67%
#Minimum Specificity Improvement: +1.31%
#Therefore, we will go ahead and perform Hyperparameter Tuning for the RBF Model using a 3-fold Cross Validation Process.

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> [5] Hyperparameter Tuning using 3-fold Cross Validation <<<<<<<<<<<<<<<<<<<<<<<<<<<<<#

#HyperParameter Tuning will be done on the basis of 3-fold cross validation using Training dataset.
#As the dataset has high dimentionality and cross validation is an iterrative process I have reduced the number of folds in cross validation to 3.
#Hyperparameter Tuning requires heavy computational resources and will execute for roughly 40 minutes for the given set of Parameters.
#I have Tried Several values of the Hyperparameters during Modelling and Have show the 3x3 values of Sigma and Cost that yield the best results.
#Concept for Selecting the HyperParameters:
#[5.1] The Default Sigma Pameter for the Gaussian Radial Basis kernel Model was: Sigma =  2.2177e-03
#This is a low sigma value, implying that the non-linearity of the decision surface is relatively low. This gives us the liberty of varying the Sigma Value During Tuning.
#The optimal solution space for HyperParameters for the given dataset will be near the values of R-program set default value. 
#Therefore we will add and subtract 1.0e-03 [To maintain order of Magnitude] to the default parameter and set the testing range of Sigma.
#We will test the following Range of Sigma Values: [2.217e-3, 3.217e-3, 4.217e-3]
  
#[5.2] The Default Cost Parameter for the Gaussian Radial Basis kernel Model was: Cost C = 1
#Since the non-linearity Hyperparameter Sigma is relatively low in the order of e-03 it gives us some liberty of increasing Cost parameter.
#This can be done cautiously without the risk of overfitting the Training Dataset.
#We will test the following Range of Cost Values: [1, 2, 3]
  
#[5.3]Creating the Cross Validation Control Table
cross_valid_control <- trainControl(method = "cv", number = 3, verboseIter=TRUE)
  
#Setting the cross validation training set evaluation parameter as Accuracy
eval_parameter <- "Accuracy"
  
#Setting Seed Value for sampling Training Data Used for Cross Validation
set.seed(90)
  
#Creating HyperParameter Grid to test Sigma Values: [2.217e-3, 3.217e-3, 4.217e-3] and Cost Values: [1, 2, 3]
hyperpar_grid <- expand.grid(.sigma = c(2.217e-3, 3.217e-3, 4.217e-3),.C=c( 1, 2, 3))
  
#Evaluating the Three Fold HyperParameter Tuning on the basis of Accuracy [evaluation parameter]
RBF_nonlin_tuned_mod.fit<- train(Digit~.,
                                   data=trng_data,
                                   method="svmRadial",
                                   metric=eval_parameter,
                                   tuneGrid=hyperpar_grid,
                                   trControl=cross_valid_control)
  
#[5.4]Interpretting the results of the 3-fold cross-validation for HyperParameter Tuning
RBF_nonlin_tuned_mod.fit
#Please Note: There will be a warning message [In .local(x, ...) : Variable(s) `' constant. Cannot scale data.]
#This warning message is due to scaling issues as most values are nearing zero and have low variability in each column. It can be ignored.
  
#19997 samples in Traning Dataset
#250 predictor attributes
#10 classes for Digit Labels: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'

#Summary of sample sizes: 13331, 13332, 13331 
#Resampling results across tuning parameters:
  
#  sigma     C  Accuracy   Kappa    
#0.002217    1  0.9557431  0.9508096
#0.002217    2  0.9619441  0.9577018
#0.002217    3  0.9641945  0.9602028
#0.003217    1  0.9620442  0.9578135
#0.003217    2  0.9663948  0.9626487
#0.003217    3  0.9679451  0.9643716
#0.004217    1  0.9659448  0.9621489
#0.004217    2  0.9692453  0.9658169
#0.004217    3  0.9695453  0.9661503

#Accuracy was used as the Evaluation Parameter to tune the RBF model's HyperParameters
#The final values used for the Tuned model were sigma = 4.217e-3 [0.004217] and C = 3.

#Plotting the Results of model Accuracy for each combination of sigma and cost
plot(RBF_nonlin_tuned_mod.fit)
#The Highest Model Accuracy obtained during Cross-Validation was for the following tuned parameters
#Tuned HyperParameters Sigma = 4.217e-3 [0.004217] and Cost [C]=3


#[5.5] Building the Final RBF [Gaussian Radial Basis kernel] Model with the above Tuned HyperParameters
RBF_nonlin_final_mod <- ksvm(Digit~.,
                             data=trng_data,
                             kernel="rbfdot",
                             scale=FALSE,
                             C=3,
                             kpar=list(sigma=4.217e-3))

RBF_nonlin_final_mod
#Interpreting Parameters of Final RBF Model
#Tuned HyperParameter Penalty for Misclassification Cost [C=3]
#Tuned Hyperparameter Sigma =  4.217e-3 [0.004217]
#Number of support vectors influencing decision boundary = 6663
#Training Error: 6e-04

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> [6] Model Evaluation of Final RBF Model <<<<<<<<<<<<<<<<<<<<<<<<<<<<<#

#The Final Model Will be evaluated with the Training Data followed by the Testing Data
#This method of evaluation will check for model overfitting concerns
#If the Model's derterministic performance [Accuracy, Sensitivity and Specificity] for test data are comparable with the training data we have arrived at a Stable Model.

#[6.1]Final Model's Performance on Training Data
RBFfinmod_train_predict_labels <- predict(RBF_nonlin_final_mod, trng_data)
RBFfinmod_train_conf_mat<- confusionMatrix(RBFfinmod_train_predict_labels, trng_data$Digit)
RBFfinmod_train_conf_mat
#Final Model Performance on Traning Data
#Overall Accuracy= 0.9994 [99.94%] | 95% CI: (0.999, 0.9997)
#Sensitivity [Min-Max] Range [ 0.9976-1 ] or [99.76%-100%]
#Specificity [Min-Max] Range [ 0.9997-1 ] or [99.97%-100%]

#We have obtained some impressive results with the Training Data, let us evaluate the final model's performance on Test Data

#[6.2]Final Model's Performance on Test Data
RBFfinmod_test_predict_labels <- predict(RBF_nonlin_final_mod, test_data)
RBFfinmod_test_conf_mat<- confusionMatrix(RBFfinmod_test_predict_labels, test_data$Digit)
RBFfinmod_test_conf_mat
#Final Model's Performance on Test Dataset
#Overall Accuracy= 0.9763 [97.63%] | 95% CI: (0.9731, 0.9792)
#Sensitivity [Min-Max] Range [ 0.9584-0.9921 ] or [95.84%-99.21%]
#Specificity [Min-Max] Range [ 0.9964-0.9983 ] or [99.64%-99.83%]

#Final Model's Evaluation
#The Final RBF model with Tuned HyperParameters yielded significantly better results than the RBF model with Default Parameters.
#The Deterministic Evaluation Criterion [Accuracy, Sensitivity and Specificity] of the Final Model for Training and Testing datasets are comparable.
#This implies that the Final Model is not overfitted and can therefore be accepted as a viable model for handwritten digit recognition.

#Comparison of Results
#----------------------------------------------------------------------------------------------------------------------------
#Model	                                                       |  Overall Accuracy |	Min Sensitivity	| Min Specificity
#RBF Model with Tuned Hyper Parameters on Test Data	           |      97.63%	     |     95.84%	      |     99.64%
#RBF Model with Tuned Hyper Parameters on Sample Training Data |	    99.93%	     |     99.76%	      |     99.97%
#RBF Model with Default Parameters on Test Data	               |      96.52%	     |     93.95%	      |     99.45%
#Linear Model with Default Parameters on Test Data	           |      89.85%	     |     82.65%	      |     98.16%
#----------------------------------------------------------------------------------------------------------------------------

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> [6.3] Interpretation of Final RBF Model <<<<<<<<<<<<<<<<<<<<<<<<<<<<<#

#The final RBF model obtained has the following Tuned Hyper Parameters:
#Tuned HyperParameter Penalty for Misclassification Cost [C=3]
#Tuned Hyperparameter Sigma =  4.217e-3 [0.004217]

#Since Tuned Hyperparameter Sigma has a relatively low value of 4.217e-3 [0.004217] it implies that the level of non-linearity of the classification boundary is relatively low.
#Yet, it is significant enough to perform better than a Linear Classification Boundary.
#Since both hyperparameter values are not comarably high the tuned RBF model is not overfitted.
#The Final RBF model has an Overall Accuracy= 0.9763 [97.63%] on the Test Dataset. 


#Model Definition
#RBF_nonlin_final_mod <- ksvm(Digit~.,data=trng_data, kernel="rbfdot", scale=FALSE, C=3, kpar=list(sigma=4.217e-3))

#Final Model's Performance on Test Dataset
#Overall Accuracy= 0.9763 [97.63%] | 95% CI: (0.9731, 0.9792)
#Sensitivity [Min-Max] Range [ 0.9584-0.9921 ] or [95.84%-99.21%]
#Specificity [Min-Max] Range [ 0.9964-0.9983 ] or [99.64%-99.83%]

#Scope for Further Improvement:
#We can work on the Dimentionality Reduction techniques by using other algorithms like RandomForrest to select the most significantly contributing attributes to be given as input to the model.
#We can increase the sample size of the training data and leverage the benefits of Parallel computing to improve the model's performance
#We can further fine tune the HyperParameters by introducing different values of Sigma and Cost [C]. This however will require more computing resources but can be performed if required.


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> [X] End of Code [X] <<<<<<<<<<<<<<<<<<<<<<<<<<<<<#

