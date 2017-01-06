# Reading the data from the Disk
loan_data<-read.csv("D:/RWork/LoansTrainingSetV2.csv",stringsAsFactors = FALSE)

# Understanding about the dataset
summary(loan_unique)
str(loan_unique)

#filtering unique loan records from the raw dataset
loan_unique<-loan_data[row.names(as.data.frame(unique(loan_data$Loan.ID))),]

loan_unique$Loan.Status<-as.factor(loan_unique$Loan.Status)
levels(loan_unique$Loan.Status)
table(loan_unique$Loan.Status)


class(loan_unique$Current.Loan.Amount)
# It is a numeric vector.
summary(loan_unique$Current.Loan.Amount)
# There are no missing values. But there are outliers.There is a big difference between mean and median
library(ggplot2)
qplot(loan_unique$Current.Loan.Amount,geom="histogram")
# Histogram suggests that we have outliers.
# Seperating them using IQR methodology.
outlier<-boxplot.stats(loan_unique$Current.Loan.Amount)
length(outlier$out)
# Replacing them with NA's
loan_unique$Current.Loan.Amount[loan_unique$Current.Loan.Amount>=min(outlier$out)]<-NA
summary(loan_unique$Current.Loan.Amount)
qplot(loan_unique$Current.Loan.Amount,geom="histogram")


loan_unique$Term<-as.factor(loan_unique$Term)
levels(loan_unique$Term)
table(loan_unique$Term)

summary(loan_unique$Credit.Score)
# It has NA's and the maximum value is 7510. But it should be between 0-800.
qplot(loan_unique$Credit.Score)
# We will replace all the values greater than 800 by NA's
loan_unique$Credit.Score[loan_unique$Credit.Score>800]<-NA
summary(loan_unique$Credit.Score)
qplot(loan_unique$Credit.Score)
#Missing value treatment, replacing missing values by median
loan_unique$Credit.Score[is.na(loan_unique$Credit.Score)==TRUE]<-median(loan_unique$Credit.Score,na.rm = T)

class(loan_unique$Years.in.current.job)
# It is a character vector.
table(loan_unique$Years.in.current.job)
# Needs to be converted to factor and "n/a" needs to be replaced to NA.
# Replacing "n/a"
loan_unique$Years.in.current.job[loan_unique$Years.in.current.job == "n/a"] <- NA
table(loan_unique$Years.in.current.job)
sum(is.na(loan_unique$Years.in.current.job))
loan_unique$Years.in.current.job[is.na(loan_unique$Years.in.current.job)==TRUE] <- "(Other)"
#Converting character to factors
loan_unique$Years.in.current.job<-as.factor(loan_unique$Years.in.current.job)
summary(loan_unique$Years.in.current.job)

class(loan_unique$Home.Ownership)
# It's a character vector
table(loan_unique$Home.Ownership)
# Needs to be converted to factor and "HaveMortgage" needs to be converted to "Home Mortgage"
loan_unique$Home.Ownership[loan_unique$Home.Ownership == "HaveMortgage"] <- "Home Mortgage"
loan_unique$Home.Ownership<-as.factor(loan_unique$Home.Ownership)
summary(loan_unique$Home.Ownership)

class(loan_unique$Annual.Income)
# It's a numeric vector
summary(loan_unique$Annual.Income)
qplot(loan_unique$Annual.Income)
quantile(loan_unique$Annual.Income,probs = seq(0,1,0.05),na.rm=TRUE)
# Outliers are in the top 5 % data only
outlier<-quantile(loan_unique$Annual.Income,probs = seq(0.95,1,0.01),na.rm=TRUE)
# Capping any values greater than 99% to 99th value
loan_unique$Annual.Income[loan_unique$Annual.Income>outlier[5]]<-outlier[5]
summary(loan_unique$Annual.Income)
qplot(loan_unique$Annual.Income)
loan_unique$Annual.Income[is.na(loan_unique$Annual.Income)==TRUE]<-median(loan_unique$Annual.Income,na.rm = T)

class(loan_unique$Purpose)
# It's a character vector
table(loan_unique$Purpose)
# Needs to be converted to a factor, and "other" and "Other" has to be merged
loan_unique$Purpose[loan_unique$Purpose == "other"] <- "Other"
table(loan_unique$Purpose)
loan_unique$Purpose<-as.factor(loan_unique$Purpose)
summary(loan_unique$Purpose)

class(loan_unique$Monthly.Debt)
# It's a character vector, but needs to be converted to numeric
library(stringr)
# It has $ sign and "," which needs to be replaced
# Replacing "$" sign with ""
loan_unique$Monthly.Debt<- str_replace_all(loan_unique$Monthly.Debt, fixed("$"), "")
#Replacing "," with ""
loan_unique$Monthly.Debt<- str_replace_all(loan_unique$Monthly.Debt, fixed(","), "")
# Converting to numeric
loan_unique$Monthly.Debt<-as.numeric(loan_unique$Monthly.Debt)
summary(loan_unique$Monthly.Debt)
qplot(loan_unique$Monthly.Debt)
# It has outliers, checking the quantiles
quantile(loan_unique$Monthly.Debt,probs = seq(0,1,0.05))
# Outliers are from 95 to 100. Let's dig deeper.
quantile(loan_unique$Monthly.Debt,probs = seq(0.95,1,0.01))
#Only 100th percentile is an outlier. We will replace it with 99th percentile
outlier<-quantile(loan_unique$Monthly.Debt,probs =c(0.99,1))
loan_unique$Monthly.Debt[loan_unique$Monthly.Debt>outlier[1]]<-outlier[1]
qplot(loan_unique$Monthly.Debt)

class(loan_unique$Years.of.Credit.History)
# It is a numeric vector
summary(loan_unique$Years.of.Credit.History)
qplot(loan_unique$Years.of.Credit.History)
# Looks Clean, no need of any processing

class(loan_unique$Months.since.last.delinquent)
# It is a numeric vector
summary(loan_unique$Months.since.last.delinquent)
# Has lots of NA's. 
# Checking for outliers
qplot(loan_unique$Months.since.last.delinquent)
# We will treat NA's later, if required.

class(loan_unique$Number.of.Open.Accounts)
# It is a numeric vector
summary(loan_unique$Number.of.Open.Accounts)
# Checking for outliers
qplot(loan_unique$Number.of.Open.Accounts)
quantile(loan_unique$Number.of.Open.Accounts,probs = seq(0.95,1,0.01))
# Only the 100th percentile is an outlier,replacing it with 99 i
outlier<-quantile(loan_unique$Number.of.Open.Accounts,probs =c(0.99,1))
loan_unique$Number.of.Open.Accounts[loan_unique$Number.of.Open.Accounts>outlier[1]]<-outlier[1]
qplot(loan_unique$Number.of.Open.Accounts)

class(loan_unique$Number.of.Credit.Problems)
# It is a numeric vector
summary(loan_unique$Number.of.Credit.Problems)
# Checking for outliers
qplot(loan_unique$Number.of.Credit.Problems)
quantile(loan_unique$Number.of.Credit.Problems,probs = seq(0,1,0.05))
table(loan_unique$Number.of.Credit.Problems)
#Cleaning not requried

class(loan_unique$Current.Credit.Balance)
summary(loan_unique$Current.Credit.Balance)
# It is a numeric vector
#Checking for outliers
qplot(loan_unique$Current.Credit.Balance)
quantile(loan_unique$Current.Credit.Balance,probs = seq(0,1,0.05))
# Capping as per IQR 
outlier<-boxplot.stats(loan_unique$Current.Credit.Balance)
summary(outlier$out)
quantile(loan_unique$Current.Credit.Balance,probs = seq(0.95,1,0.01))
loan_unique$Current.Credit.Balance[loan_unique$Current.Credit.Balance>min(outlier$out)]<-min(outlier$out)
qplot(loan_unique$Current.Credit.Balance)

class(loan_unique$Maximum.Open.Credit)
head(loan_unique$Maximum.Open.Credit)
# It's a character vector,but has numeric values.
# It has some junk values such as "#VALUE!", which needs to be replaced with NA
loan_unique$Maximum.Open.Credit<- str_replace_all(loan_unique$Maximum.Open.Credit, fixed("#VALUE!"),NA)
#  Converting to numeric data
loan_unique$Maximum.Open.Credit<-as.numeric(loan_unique$Maximum.Open.Credit)
summary(loan_unique$Maximum.Open.Credit)
# It also has outliers
qplot(loan_unique$Maximum.Open.Credit)
#Checking the quantiles
quantile(loan_unique$Maximum.Open.Credit,probs = seq(0,1,0.05),na.rm = T)
quantile(loan_unique$Maximum.Open.Credit,probs = seq(0.95,1,0.01),na.rm = T)
quantile(loan_unique$Maximum.Open.Credit,probs = seq(0.99,1,0.001),na.rm = T)
# Capping beyond 150000
loan_unique$Maximum.Open.Credit[loan_unique$Maximum.Open.Credit>150000]<-150000
qplot(loan_unique$Maximum.Open.Credit)
loan_unique$Maximum.Open.Credit[is.na(loan_unique$Maximum.Open.Credit)==TRUE]<-median(loan_unique$Maximum.Open.Credit,na.rm = T)

#Replacing NA's with 0's
loan_unique$Bankruptcies[is.na(loan_unique$Bankruptcies)==TRUE]<-0

#Replacing NA's with 0's
loan_unique$Tax.Liens[is.na(loan_unique$Tax.Liens)==TRUE]<-0

#Most of the data is NA's so removing the column from the dataset
loan_unique$Months.since.last.delinquent<-NULL

#Imputing missing data using Mice Package
library(mice)
simple<-loan_unique[,4:18]
impute<-mice(simple)
loan_complete<-complete(impute)
summary(loan_complete)
#Adding loan status
loan_complete<-cbind(loan_complete,Loan.Status=loan_unique$Loan.Status)

#--------------------------------------------------------------------------------------
#Splitting Enter dataset into Train and Test datasets

set.seed(8888)
library(caTools)
# Converting labels to 0 and 1 
loan_complete$Loan.Status<-ifelse(loan_complete$Loan.Status=="Charged Off",1,0)
spl<-sample.split(loan_complete$Loan.Status,SplitRatio = 0.7)
train<-subset(loan_complete,spl==TRUE)
test<-subset(loan_complete,spl==FALSE)

#--------------------------------------------------------------------------------------
#XGBoost Model implemenation & validation

library(xgboost)
library(Matrix)
# Preparing sparse matrix
sparse.train<-sparse.model.matrix(Loan.Status~.-1,data = train)
sparse_mat<-as.matrix(sparse.train)
dtrain<-xgb.DMatrix(data=sparse.train,label=train$Loan.Status)
sparse.test<-sparse.model.matrix(Loan.Status~.-1,data=test)
dtest<-xgb.DMatrix(data=sparse.test,label=test$Loan.Status)
watchlist<-list(train=dtrain,test=dtest)
params<-list(eta=0.01,max_depth=10,objective="binary:logistic")
model_xgb<-xgb.train(params = params,nrounds=1000,data=dtrain,verbose=2,watchlist=watchlist)

pred_xgb<-predict(model_xgb,sparse.test)
table(test$Loan.Status,pred_xgb>=0.5)


(18202+2258)/nrow(test)#0.7670678

#--------------------------------------------------------------------------------------
#Logistic Regression Model implemenation & validation

modLog1 = glm(Loan.Status ~ .,data=train, family="binomial")
summary(modLog1)

modLog2 = glm(Loan.Status ~ Current.Loan.Amount+Term+Credit.Score +Years.in.current.job+Home.Ownership+Annual.Income+Purpose+Monthly.Debt+Years.of.Credit.History+Number.of.Open.Accounts+Current.Credit.Balance +Maximum.Open.Credit ,data=train, family="binomial")
summary(modLog2)

vif(modLog1)
vif(modLog2)
##baseline
table(test$Loan.Status)

19121/(19121+7552) #0.716

##Prediction

test$predicted.risk = predict(modLog2, newdata=test, type="response")
table(test$Loan.Status, as.numeric(test$predicted.risk >= 0.5))
#Overall Accuracy =

(793 + 18347)/nrow(test) #0.7175796


# Using ROCR package estimating performance measures and plotting these measures over a range of cutoffs
library(ROCR)
# Make predictions on training set
predictTrain = predict(modLog1, type="response")
# Prediction function
ROCRpred = prediction(predictTrain, train$Loan.Status)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)
# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.3,3.7))



#--------------------------------------------------------------------------------------
#Random Forest Model implemenation & validation

# Creating new train and test data for Random Forest Model
RFtrain=train
RFRFtest=test

#Converting the Loan.Status to Factor
RFtrain$Loan.Status = as.factor(RFtrain$Loan.Status)
RFtest$Loan.Status = as.factor(RFtest$Loan.Status)


library(randomForest)

CreditForest = randomForest(Loan.Status ~.,data= RFtrain, importance=TRUE,keep.forest=TRUE, ntree=2000, mtry=3)

summary(CreditForest)

PredictCreditForest=predict(CreditForest,newdata = RFtest)
table(RFtest$Loan.Status,PredictCreditForest)

(18056+2403)/nrow(RFtest) #0.7667679

# tuneRF() function searches for optimal mtry values for train dataset
tune.rf <- tuneRF(train1[,-16], train1[,16], stepFactor=1.5)
bestmtry <- tuneRF(train1[,-16], train1[,16], stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

#--------------------------------------------------------------------------------------
