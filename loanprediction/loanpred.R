######----Loan prediction----######
data<-read.csv("train.csv", header = TRUE,na.strings = "")
data
str(data)
summary(data)
colSums(is.na(data))

data$LoanAmount[is.na(data$LoanAmount)]=round(mean(data$LoanAmount,na.rm = T))
View(data)

data$Loan_Amount_Term=as.factor(data$Loan_Amount_Term)
str(data)

data$Credit_History=as.factor(data$Credit_History)
str(data)
table(data$Loan_Amount_Term)

data$Loan_Amount_Term[is.na(data$Loan_Amount_Term)]=360
table(data$Credit_History)

data$Credit_History[is.na(data$Credit_History)]=1
colSums(is.na(data))

table(data$Gender)
data$Gender[is.na(data$Gender)]="Male"

table(data$Dependents)
data$Dependents[is.na(data$Dependents)]=0

table(data$Self_Employed)
data$Self_Employed[is.na(data$Self_Employed)]="No"

colSums(is.na(data))
table(data$Married)
data$Married[is.na(data$Married)]="Yes"
str(data)

train=subset(data,select=-c(1))
str(train)
#Inital Model- 1st Iteration 
Model1<-glm(Loan_Status~.,family = binomial(link = 'logit'), data = train)
summary(Model1)
names(train)

Model2=glm(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+
             ApplicantIncome+CoapplicantIncome+LoanAmount+Credit_History+Property_Area,
           family = binomial(link = 'logit'), data = train)
summary(Model2)


Model3=glm(Loan_Status~Gender+Married+Dependents+Education+
             ApplicantIncome+CoapplicantIncome+LoanAmount+Credit_History+Property_Area,
           family = binomial(link = 'logit'), data = train)
summary(Model3)


Model4=glm(Loan_Status~Married+Dependents+Education+
             ApplicantIncome+CoapplicantIncome+LoanAmount+Credit_History+Property_Area,
           family = binomial(link = 'logit'), data = train)
summary(Model4)

Model5=glm(Loan_Status~Married+Education+ ApplicantIncome+CoapplicantIncome
           +LoanAmount+Credit_History+Property_Area,
           family = binomial(link = 'logit'), data = train)
summary(Model5)


Model6=glm(Loan_Status~Married+Education+CoapplicantIncome
           +LoanAmount+Credit_History+Property_Area,
           family = binomial(link = 'logit'), data = train)
summary(Model6)


Model7=glm(Loan_Status~Married+Education+CoapplicantIncome
           +LoanAmount+Credit_History,
           family = binomial(link = 'logit'), data = train)
summary(Model7)

Model8=glm(Loan_Status~Married+Education+CoapplicantIncome
           +Credit_History, family = binomial(link = 'logit'), data = train)
summary(Model8)

Model9=glm(Loan_Status~Married+CoapplicantIncome
           +Credit_History, family = binomial(link = 'logit'), data = train)
summary(Model9)


test=read.csv("test.csv",header = TRUE,na.strings = "")

View(test)
colSums(is.na(test))
str(test)
test$Loan_Amount_Term=as.factor(test$Loan_Amount_Term)
test$Credit_History=as.factor(test$Credit_History)
str(test)

library(Amelia)
missmap(test, main = "Missing values vs Observed")

table(test$Gender)
test$Gender[is.na(test$Gender)]="Male"

table(test$Dependents)
test$Dependents[is.na(test$Dependents)]=0

table(test$Self_Employed)
test$Self_Employed[is.na(test$Self_Employed)]="No"

table(test$Loan_Amount_Term)
test$Loan_Amount_Term[is.na(test$Loan_Amount_Term)]=360


table(test$Credit_History)
test$Credit_History[is.na(test$Credit_History)]=1

test$LoanAmount[is.na(test$LoanAmount)]=round(mean(test$LoanAmount,na.rm=TRUE))
colSums(is.na(test))

#Treating the missing values using the MICE Package
#install.packages("MICE", dependencies = TRUE)
#library(mice)
#set.seed(144)
#vars.for.imputation = setdiff(names(test), "Loan_Status")
#imputed = complete(mice(test[vars.for.imputation]))
#test[vars.for.imputation] = imputed
#as.data.frame(colSums(is.na(test)))

testdata=subset(test,select=-c(1))
str(testdata)

#Assessing the predictive power of the model
#Predicted Probabilities
pred_loan=predict(Model9,newdata=testdata,type="response")
pred_loan_n=ifelse(pred_loan>0.5,1,0)

x=test$Loan_ID
x
sub=read.csv("Sample_Submission.csv",header = TRUE)
str(sub)
sub$Loan_Status=pred_loan_n
head(sub)
write.csv(sub,"submission.csv",row.names = FALSE)
