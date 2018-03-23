##############House Prices###############

load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

train=read.csv("train.csv",header=T)
str(train)
cat_var <- names(train)[which(sapply(train, is.character))]
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(train)[which(sapply(train, is.numeric))]

summary(train$SalePrice)



lines(hist(train$SalePrice))
skewness(train$SalePrice)
kurtosis(train$SalePrice)
#scatter plot grlivarea/saleprice
plot(train$GrLivArea,train$SalePrice)
plot(train$TotalBsmtSF,train$SalePrice)

#Relationship with categorical features
train$OverallQual=as.factor(train$OverallQual)
levels(train$OverallQual)
train$YearBuilt=as.factor(train$YearBuilt)

boxplot(SalePrice~OverallQual,data=train)
boxplot(SalePrice~YearBuilt,data=train)


library(Amelia)
missmap(train, main = "Missing values vs Observed")
library(data.table)

sum(train[,'YearRemodAdd', with = FALSE] != train[,'YearBuilt', with = FALSE])


####Convert character to factors 
train_cont$OverallQual=as.numeric(train_cont$OverallQual)
train_cont$YearBuilt=as.numeric(train_cont$YearBuilt)

train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]
str(train)




train_cat <- train[,.SD, .SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = numeric_var]
str(train_cont)



library(corrplot)
correlations <- cor(na.omit(train_cont[,-1, with = FALSE]))


# correlations
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)

correlations<- correlations[row_indic ,row_indic]
corrplot(correlations, method="square")


highcorr <- c(names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] > 0.5)], names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] < -0.2)])

data_corr <- train[,highcorr, with = FALSE]

install.packages("devtools")
library(devtools)
install_github("theasjblog/tcx_package")


class(data_corr)

library(GGally)
ggpairs(data_corr)

data.frame(colSums(is.na(train)))
x=as.data.frame(colMeans(is.na(train)))
x$total=colSums(is.na(train))
x['total']>1
y=rownames(x)[x['total']>1]
class(y)
z=as.vector(y)
z
class(z)
library(caTools)
var=c("LotFrontage","Alley","MasVnrType","MasVnrArea","BsmtQual","BsmtCond","BsmtExposure",
"BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageYrBlt","GarageFinish","GarageQual","GarageCond","PoolQC"
,"Fence","MiscFeature")
var

traindrop=train[ , !(names(train) %in% var)]
str(traindrop)
table(traindrop$Electrical)


boxplot(SalePrice~Electrical,data=traindrop)
traindrop$Electrical[is.na(traindrop$Electrical)]="FuseF"
as.data.frame(colSums(is.na(traindrop)))


model=lm(SalePrice~OverallQual+GrLivArea+YearBuilt+TotalBsmtSF+GarageCars,data=traindrop)
summary(model)



model1=lm(SalePrice~OverallQual+GrLivArea+YearBuilt+GarageCars,data=train)
summary(model1)

plot(model1)

pred=predict(model1,test)
pred
sub=read.csv("sample_submission.csv",header=T)
head(sub)
nrow(sub)
sub$SalePrice=pred
head(sub)
