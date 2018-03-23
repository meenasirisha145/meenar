load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)


train <- fread('train.csv',colClasses=c('MiscFeature' = "character", 'PoolQC' = 'character', 'Alley' = 'character'))
test <- fread('test.csv' ,colClasses=c('MiscFeature' = "character", 'PoolQC' = 'character', 'Alley' = 'character'))


cat_var <- names(train)[which(sapply(train, is.character))]
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(train)[which(sapply(train, is.numeric))]



## Structure of the data
##```{r structure}
dim(train)
str(train)


# Summarize the missing values in the data.
head(train)
colSums(sapply(train, is.na))
colSums(sapply(train[,.SD, .SDcols = cat_var], is.na))
colSums(sapply(train[,.SD, .SDcols = numeric_var], is.na))

library(Amelia)
missmap(train, main = "Missing values vs Observed")

#number of houses that are remodeled
sum(train[,'YearRemodAdd', with = FALSE] != train[,'YearBuilt', with = FALSE])

cat('Percentage of houses remodeled',sum(train[,'YearRemodAdd', with = FALSE] != train[,'YearBuilt', with = FALSE])/ dim(train)[1])

train %>% select(YearBuilt, YearRemodAdd) %>%    mutate(Remodeled = as.integer(YearBuilt != 
YearRemodAdd)) %>% ggplot(aes(x= factor(x = Remodeled, labels = c( 'No','Yes')))) + 
geom_bar() + xlab('Remodeled') + theme_light()



## Summarize the numeric values and the structure of the data.

summary(train[,.SD, .SDcols =numeric_var])

cat('Train has', dim(train)[1], 'rows and', dim(train)[2], 'columns.')
cat('Test has', dim(test)[1], 'rows and', dim(test)[2], ' columns.')


# The percentage of data missing in train.
sum(is.na(train)) / (nrow(train) *ncol(train))

# The percentage of data missing in test.
sum(is.na(test)) / (nrow(test) * ncol(test))

# Check for duplicated rows.

cat("The number of duplicated rows are", nrow(train) - nrow(unique(train)))

####Convert character to factors 

train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]



train_cat <- train[,.SD, .SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = numeric_var]

plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

##categorical Variables

doPlots(train_cat, fun = plotHist, ii = 1:4, ncol = 2)
doPlots(train_cat, fun = plotHist, ii  = 4:8, ncol = 2)
doPlots(train_cat, fun = plotHist, ii = 8:12, ncol = 2)
doPlots(train_cat, fun = plotHist, ii = 13:18, ncol = 2)
doPlots(train_cat, fun = plotHist, ii = 18:22, ncol = 2)


train %>% select(LandSlope, Neighborhood, SalePrice) %>% filter(LandSlope == 
c('Sev', 'Mod')) %>% arrange(Neighborhood) %>% group_by(Neighborhood, LandSlope)
%>% summarize(Count = n()) %>% ggplot(aes(Neighborhood, Count)) + geom_bar(aes(fill = 
LandSlope), position = 'dodge', stat = 'identity') + theme_light() +theme(axis.text.x 
= element_text(angle = 90, hjust =1))


train %>% select(Neighborhood, SalePrice) %>% ggplot(aes(factor(Neighborhood), SalePrice)) 
+ geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  xlab('Neighborhoods')

doPlots(train_cont, fun = plotDen, ii = 2:6, ncol = 2)
doPlots(train_cont, fun = plotDen, ii = 7:12, ncol = 2)
doPlots(train_cont, fun = plotDen, ii = 13:17, ncol = 2)


doPlots(train_cont, fun = plotHist, ii = 18:23, ncol = 2)



## Explore the correlation

correlations <- cor(na.omit(train_cont[,-1, with = FALSE]))

# correlations
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)

correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")

train %>% select(OverallCond, YearBuilt) %>% ggplot(aes(factor(OverallCond),YearBuilt)) +
  geom_boxplot() + xlab('Overall Condition')

plotCorr <- function(data_in, i){
  data <- data.frame(x = data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data, aes(x = x, y = SalePrice)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$SalePrice, use = 'complete.obs'), 2))) + theme_light()
  return(suppressWarnings(p))
}


highcorr <- c(names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] > 0.5)], names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] < -0.2)])

data_corr <- train[,highcorr]

doPlots(data_corr, fun = plotCorr, ii = 1:6)

ggplot(train, aes(x=SalePrice)) + geom_histogram(col = 'white') + theme_light()

summary(train[,.(SalePrice)])
#Normalize distribution
ggplot(train, aes(x=log(SalePrice+1))) + geom_histogram(col = 'white') + theme_light()

hist(train$SalePrice)
train$SalePrice=log(train$SalePrice+1)
hist(train$SalePrice)

hist(train$GrLivArea)
train$GrLivArea=log(train$GrLivArea+1)
hist(train$GrLivArea)

hist(train$TotalBsmtSF)
train$TotalBsmtSF=10^train$TotalBsmtSF
hist(train$TotalBsmtSF)

model=lm(SalePrice~OverallQual+GrLivArea+YearBuilt+TotalBsmtSF+GarageCars,data=train)
summary(model)



model1=lm(SalePrice~OverallQual+GrLivArea+YearBuilt+GarageCars,data=train)
summary(model1)

plot(model1)
test
str(test)
test$OverallQual=as.factor(test$OverallQual)
test$YearBuilt=as.factor(test$YearBuilt)
pred=predict(model1,test)
pred
sub=read.csv("sample_submission.csv",header=T)
head(sub)
nrow(sub)
sub$SalePrice=pred
head(sub)
