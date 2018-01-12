data=read.csv("statedata.csv",stringsAsFactors = FALSE,header = TRUE,as.is = TRUE)
data
str(data)
summary(data)

#dividing the continuous and categorical variables
data_ctn<-subset(data, select = c(Population,Income,Illiteracy,Life.Exp,Murder,HS.Grad,Frost,Area,state.area,x,y))
data_cat<-subset(data, select = -c(Population,Income,Illiteracy,Life.Exp,Murder,HS.Grad,Frost,Area,state.area,x,y))

#pastecs analyses only continuous variables
library(pastecs)
options(scipen=100)#scipen takes care of the scientific data other than numerical data
options(digits=2)
stat.desc(data_ctn)

#Categorical Variables
as.matrix(sort(table(data_cat$state.division), decreasing = TRUE))
as.matrix(sort(table(data_cat$state.name), decreasing = TRUE))
as.matrix(sort(table(data_cat$state.region), decreasing = TRUE))


#Data Visualisation
plot(data$x,data$y, xlab = "longtitude of centres", ylab = "latitude of centres", main = "PLOT OF STATE CENTRES")

# About Highest Median High School Graduation Rate

boxplot(data$HS.Grad ~ data$state.region, xlab = "Region of USA", ylab = "High School Graduation Rate", main = "Distribution of High School Graduation Rate")



#########################################################################################
##############          Building the Predictive Model       #############################
#########################################################################################



#Inital Model- 1st Iteration 
#Converting Character into Factor Variables
data$state.region<-as.factor(data$state.region)
data$state.division<-as.factor(data$state.division)
data$state.name<-as.factor(data$state.name)



Model1<-lm(Life.Exp~ Population + Income + Illiteracy + Murder + HS.Grad + 
             Frost + Area + x + y + state.division +state.region,
           data = data)

#Printing the results of the Model
summary(Model1)


Model2<-lm(Life.Exp~ Population + Income + Illiteracy + Murder + HS.Grad + 
             Frost + Area + x + y +state.region,
           data = data)
summary(Model2)


Model3<-lm(Life.Exp~ Population + Income + Illiteracy + Murder + HS.Grad + 
             Area + x + y +state.region,
           data = data)
summary(Model3)


Model4<-lm(Life.Exp~ Population + Income +  Murder + HS.Grad + 
             Area + x + y +state.region,
           data = data)
summary(Model4)


Model5<-lm(Life.Exp~ Population + Income +  Murder + HS.Grad + 
             Area + x + y ,
           data = data)
summary(Model5)

Model6<-lm(Life.Exp~ Population + Income +  Murder + 
             Area + x + y ,
           data = data)
summary(Model6)

Model7<-lm(Life.Exp~ Population + Income +  Murder + 
              x + y ,
           data = data)
summary(Model7)

Model8<-lm(Life.Exp~ Population +  Murder + 
             x + y ,
           data = data)
summary(Model8)

Model9<-lm(Life.Exp~ Population +  Murder + 
             Frost+HS.Grad ,
           data = data)
summary(Model9)

Model10<-lm(Life.Exp~ Population +  Murder + 
             HS.Grad ,
           data = data)
summary(Model10)

library(car)
as.data.frame(vif(Model9))#ideally vif is <=1.7
as.data.frame(vif(Model8))


Pred_Life<-predict(Model9)
data$Pred_Life<-Pred_Life
data
data[,c(4,16)]
data[,c("Life.Exp","Pred_Life")]


write.csv(data, "statedata_pred.csv", row.names = FALSE)
plot(Model9)
