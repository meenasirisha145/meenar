data=mtcars
library(stargazer)
stargazer(data,type="text",title = "descriptive statistics",out="table.txt")
stargazer(data,type="text",digits = 1)
data$fast <- as.numeric((data$mpg > 20.1)) #Creating a dummy variable 1 = fast car
m1 <- lm(mpg ~ hp, data=data)
summary(m1)

m2 <- lm(mpg ~ hp + drat, data=data)
summary(m2)
m3 <- lm(mpg ~ hp + drat + factor(gear), data=data)

data
m4 <- glm(fast ~ hp + drat + am, family=binomial(link="logit"), data=data)
summary(m4)
stargazer(m1, m2, m3, m4, type="text",
          dep.var.labels=c("Miles/(US) gallon","Fast car (=1)"),
          covariate.labels=c("Gross horsepower","Rear axle ratio","Four foward gears",
                             "Five forward gears","Type of transmission (manual=1)"), out="models.txt")


stargazer(m1, m2, m3, m4, type="text",
          dep.var.labels=c("Miles/(US) gallon","Fast car (=1)"), out="models1.txt")
