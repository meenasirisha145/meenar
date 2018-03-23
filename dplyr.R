library(dplyr)
library(datasets)
library(nycflights13)
data("flights")
data=flights
data
str(data)
print(dplyr::tbl_df(data),n=30)
tbl_df(data) %>% print(n = Inf)

iris
dplyr::summarise_each(iris,funs(mean))#apply summary function to each column

dplyr::count(iris,Species,wt=Sepal.Length)
dplyr::transmute(iris,Sepal = Sepal.Length + Sepal.Width)
