v1 <- 1:100
v1[1:10]
v1[c(4,6,8,9)]

v1[sample(x = c(TRUE,FALSE), size = 100, replace = TRUE)]
v1[v1%%2 == 0]

mydata1 <- ggplot2::mpg
head(mydata1)
str(mydata1)

mydata1[,c(2,3)]
mydata1[,c("model","displ")]

mydata1[1:5,]
mydata1[c(1,4,6,8,9)]
mydata1[c(1,4,6,8,9),c("model","displ")]

mydata1[mydata1$model == "audi" | mydata1$manufacturer == "mercury",]
mydata1[mydata1$model == "a4" & mydata1$manufacturer == "audi",]

subset(mydata1,model == "audi" | manufacturer == "mercury" , select = c("model","manufacturer","year"))
subset(mydata1,model == "a4" & manufacturer == "audi" , select = c("model","manufacturer","year"))

library(dplyr)
mydata1%>% filter(model == "audi"|manufacturer == "mercury") %>% select(model,manufacturer,year)
mydata1%>% filter(model == "a4"&manufacturer == "audi") %>% select(model,manufacturer,year)

iris1 <- iris
head(iris1)

iris1 <- transform(
  iris1,
  dek=Sepal.Length/Sepal.Width,
  pek=Sepal.Length+Sepal.Width
)
iris1 <- transform(
  iris1,
x=dek * pek
)
head(iris1)


