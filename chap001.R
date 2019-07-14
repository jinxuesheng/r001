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

head(iris1)

iris1 <- mutate(
  iris1,
  dek=Sepal.Length/Sepal.Width,
  pek=Sepal.Length+Sepal.Width,
  jek=sqrt(dek)
)
head(iris1)

mydata <- data.frame(
  Name = sample(LETTERS[1:20]),
  old = runif(20,5,65) %>% round(0)
)

mydata <- within(mydata,{
  agecat <- NA
  agecat[old > 55] <- "Elder"
  agecat[old >= 35 & old <= 55] <- "Middle Aged"
  agecat[old < 35] <- "Young"
})


mydata$old_n <- ifelse(
  mydata$old >55, "Elder",
  ifelse(
    mydata$old >=35 & mydata$old <= 55,"Middle Aged", "Young"
  )
)

aggregate(Sepal.Length ~ Species, iris1, mean)

aggregate(Sepal.Length ~ Species, iris1, sum)

iris1 %>%
  group_by (Species) %>%
  summarize(
    means = mean(Sepal.Length),
    sums = sum(Sepal.Length)
  )

tapply(iris1$Sepal.Length,iris1$Species,mean)

library(plyr)

ddply(
  iris1,
  .(Species),
  summarize,
  means = mean(Sepal.Length),
  sums = sum(Sepal.Length)
)


###1.4 数据合并、联结与长度转换

data1 <- data.frame(
  name1 = c("A","B","C","D"),
  value = c(34,65,43,76)
)

data2 <- data.frame(
  name1 = c("E","F","G","H"),
  value = c(34,21,78,65)
)

#数据纵向合并
rbind(data1,data2)
dplyr::bind_rows(data1,data2)


#数据横向合并
cbind(data1,data2)
dplyr::bind_cols(data1,data2)

#1.4.2 数据联结
tableA <- data.frame(
  id = c(1,2,4),
  name = c("t1","t2","t4")
)


tableB <- data.frame(
  id = c(1,2,3),
  age = c(18,20,19)
)

merge(tableA,tableB,by = "id", all = FALSE) #内连接
merge(tableA,tableB,by = "id", all = TRUE) #外连接
merge(tableA,tableB,by = "id", all.x = TRUE) #左连接
merge(tableA,tableB,by = "id", all.y = TRUE) #右连接


library("dplyr")

inner_join(tableA,tableB,by = "id")#内连接
left_join(tableA,tableB,by = "id")#左连接
right_join(tableA,tableB,by = "id")#右连接
full_join(tableA,tableB,by = "id")#全连接

tableC <- data.frame(
  id = c(1,2,4,5),
  name = c("t1","t2","t4","t5")
)

tableD <- data.frame(
  name = c("t1","t2","t4","t4"),
  age = c(23,54,32,12)
)

semi_join(tableC,tableD,by = "name")
anti_join(tableC,tableD,by = "name")

#向量的交并补集运算

intersect(1:10,6:15) #交集
union(1:10,6:15)     #并集
setdiff(1:10,6:15)   #差集

#1.4.2 数据长宽转换

library("reshape2")
library("tidyr")

mydata <- data.frame(
  Name = c("苹果","谷歌","脸书","亚马逊","腾讯"),
  Company = c("Apple","Google","Facebook","Amazon","Tencent"),
  Sales2013 = c(5000,3500,2300,2100,3100),
  Sales2014 = c(5050,3800,2900,2100,3300),
  Sales2015 = c(5050,3800,2900,2100,3300),
  Sales2016 = c(5050,3800,2900,2100,3300)
);mydata

#宽转长，溶解
mydata1 <- melt(
  mydata,                           #数据集名称
  id.vars = c("Company","Name"),    #保留的主字段
  variable.name = "Year",           #转换后的分类字段名称（维度）
  value.name = "Sales"              #转换后度量值名称
);mydata1

#长转宽
dcast(
  data = mydata1,                           #数据集名称
  Name+Company ~ Year                       #x1+x2+...~class
  #这行是转换表达式，
  #左侧列出需要保留的主字段（不被拓宽）
  #右侧是要分割的分类变量，扩展之后的宽数据会增加若干列度量值
  #列数等于表达式右侧分类变量类别个数
);mydata1



#宽转长，gather
mydata1 <- tidyr::gather(
  data = mydata,                           #数据集名称
  key = "Year",                            #转换后的分类字段名称（维度）
  value = "Sales",                         #度量值名称
  Sales2013:Sales2016                      #选择将要被拉长的字段组合
);mydata1                                  #用x:y格式选择连续列、或-z格式排除主字段

#长转宽，spread
tidyr::spread(
  data = mydata1,                           #数据框名称
  key = Year,                               #待扩展的类别变量（编程新增列名称）
  value = Sales                            #待扩展的度量值（编程新增列度量值）
)


