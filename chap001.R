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

#1.5.1 字符串格式化

#paste

myword <- sample(LETTERS,10,replace = FALSE);myword

paste(myword,collapse="-")
paste0(myword,collapse="-")

url <- "http://study.163.com/category/400000000146050#/?p="

num <- 1:20

myurl <-paste(url,num,sep="");myurl #遍历网页时，必须sep=""
myurl <-paste(url,num);myurl #遍历网页时，默认加空格不是我们要的

myurl <-paste0(url,num);myurl       #默认等效sep=""

library("stringr")
str_c(url,num,sep="") #用法与原生的paste相同

#sprinf()

#%d 整数    %02d   d代表正数；2代表长度；0代表不足补0
#%f 浮点数  %4.2f  4代表总位数；2代表小数位数
#%s 字符串
#%% 百分比

sprintf("%d%%",1:10)  #遍历百分比
sprintf("%d-%d-%2d",2001,12,1:30)  #遍历日期
sprintf("有%.1f%%的人评价变形金刚5较差",30.77)
sprintf("%s是阿里巴巴的%s","马云","老板")

#字符串高阶——如同Python一样控制字符串
#包项目主页  https://github.com/Ironholds/pystr
#可能需要 install.packages("devtools")
devtools::install_github("nicolewhite/pystr")

library("pystr")

#顺序参数
sprintf("Hello %s, my name is %s.","world","JXS")
pystr_format("Hello {1}, my name is {2}.","world","JXS")
pystr_format("Hello {2}, my name is {1}.","world","JXS")

#1.5.2 字符串合并拆分

myword <- c("fff-888","hh-333","ff-666","ccc-666")
result <- strsplit(myword,"-")

#初级方法
mydata <- data.frame(
  word = myword,
  first = NA,
  second = NA
)

for (i in 1:length(myword)){
  mydata$first[i] <- result[i][1]
  mydata$second[i] <- result[i][2]
};mydata

#高级方法
#可能需要  install.packages("rlist")
do.call(rbind,result) %>% cbind(myword,.) %>% data.frame %>% dplyr::rename(name = "V2",value ="V3")
rlist::list.rbind(result) %>% cbind(myword,.) %>% data.frame %>% dplyr::rename(name = "V2",value ="V3")

#在数据框中批量实现合并拆分字符串

myyear <- sprintf("20%02d",sample(0:17,10))
mymonth <- sprintf("%02d",sample(1:12,10))
myday <- sprintf("%02d",sample(1:28,10))
mydata <- data.frame(myyear,mymonth,myday)

#合并
library("tidyr")
mydata1 <- unite(
  mydata,
  col="datetime",
  c("myyear","mymonth","myday"),
  sep="-",
  remove=FALSE
);mydata1

separate(
  data = mydata1,
  col="datetime",
  into= c("year1","month1","day1"),
  remove=FALSE
);mydata1


#1.5.3 字符串替换、抽取与处理工具——正则表达式

length() #字符串长度
grep/grepl() #字符串筛选
sub/gsub() #字符串替换
regexpr/gregexpr() #返回字符串起始位置
substr/substring() #字符串截取
stringr::str_extract() #返回匹配值


#1.6 管道函数与向量化函数

#不使用管道函数的代码
summarize(
  group_by(iris,
           Species),
  means = mean(Sepal.Length),
  sums = sum(Sepal.Length)
)

#使用管道函数优化后

iris %>%
  group_by(Species) %>%
  summarize(
    means = mean(Sepal.Length),
    sums = sum(Sepal.Length)
  )

#管道函数传参的一般规则

myword <- c("fff-888","hh-333","ff-666","ccc-666")
result <- strsplit(myword,"-")
do.call(rbind,result) %>% cbind(myword, .) %>% data.frame %>% dplyr::rename(name = "V2",value ="V3")

#在数据抓取中管道函数更加神奇
#install.packages("rvest")
library("rvest")
url <- "https://kongzhong.tmall.com/p/rd828551.htm"
Name <- read_html(url, encoding = "GBK") %>%
  html_nodes("b") %>%
  html_text(trim = FALSE) %>%
  gsub("(\\n\t|. |\\d|、)","", .) %>%
  grep("\\s", .,value = T) %>%
  str_trim(side = "both") %>%
  .[1:54] %>%
  .[setdiff(1:54,c(35,39))]

Name

#向量化函数

#apply  按照维度计算

matrix(runif(30,100,1000),nrow = 5) %>% apply(1,mean)  #按行处理
matrix(runif(30,100,1000),nrow = 5) %>% apply(2,mean)  #按列处理

#tapply

tapply(iris$Sepal.Length,iris$Species,mean)
class(tapply(iris$Sepal.Length,iris$Species,mean))

#plyr::ddply
library("plyr")
plyr::ddply(
  iris,
  .(Species),    #分组
  summarize,                 #summarize()函数参数可以在means、sums里逐个传给他
  means=mean(Sepal.Length),
  sums=sum(Sepal.Length)
)

#sapply/lapply

sapply(1:3,function(x)x^2)
lapply(1:3,function(x)x^2)
lapply(1:3,function(x)x^2) %>% unlist

#l_ply   仅仅执行任务，放弃输出结果的函数

fun <- function(i) cat(sprintf("你好，%d号！",i),sep = "\n")
l_ply(1:3,fun)
(l_ply(1:3,fun)) #最外面括号的意思是，把里面代码的输出结果放出来，这里是NULL



#1.7 非结构数据处理 之 list/json

library("jsonlite")
library("magrittr")

#jsonlite

#fromJSON()

info_json <- '{"p1":{"name":["Ken"],"age":[24],"interest":["reading","music","movies"],"lang":{"r":[2],"csharp":[4],"python":[3]}},"p2":{"name":["James"],"age":[25],"interest":["sports","music"],"lang":{"r":[3],"java":[2],"cpp":[5]}},"p3":{"name":["Penny"],"age":[24],"interest":["movies","reading"],"lang":{"r":[1],"cpp":[4],"python":[2]}}}'

myjson_info <- info_json %>% fromJSON()
cat(info_json)


#toJSON()

mylist <- list(
  object1 = c("A","B","C","D","E"),
  object2 = matrix(1:20,nrow = 4, byrow = FALSE),
  object3 = array(1:27,dim =  c(3,3,3)),
  object4 = data.frame(
    name1 = c("A","B","C","D","E"),
    name2 = c(21,34,56,54,32),
    name3 = c(TRUE,FALSE,TRUE,FALSE,FALSE)
  )
)

toJSON(mylist, auto_unbox = FALSE) %>% cat()

#rlist
library("rlist")
#setwd("c:\...")
#getwd()

#list.load
mylist <- list.load("mylist.json")

list.save(mylist,"mylistnew.json")


#1.8 MYsql数据库连接
#install.packages("RMySQL")
library("RMySQL")
library("dplyr")

devtools::install_github("mongosoup/rmongodb")
library("rmongodb")



#1.9 高阶数据处理工具之 data.table

library("data.table")



