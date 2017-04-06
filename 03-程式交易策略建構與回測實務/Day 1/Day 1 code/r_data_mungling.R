##############################################################
# dplyr, reshape2 資料操作範例程式碼
# 僅限 證基會課程學員教學使用 
# 
# 2016/09/10 
#
# Julian Lin 

tips <-read.csv("http://www.ggobi.org/book/data/tips.csv")
n <-15 #sample size
rows <-nrow(tips)
indx <-sample(seq(rows),n)
tips[indx,]


myColumns <-c("tip", "day", "size")
tips[indx, myColumns]

subset(tips, size > 5, select = myColumns)

tips[which(tips$size > 5),myColumns]



install.packages("reshape2")
library(reshape2)

names(airquality) <- tolower(names(airquality))
head(airquality)


aql <- melt(airquality) # [a]ir [q]uality [l]ong format
head(aql)
tail(aql)


aql <- melt(airquality, id.vars= c("month", "day"))
head(aql)


aql <- melt(airquality, id.vars= c("month", "day"),
          variable.name = "氣候變數",
          value.name = "氣候數值")
head(aql)


aql <- melt(airquality, id.vars= c("month", "day"))
aqw <- dcast(aql, month + day ~ variable)
head(aqw)

head(airquality)



install.packages("dplyr")
library(dplyr)

tips[tips$size==3 & tips$tip> 4, ]

filter(tips, size==3, tip > 4)

df <- head(tips, n=3)
select(df,tip,sex,size)

select(df,sex:size)

df <- arrange(tips, total_bill)
head(df, n=5)

df <- arrange(tips, total_bill, desc(tip))
head(df, n=5)


df <- mutate(tips, tiprate= tip/total_bill)
head(df)


tips$tiprate <- with(tips, tip/total_bill)
head(tips)

tips %>%
  filter(size > 5) %>%
  select(total_bill, tip, sex, day, size) %>%
  arrange(tip) %>%
  mutate(tiprate= tip/total_bill)



tips %>%
  filter(size > 3) %>%
  select(total_bill, tip, sex, day, size) %>%
  arrange(tip) %>%
  mutate(tiprate=tip/total_bill) %>%
  select(total_bill, tip, sex, day, tiprate) %>%
  group_by(sex, day) %>%
  summarise_each(funs(mean)) %>%
  filter(tiprate> 0.12)
