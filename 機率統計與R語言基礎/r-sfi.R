

pnorm(1.96)  # p = 0.975, z值推估機率值
qnorm(0.975) # z = 1.96, 機率值推估z值

# 範例:負投資報酬率的機率

# a. P(X < 0)=P(Z < -2) --> 查表 p=0.0228
pnorm(-2) # p=0.02275013, 約等於0.0228

# b. P(Z <- 1) --> 查表 p=0.0228
pnorm(-1) # p=0.1586553, 約等於0.1587

# 平均數之區間估計 範例
# t.alpha/2(n-1) = t.0.025(9-1)=t.0.025(8)=2.306
?pt
qt(p=0.025, df=8) # -2.306004
# -2.306004 * (-1) = 約等於2.306

# 迴歸分析:線性(linear)與非線性(nonlinear)兩種圖形
x <- 1:100
y <- 1 + x^0.15 + rnorm(100, 0, 0.01)
m <- nls(y ~ a + b * I(x^z), start = list(a = 1, b = 1, z = 1))
m
op <- par(mfrow=c(1,2))

plot(y~x, main="線性迴歸")
abline(lm(y~x), col="red", lwd=2)

plot(y ~ x, main="非線性迴歸")
lines(x, fitted(m), lty = 2, col = "red", lwd = 2)
par(op)

# @@@Day 2. 1-2 R軟體漫談與平台特徵 -----
# R的深度介紹與語法結構
# RStudio介紹與使用
# 讀取外部資料：read.table/read.csv/readLines 
# 資料匯出：write.csv/write.table
# 撰寫第一個R程式：輸入資料與定義函數

# R的深度介紹與語法結構 -----

# 官網: http://www.r-project.org/

plot(runif(100), type="l")
demo(graphics)
demo(persp)

pairs(iris[-5])
pairs(iris[-5], col=iris$Species)

# Ubuntu 安裝R
# http://rwepa.blogspot.tw/2013/05/ubuntu-r.html

# Ubuntu 16.04 LTS + SQL Server
# https://docs.microsoft.com/en-us/sql/linux/sql-server-linux-setup-ubuntu

# RStudio介紹與使用 -----

# 完成設定
# 關閉 RStudio
# 重新開啟 RStudio

# RStudio 使用環境 -----
weight <- seq(50, 70, length=10) + rnorm(10,5,1)
height <- seq(150, 170, length=10) + rnorm(10,6,2)
test <- data.frame(weight, height) # type [tab]
test.lm <- lm(weight ~ height, data=test)
summary(test.lm)
op <- par(mfrow=c(2,2)) # 設定2列,2行
plot(test.lm)
par(op) # 還原成1列,1行

height

# 輔助說明 -----
help.start()
?plot
help(plot)
# 選取 plot 按 F1
help.search("regression")
??regression

# 套件 packages -----
library(quantmod)
getSymbols("^TWII", from="2000-1-1")
chartSeries(TWII)


install.packages("e1071")
library(e1071)
example(svm, package="e1071")

# 已載入套件
search()

# R套件-34類別
# http://rwepa.blogspot.tw/2013/10/packages-list-32.html

# 已下載安裝套件
x <- installed.packages()
x[, 1] # [列, 行]

library() # same as install.packages()

# R 套件選單
update.packages("xxx") # 套件更新
detach("package:xxx")  # 套件卸離
remove.packages("xxx") # 套件移除

# RStudio練習-quantmod 套件 -----
library(quantmod)
getSymbols("^TWII", from="2000-1-1")
chartSeries(TWII)

# RStudio練習-ggmap 套件 -----
library(ggmap)
map.taiwan <- get_map(location="Taiwan", zoom=8)
ggmap(map.taiwan)

# RStudio練習-googleVis 套件 -----
install.packages("googleVis")
# 練習 R code

# 函數 function -----
# refer to materials

# 讀取外部資料與資料匯出 -----
# read.table, read.csv, readLines; write.csv, write.table

# 步驟 1. 設定工作目錄
# 預設工作目錄
getwd() # get working directory
workpath <- "C:/rdata"
workpath <- "C:\\rdata"  # for windows
setwd(workpath)

# 已更改為 C:\rdata 工作目錄
getwd()

# 步驟 2. 建立資料檔
# 參考講義建立檔案

# 步驟 3. 匯入資料 read.table
# ?read.table

production2015 <- read.table("production2015.csv", header=TRUE, sep=",")
# header: 標題名稱, sep: 區隔符號
production2015

# 步驟 4.資料處理
# 資料檢視
str(production2015)
dim(production2015) # 10列5行
names(production2015) # 欄位名稱
production2015$生產量
production2015$目標量

# attach(production2015)
# 生產量
# 目標量

# 新增欄位
round((production2015$生產量/production2015$目標量)*100,0)
production2015$達成率 <- round((production2015$生產量/
                               production2015$目標量)*100,0)
production2015

# 繪製直線圖
plot(production2015$生產量, type="b")
lines(production2015$目標量, col="red")
points(production2015$目標量, col="red")

# 步驟 5. 匯出資料
# 匯出資料
write.table(production2015, 
            "production2015_revised.csv", 
            sep=",", 
            row.names=FALSE)
# CTRL + L
# end

# 撰寫第一個R程式:輸入資料與定義函數 -----

# 資料來源: https://www.taifex.com.tw/chinese/3/3_5.asp
# http://web.ydu.edu.tw/~alan9956/rdata/exchange-2016.02.13-2017.02.13.csv

urls <- "http://web.ydu.edu.tw/~alan9956/rdata/exchange-2016.02.13-2017.02.13.csv"

exchange <- read.table(urls, header=TRUE, sep=",")
exchange
str(exchange) # 部分欄位轉換成factor

exchange <- read.table(urls, header=TRUE, sep=",", stringsAsFactors=FALSE)
exchange
str(exchange) # 部分欄位保留成字串

exchange$日期 <- as.Date(exchange$日期)
exchange$紐幣.美元 <- as.numeric(exchange$紐幣.美元)

summary(exchange)

# 方法1
plot(exchange$美元.新台幣, type="l")

# 方法2
plot(exchange$美元.新台幣, type="l", 
     axes=FALSE, 
     xlab="日期", 
     ylab="美元vs.新台幣", 
     main="美元vs.新台幣趨勢圖-2016.2.13~2017.2.13")

ind <- seq(1, nrow(exchange), round(nrow(exchange)/10))
axis(1, at=ind, labels=exchange$日期[ind])
axis(2)
grid()
box()

# @@@Day 3. R軟體漫談與平台特徵 -----
# 向量(vector)、因子(factor)、矩陣(matrix)與陣列(array)
# 資料框(data.frame)與串列(list)
# 字串和日期時間資料處理
# 時間序列與財務經濟分析
# 案例實作與討論(一)

# 資料型態 -----
# 整數
x1 <- c(1:10)
x1
typeof(x1)
is.integer(x1)
is.numeric(x1)
class(x1)

# 數值
x2 <- c(1.1, 1.3, 1.5, 1.7, 2)
x2
typeof(x2)
is.integer(x2)
is.numeric(x2)
class(x2)

# 字串資料
x3 <- c("台北市", "新北市", "台中市", "台南市", "高雄市")
x3
typeof(x3)
is.character(x3)
class(x3)

x4 <- c(1, 2.3, "巨量資料")
x4
class(x4)

# 邏輯值
TRUE*2
FALSE*3

x5 <- x2 >= 1.4
x5
typeof(x5)
is.integer(x5)
is.numeric(x5)
is.character(x5)
is.logical(x5)
class(x5)

# 資料物件 -----
# 資料物件名稱中,英文皆可,但不可用數字開頭.
# 參考名稱: customerSale, customerSale2015, customerSale_2015

A <- "中華R軟體學會CARS"; compar <- TRUE; z <- 1+2i
mode(A); mode(compar); mode(z)

# 向量 vector -----
# 整數
v0 <- c(1:10)
v0
class(v0)
typeof(v0)

# 實數
v1 <- c(.29, .30, .15, .89, .12)
v1
class(v1)
typeof(v1)
v1[1]
v1[2:4]
v1[1,3,5] # error
v1[c(1,3,5)]

# 字元
x2 <- c("Taiwan", "China", "USA")
x2
is.vector(x2)

# Expand the length of a vector
length(x2) <- 5
x2

# 數值+字元
# 強制(coercion)轉換成單一相同型態
v2 <- c(.29, .30, .15, .89, .12, "wepa")
v2
class(v2)
typeof(v2)
mean(v2)
# 如何取得數值資料之平均值

# 向量具有 mode, length 屬性 
x1 <- vector(mode="numeric", length=1000000)

# View x1
head(x1)

# Verify a vector
is.vector(x1)

taiwan1 <- c("正月初一", "正月初二", "正月初四", "正月初五", "正月初九", "正月十五")
names(taiwan1) <- c("春節", "回娘家", "接神日", "迎財神", "天公生", "元宵節")
taiwan1

# 因子 factor -----

eye.color <- c("brown", "blue", "blue", "green", "brown", "brown", "brown")

eye.colors <- factor(c("brown", "blue", "blue", "green", "brown", "brown", "brown"))
levels(eye.colors)

eye.color
eye.colors

class(eye.color)
class(eye.colors)

# 矩陣 matrix -----
matrix.data <- matrix(c(1,2,3,4,5,6), 
                      nrow = 2, ncol = 3, byrow=TRUE, 
                      dimnames = list(c("row1", "row2"), c("C1", "C2", "C3")))
matrix.data

# 陣列 array -----
a1 <- array(letters)
a1
class(a1)
dim(a1) #26

a2 <- array(1:3, c(2,4)) # 2 rows, 4 columns
a2
dim(a2) # 2, 4
length(a2)
a2[1, ] # select row 1
a2[, 4] # select column 4

# 具有三個維度陣列
a3 <- array(data=1:24, dim=c(3,4,2))
a3
class(a3)
str(a3)

# 資料框 data.frame -----
x <- c(1:4); n <- 10; m <- c(10, 35); y <- c(2:4)
df1 <- data.frame(x, n)
df1
df2 <-data.frame(x, m)
df2

# cars 資料集
data(cars)
cars
# help(cars)
class(cars)
head(cars)
head(cars, n=3)

cars[2]
cars["dist"]
cars[,2]

# 串列 list -----
list.test1 <- list(1,2,3,4,5)
list.test1
str(list.test1)

list.test1[5]   # [ ]   : key + values
list.test1[[5]] # [[ ]] : values

# 串列的每個元素可以付予名稱
product <- list(destination="Taipei",
                dimensions=c(3,6,10),
                price=712.19)
product[2]
product[[2]]
product$price

#  特殊數值 -----
x <- 5/0
x
exp(x)
exp(-x)
x - x
0/0

# NA
x.NA <- c(1, 2, 3)
x.NA
length(x.NA) <- 4
x.NA

# 建立 1月,2月,3月,...,12月 -----

# 方法1 土法煉鋼
tw.month <- c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月")
tw.month

# 方法2 採用 paste 函數
tw.months <- paste(c(1:12), "月", sep="")
tw.months

# tw.month 與 tw.months 是否完全相同?

# 矩陣運算 -----

# create a matrix
a <- matrix(c(2,3,-2,1,2,2), 3, 2)
a

# verify a matrix
is.matrix(a)
is.vector(a)

# multiplication by a scalar
c <- 3
c*a

# matrix addition and substracion
(b <- matrix(c(2,1,-2,1,2,1),3, 2))
a
m.add <- a+b
m.add
m.sub <- a-b
m.sub

# matrix multiplication
(d <- matrix(c(2,-2,1,2,3,1),2,3))
(m.product <- d %*% a)

# transpose matrix
a.trans <- t(a)
a.trans

# unit vector
m.unit <- matrix(1,3,1)
m.unit

# unit matrix
m.comm <- matrix(1,3,2)
m.comm

# diagonal
s <- matrix(c(2,3,-2,1,2,2,4,2,3),3,3)
s
m.diag <- diag(s)
m.diag
class(m.diag) # numeric
as.matrix(m.diag)

# 字串和日期時間資料處理 -----
# refer to materials

# 時間序列與財務經濟分析 -----

# 時間序列物件 ts -----
data.ts <- ts(c(2,5,4,6,3,7,9:8),start=c(2009,2),frequency=4)
data.ts
class(data.ts)
is.ts(data.ts)
start(data.ts)
end(data.ts)
frequency(data.ts)
deltat(data.ts) # 0.25(=1/4)
plot(data.ts, type="b")

# ARIMA (AutoRegressive Integrated Moving Average)model -----
fit <- arima(AirPassengers, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=24)
# error bounds at 95% confidence level
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(AirPassengers, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2), ylab="Passengers(1000)")
legend("topleft", col=c(1,2,4), lty=c(1,1,2),
       c("Actual", "Forecast",
         "Error Bounds (95% Confidence)"))

# 案例實作與討論(一) -----

library(zoo)

# 匯入資料
urls <- "http://web.ydu.edu.tw/~alan9956/rdata/tx_1min_2014.csv"
tx1min <- read.csv(urls, stringsAsFactors=FALSE)

# 資料結構(str)與摘要(summary)
head(tx1min)
str(tx1min) # data.frame 74217*7
summary(tx1min) # 檢查是否有NA

# 將日期,時間合併為一個 DateTime 欄位
tx1min$DateTime <- paste(tx1min$Date, tx1min$Time)
tx1min <- tx1min[, c(8, 3:7)]
head(tx1min)

# 轉換為 zoo 物件
tx1min <- read.zoo(tx1min, format="%Y/%m/%d %H:%M:%S", FUN=as.POSIXct)
str(tx1min) # zoo 74217*5
head(tx1min)

# 繪圖
plot(tx1min, main="2014年台股期貨", xlab="日期")

# 資料篩選
tx1min.2014q1 <- window(tx1min, start=as.POSIXct("2014-1-1"), end=as.POSIXct("2014-3-31"))
dim(tx1min.2014q1) # 16452*5
head(tx1min.2014q1, n=10)
tail(tx1min.2014q1, n=3)

# @@@Day 4. 1-3 R軟體的資料處理 -----
# 資料形狀轉換
# 遺漏值與差補技術
# apply家族
# loop and if 流程控制
# 樞紐分析一把抓

# 資料形狀轉換 -----

# 標準化 scale -----
x <- matrix(1:30, ncol = 3)
scale(x) # mean=0, sd=1

# 標準化-平均值MEAN, 標準差SD
x.new <- scale(x)*10 + 100
x.new

# R 練習-將 Cars93$Price 標準化至1~5之間 -----
# 標準化－最小值/最大值法
data(Cars93, package="MASS")
x <- Cars93[1:10,]
x$Price

(max.old <- max(x$Price))
(min.old <- min(x$Price))

price.new <- ((x$Price - min.old)/(max.old - min.old))*(5 - 1) + 1
price.new

# 長寬資料轉換 long and wide data -----
olddata_wide <- read.table(header=TRUE, text="
                           subject sex control cond1 cond2
                           1   M     7.9  12.3  10.7
                           2   F     6.3  10.6  11.1
                           3   F     9.5  13.1  13.8
                           4   M    11.5  13.4  12.9
                           ")
# Make sure the subject column is a factor
olddata_wide$subject <- factor(olddata_wide$subject)
str(olddata_wide)
olddata_wide

olddata_long <- read.table(header=TRUE, text='
                           subject sex condition measurement
                           1   M   control         7.9
                           1   M     cond1        12.3
                           1   M     cond2        10.7
                           2   F   control         6.3
                           2   F     cond1        10.6
                           2   F     cond2        11.1
                           3   F   control         9.5
                           3   F     cond1        13.1
                           3   F     cond2        13.8
                           4   M   control        11.5
                           4   M     cond1        13.4
                           4   M     cond2        12.9
                           ')
# Make sure the subject column is a factor
olddata_long$subject <- factor(olddata_long$subject)
str(olddata_long)
olddata_long

# tidyr package
library(tidyr)

# gather: From wide to long
data_long <- gather(olddata_wide, condition, measurement, control:cond2)
data_long

# spread: From long to wide
data_wide <- spread(olddata_long, condition, measurement)
data_wide

# reshape2 package
library(reshape2)

# melt: From wide to long

# Specify id.vars: the variables to keep but not split apart on
# method 1
melt(olddata_wide, id.vars=c("subject", "sex"))

# method 2
data_long <- melt(olddata_wide,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars=c("subject", "sex"),
                  # The source columns
                  measure.vars=c("control", "cond1", "cond2" ),
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
                  variable.name="condition",
                  value.name="measurement")
data_long

# dcast: From long to wide
data_wide <- dcast(olddata_long, subject + sex ~ condition, value.var="measurement")
data_wide

# 遺漏值與差補技術 -----

# 測試是否為NA
data(algae, package="DMwR")
head(algae)
summary(algae)

# 使用 is.na
is.na(algae$mxPH)
which(is.na(algae$mxPH))

# apply 家族 -----
m <- matrix(c(1:8), ncol=2)
m
apply(m, 1, function(x) mean(x))
as.matrix(apply(m, 1, function(x) mean(x)))

# R向量化處理 -----
x1 <- c(1:1000000)
system.time(x1.square <- x1^2)

x2 <- c(1:23000000)
system.time(x2.square <- x2^2)
head(x2.square)
tail(x2.square)

c(1,2,3,4) + c(4,5)
x <- matrix(c(1:6), ncol=2)
x
x + c(5,6)

# which, any, all -----
(x <- matrix(rnorm(10)*10, ncol=2))
x>5
# !(x>5)
which(x>5) # return index
which(x>5, arr.ind=TRUE) # return (row,column)
any(x>5)
all(x>5)

# loop and if 流程控制 -----

# 範例: if
netPrice <- 100
member <- "vip"

if (member == "vip") {
  salePrice <- netPrice * 0.7
} else if(member == "private") {
  salePrice <- netPrice * 0.8
} else {
  salePrice <- netPrice
}
salePrice

# 範例: 新增漲跌欄位-returns
head(tx1min)
tx1min.temp <- window(tx1min, start=as.POSIXct("2014-01-02 08:40:00"), end=as.POSIXct("2014-01-02 13:45:00"))
tx1min.temp <- as.data.frame(tx1min.temp)

for (i in (2:nrow(tx1min.temp))) {
  
  print(paste0("Number = ", i))
  
  if (tx1min.temp$Close[i] >  tx1min.temp$Close[i-1]) {
    tx1min.temp$returns[i] <- 1 # 1: up
  }
  
  else {
    tx1min.temp$returns[i] <- 0 # 2: down
  }
  
  print(tx1min.temp$returns[i])
}
head(tx1min.temp)

# 樞紐分析一把抓 -----

# 計算每日均線
# ?aggregate

selectdate <- as.Date(index(tx1min))
tx1min.day.mean <- aggregate(tx1min, selectdate, mean)
head(tx1min.day.mean)

# @@@Day 5. 1-3 R軟體的資料處理 -----
# 大型與大量資料處理
# SQL & MySQL資料庫整合應用
# 網頁資料擷取
# 全球財經開放數據應用
# 案例實作與討論(二)

# 大型資料處理 -----
# 建立2300萬筆模擬資料 
working <- "C:/rdata"
setwd(working)
getwd()
datasize <- 23000000
mydata <- matrix(c(NA), nrow=datasize, ncol=5)
set.seed(168)
mydata[,1] <- sample(c(1:17770), datasize, replace = TRUE)
mydata[,2] <- sample(c(1:480189), datasize, replace = TRUE)
mydata[,3] <- sample(c(1:5), datasize, replace = TRUE)
mydata[,4] <- sample(c(1999:2014), datasize, replace = TRUE)
mydata[,5] <- sample(c(1:12), datasize, replace = TRUE)
colnames(mydata) <- c("movie", "customer","rating","year", "month")
write.table(mydata, file="bigdata.txt", sep=" ", row.names=FALSE, col.names=TRUE)
# 2300萬*5, 491MB

# Ctrl + Shift + F10: 重新啟動R

# system.time(bigdata1 <- read.csv("C:/rdata/bigdata.txt", header=TRUE))

library(data.table)
system.time(movies <- fread("C:/rdata/bigdata.txt"))
dim(movies) # 23000000*5,  # 6.41秒
summary(movies)

# 大量資料處理 -----
working_path <- "C:/rdata"
setwd(working_path)
getwd()

sample1 <- iris[sample(1:nrow(iris),10),]
sample2 <- iris[sample(1:nrow(iris),10),]
sample3 <- iris[sample(1:nrow(iris),10),]

write.table(sample1, file="sample1.csv", sep=",", row.names=FALSE)
write.table(sample2, file="sample2.csv", sep=",", row.names=FALSE)
write.table(sample3, file="sample3.csv", sep=",", row.names=FALSE)

files <- dir(getwd(), pattern="sample.*.csv", recursive=TRUE, full.names=TRUE)
files
tables <- lapply(files, read.table, header=TRUE, sep=",") # list
sample.all <- do.call(rbind, tables) # data.frame
sample.all


# R與資料庫連結 -----
# RMySQL套件編譯與建立
# http://rwepa.blogspot.tw/2013/01/windows-rmysql.html

# RODBC 與 SQL Server 資料匯入與寫入
# http://rwepa.blogspot.tw/2013/08/rodbc-sql-server.html

# SQL語法-sqldf 套件 -----
# sqldf 套件 -----
library(sqldf)

# head
a1r <- head(warpbreaks)
a1r
a1s <- sqldf("select * from warpbreaks limit 6")
a1s
identical(a1r, a1s)

# subset
a2r <- subset(CO2, grepl("^Qn", Plant))
a2s <- sqldf("select * from CO2 where Plant like 'Qn%'")
all.equal(as.data.frame(a2r), a2s)
a2r

data(farms, package = "MASS")
a3r <- subset(farms, Manag %in% c("BF", "HF"))
a3s <- sqldf("select * from farms where Manag in ('BF', 'HF')")
row.names(a3r) <- NULL
identical(a3r, a3s)

a4r <- subset(warpbreaks, breaks >= 20 & breaks <= 30)
a4s <- sqldf("select * from warpbreaks where breaks between 20 and 30", row.names = TRUE)
identical(a4r, a4s)

a5r <- subset(farms, Mois == 'M1')
a5s <- sqldf("select * from farms where Mois = 'M1'", row.names = TRUE)
identical(a5r, a5s)

# 網頁資料擷取 -----
# load RCurl

library(RCurl)

# retrieving the content of the R homepage
Rproj <- getURL("http://www.r-project.org/")
Rproj

# load XML
library(XML)

# parsing the content of the R homepage
Rproj_doc <- htmlParse(Rproj) # htmlParse {XML}

# getForm
if(url.exists("http://www.google.com")) {
  # Two ways to submit a query to google. Searching for RCurl
  getURL("http://www.google.com/search?hl=en&lr=&ie=ISO-8859-1&q=RCurl&btnG=Search")
  
  # Here we let getForm do the hard work of combining the names and values.
  getForm("http://www.google.com/search", hl="en", lr="", ie="ISO-8859-1",  q="RCurl", btnG="Search")
  
  # And here if we already have the parameters as a list/vector.
  getForm("http://www.google.com/search", .params = c(hl="en", lr="", ie="ISO-8859-1",  q="RCurl", btnG="Search"))
}

# postForm
if(url.exists("http://wwwx.cs.unc.edu/~jbs/aw-wwwp/docs/resources/perl/perl-cgi/programs/cgi_stdin.cgi")) {
  postForm("http://wwwx.cs.unc.edu/~jbs/aw-wwwp/docs/resources/perl/perl-cgi/programs/cgi_stdin.cgi",
           name = "Bob", "checkedbox" = "spinich",
           submitButton = "Now!",
           textarea = "Some text to send",
           selectitem = "The item",
           radiobutton = "a", style = "POST")
}

# 中文網頁表格擷取,亂碼處理 -----
# part 1 設定R語系為C, 以配合中文編碼使用
Sys.setlocale(category="LC_ALL", locale="C")
Sys.getlocale()

# part 2 載入套件,讀取網頁資料
library(XML) # readHTMLTable 函數

# IE: F12 --> line 7: <meta http-equiv=Content-Type content="text/html; charset=big5"> 
# 觀察原始網頁採用 big5, 因此encoding參數設定為 big5

urls <- "http://web.ydu.edu.tw/~alan9956/02person.htm"
tmp <- readHTMLTable(urls, stringsAsFactors = FALSE, encoding="big5")

length(tmp) # length=5
train <- tmp[[1]] # 139*5

# part 3 資料整理
train[1,2] # 顯示亂碼
iconv(train[1,2], "UTF-8", "BIG-5") # 顯示正常

# 中文編碼轉換
train <- as.data.frame(lapply(train, function(x) iconv(x, "UTF-8", "BIG-5")))
names(train) <- c("id", "traindate", "program", "location", "hours")
head(train)

# 匯出資料
write.table(train, file="C:/rdata/train.txt", row.names=FALSE)

# RData 資料物件儲存
# save(資料物件1, 資料物件2, file=“myData2017.RData” )

# RData 資料物件匯入
# load(“myData2017.RData”)

# save(train, file="C:/rdata/train.RData") # 亦可儲存成 RData 格式

# 全球財經開放數據應用 -----

# fPortfolio 套件
# Windows 版本如果有安裝錯誤
# 下載 Rsymphony 套件並安裝
# https://cran.r-project.org/bin/windows/contrib/3.2/Rsymphony_0.1-22.zip
library(fPortfolio)

# 檢視資料集
# view default data sets
data(package="fPortfolio")

data(SWX)

class(SWX)
head(SWX)
colnames(SWX)
SWX


x <- as.data.frame(SWX[,2])
hist(x, breaks = 25, col = 'gray')
seq(min(x),max(x))
head(x)
# 讀取資料集-readSeries
# create small data set
mydata <- head(SWX[, 1:3])
class(mydata)

# write data to a CSV file in the current directory
write.csv(mydata, file="myFinance.csv")

# read data set
data1 <- readSeries(file="myFinance.csv", header=TRUE, sep=",")
data1

# 讀取資料集-yahooSeries -----
# read data set - internet
library(fImport)
ibm <- yahooSeries("IBM")

head(ibm)
tail(ibm)
end(ibm)

# 資料排序 -----
# sorting
myibm <- sample(ibm[c(1:5),])
myibm
sort(myibm)

# rev 相反順序
sort(myibm, decreasing=TRUE)
rev(sort(myibm))

# 資料合併 - cbind -----
# merge
ts1 <- ibm[sample(1:nrow(ibm),5),2]
ts1
ts2 <- ibm[sample(1:nrow(ibm),5),3]
ts2

cbind(ts1,ts2)

# 資料合併-rbind -----
rbind(ts1,ts2)

# fPortfolio 套件-統計函數 -----
library(fPortfolio)
summary(SWX)

basicStats(SWX)

# 輸入練習class, head, colnames

# plot繪圖-線圖
plot(SWX)

plot(SWX[, 6:4], plot.type="single", col=2:4, xlab = "Date", ylab = "LP Index Family")
title(main = "LP25 - LP40 - LP60")
hgrid()

# plot繪圖-散佈圖
SBI.RET <- 100 * SWX.RET[, "SBI"]
SPI.RET <- 100 * SWX.RET[, "SPI"]
plot(SBI.RET, SPI.RET, xlab = "SBI", ylab = "SPI", pch = 19, cex = 0.4, col = "brown")
grid()

# plot繪圖-進階
SPI <- SWX[, "SPI"]
SPI.RET <- SWX.RET[, "SPI"]

seriesPlot(SPI)
returnPlot(SPI)
cumulatedPlot(SPI.RET)

# 3列,2行繪圖
par(mfcol = c(3, 2))
seriesPlot(SWX)

# 客製化繪圖
par(mfrow = c(1, 1))
seriesPlot(SPI, labels = FALSE, type = "h", col = "brown", title = FALSE, grid = FALSE, rug = FALSE)
lines(SPI, col = "orange")
title(main = "Swiss Performance Index")
hgrid()
box_()
copyright()
mtext("SPI", side=3, line=-2, adj=1.02, font=2)

# 案例實作與討論(二) SP500個案討論 -----

sp500 <- read.csv("http://web.ydu.edu.tw/~alan9956/rdata/sp500.csv")

ind <- sample(nrow(sp500), 5)

spindex <- as.character(sp500$Symbol[ind])

library(quantmod)
getSymbols(spindex, src="yahoo", from=as.Date("2016-01-01"), end = as.Date("2016-12-31"))

# @@@Day 5. 3/11 1-4 R軟體的財經數據視覺化應用 -----

# 複雜結構的財經數據視覺化 -----
# refer to materials

# 互動式視覺化應用：googleVis套件 -----
# gvisSankey
library(googleVis)
dat <- data.frame(From=c(rep("A",3), rep("B", 3)), 
                  To=c(rep(c("X", "Y", "Z"),2)), 
                  Weight=c(5,7,6,2,9,4))

sk1 <- gvisSankey(dat, from="From", to="To", weight="Weight")
plot(sk1)

# Web化服務應用：shiny套件
# shiny套件簡介 -----

# 顯示內建11個範例
library(shiny)
dir(paste0(.libPaths(), "/shiny/examples"))

# shiny example - 01_hello
runExample("01_hello")

runExample("02_text")

# 建立第一個shiny網頁程式 - myFirstShiny -----
# 上課練習

runExample("09_upload")

# shiny 範例 - googleVis plot
# http://web.ydu.edu.tw/~alan9956/rdata/shinygoogleVis_scatter.zip

# shiny 範例 - shinyCurve
# http://web.ydu.edu.tw/~alan9956/rdata/shinyCurve.zip

# shiny App 如何佈署
# 方法1: shiny server 免費版(安裝於住家/公司)
# 方法2: shiny Server Pro 付費版
# 方法3: https://www.shinyapps.io/
# end