## xts prac

require(quantmod)

idx <- c('^TWII')
x <- get(getSymbols(idx))

head(x)
class(x[1,1])
class(x)

tz <- Sys.getenv('TZ')
Sys.setenv(TZ='UTC')
plot(Hi(to.monthly(x)))
plot(Op(to.monthly(x)))

plot.xts(to.weekly(x))


require(xts)
data(sample_matrix)
head(m)

m <- m[seq(1,nrow(m),10),]
head(m)
colnames(m)


library(xts)
library(stringi)
x <- get(load(file = 'TX.RData'))
#class(x)

x$Time <- paste('0', trimws(x$Time), sep='')
x$Time <- stri_sub(x$Time,-6)
x$DateTime <- paste(x$Date, x$Time)
x$DateTime <- strptime(x$DateTime, '%Y%m%d %H%M%S')
x$DateTime <- as.POSIXlt(x$DateTime)
head(x,20)
str(x)

### x$Date <- as.Date(x$Date, '%Y%m%d')

y <- x[seq(1,nrow(x),by=1500),c(5,3,4)]



str(y)

tt <- as.xts(y, dateFormat="POSIXlt")

rownames(y) <- as.POSIXlt(y$DateTime)



#
require(quantmod)

sp500 <- na.omit(getSymbols('^GSPC', from='2016/1/1', auto.assign=FALSE))  #auto.assign=FALSE是不把資料自動給GSPC這個物件
head(sp500)

sp500[endpoints(sp500, on='months'),]
#sp500[endpoints(sp500, on='weeks'),]
#to.weekly(sp500)

index(to.weekly(sp500))      #指數
coredata(to.weekly(sp500))   #內容

plot(to.weekly(sp500), xlab='Date', ylab='price', main='SP500 INDEX')
