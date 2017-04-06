
library(xts)
library(timeDate)
library(lubridate)
library(quantmod)
require(doParallel)

# 下載檔放置位置
hist_dir <- 'C:/Users/julian/Downloads/hist'
setwd(hist_dir)

# 使用平行運算, 加快資料清整速度
cl <- makeCluster(4)
registerDoParallel(cl)


# 指數期貨資料轉換主程式
TXFExtract <- function(qryyear, qrymonth, qryday){
  
  # 判斷當日屬於哪一近月結算月
  settleMonth <- timeNthNdayInMonth(paste(qryyear, qrymonth,'01', sep ='-'), 3, 3)
  
  if(as.Date(paste(qryyear, qrymonth, qryday, sep ='-')) > as.Date(settleMonth["Data"])){
    print("leap")
    curStlMth <- substr(as.character(as.Date(paste(qryyear, qrymonth,'01', sep ='-')) %m+% months(c(1))), 1, 7)
    curStlMth <- gsub('-', '', curStlMth)
    
  }else{
    print("not leap")
    curStlMth <- paste(qryyear, qrymonth, sep ='')
  }
  print(curStlMth)
  
  unzip(zipfile=paste(hist_dir, "/Daily_", qryyear, "_", qrymonth, "_", qryday, ".zip", sep = ''), 
        exdir=".")
  
  tx <- read.csv(paste(hist_dir, "/Daily_", qryyear, "_", qrymonth, "_", qryday, ".csv", sep = ''), 
                 header = FALSE, skip = 1, sep = ",")
  
  unlink(paste("Daily_", qryyear, "_", qrymonth, "_", qryday, ".csv", sep = ''))
  
 
  # 清整資料
  # Trim spaces 
  tx$V2 =trimws(tx$V2)
  tx$V3 =trimws(tx$V3)
  
  tx <- tx[(tx$V3 == curStlMth & tx$V2 == "TX"),]
  
  tx <- subset(tx, select = c("V1", "V4", "V5", "V6"))
  
  colnames(tx) <- c("Date", "Time", "Price", "Volume")
  tx$Date <- as.character(tx$Date)
  tx$Time <- as.character(tx$Time)
  tx$Price <- as.integer(tx$Price)
  tx$Volume <- as.integer(tx$Volume)/2

  # 填齊8位時間個數
  tx[nchar(tx$Time) == 5,]$Time <- paste('0', tx[nchar(tx$Time) == 5,]$Time, sep = '')
  
  tx$Date <- paste(substr(tx$Date, 1, 4), '-', substr(tx$Date, 5, 6),'-', substr(tx$Date,7, 8),sep = '')
  tx$Time <- paste(substr(tx$Time, 1, 2), ':', substr(tx$Time, 3, 4),':', substr(tx$Time,5, 6),sep = '')
  tx$DateTime <- paste(tx$Date, tx$Time )
  
  tx <- subset(tx, select = c(DateTime, Price, Volume))
  
  # Data Frame 轉型為xts 時間序列物件
  tx_xts <- xts(tx[, -1], order.by=as.POSIXct(tx$DateTime))
  
  
  # 轉換tick 為分鐘 OHLC Bar data
  bars_1min <- period.apply(tx_xts, 
                            endpoints(tx_xts,"secs",60),
                            function(xx){
                              ticks=coredata(xx$Price)
                              c( first(ticks),max(ticks), min(ticks),
                                 last(ticks), sum(xx$Volume) )
                            })
  colnames(bars_1min) <- c("Open","High","Low","Close","Volume")
  align.time(bars_1min,60)
  
  return(bars_1min)
}

# 掃描目錄, 取Daily開頭的zip壓縮檔檔名
file.ls <- list.files(path=hist_dir, pattern=glob2rx("Daily_*.zip"))

for(l in seq_along(file.ls)){
  
  qryyear <- substr(file.ls[[l]], 7, 10)
  qrymonth <- substr(file.ls[[l]], 12, 13)
  qryday <- substr(file.ls[[l]], 15, 16)
  
  
  if(l == 1 ){
    txf1min <- TXFExtract(qryyear, qrymonth, qryday)
  }else{
    txf1min <- rbind(txf1min, TXFExtract(qryyear, qrymonth, qryday))
  }
  
  print(sprintf("共 %s 個交易日歷史資料, 目前轉第 %s 個檔, 日期: %s-%s-%s ", length(file.ls), l, qryyear, qrymonth, qryday ))
  
}

#################################################################################################

# 產生訊號資料表
txf15min <- to.minutes15(txf1min)
txf15min$ssma <- SMA(Cl(txf15min), n=10)
txf15min$slma <- SMA(Cl(txf15min), n=60)

txf15min$up <- NA; txf15min$dn <- NA; 
txf15min$up[txf15min$ssma > txf15min$slma, ] <- 1
txf15min$dn[txf15min$ssma < txf15min$slma, ] <- -1

txf15min_sig <- subset(txf15min, select = c(ssma, slma, up, dn))

# 最後一筆 為最近時間的觸動訊號，接下單機制(交易執行程式)
tail(txf15min_sig, 1)

if(tail(txf15min_sig, 1)$dn == -1)
{
  print("接交易程式下賣出單")
}
if(tail(txf15min_sig, 1)$up == 1)
{
  print("接交易程式下買入單")
}