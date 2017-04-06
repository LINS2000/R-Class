# 整理 期交所 期貨歷史資料CSV 檔
# qyryear: 查詢年份
# qrymonth: 查詢月份
# qrydate: 查詢日期
#
# 期交所網站下載網址:  
# "https://www.taifex.com.tw/chinese/3/3_1_3.asp"
#
# 期交所實際目標URL:
# "https://www.taifex.com.tw/DailyDownload/DailyDownloadCSV/Daily_2016_07_12.zip"
#
#
# 僅限 證基會課程學員教學使用 
# 
# 2016/07/10 ver .01 單日指定日期download
# 2016/09/10 ver .02 增加網頁parse 判別當日下載功能
#                    判別撈取近月資料
#
# Julian Lin 


library(timeDate)


#-----------------------------------------------------------------------------------------------
#  判斷最近結算月
#

curDateStr <- unlist(strsplit(as.character(Sys.Date()), '-'))

# 測試用only
symbolid <- 'TXF'; qryyear <- '2017'; qrymonth <- '03';qryday <- '22'
#symbolid <- 'TXF'; qryyear <- curDateStr[1]; qrymonth <- curDateStr[2];qryday <- curDateStr[3]


# 判斷當日屬於哪一近月結算月
settleMonth <- timeNthNdayInMonth(paste(curDateStr[1], curDateStr[2],'01', sep ='-'), 3, 3)
if(Sys.Date() > as.Date(settleMonth["Data"])){
  print("leap")
  curDateStr <- unlist(strsplit(as.character(Sys.Date()+30), '-'))
  
  curStlMth <- paste(curDateStr[1], curDateStr[2], sep ='')
}else{
  print("not leap")
  curStlMth <- paste(qryyear, qrymonth, sep ='')
}
print(curStlMth)


#-----------------------------------------------------------------------------------------------
# 確定當天 期交所盤後資料網頁存放有當天盤後資料 
# 發現當天盤後資料 則啟動下載與資料清整 清整完後存入資料庫
#
library(rvest)

page <- read_html("http://www.taifex.com.tw/chinese/3/3_1_3.asp")

rst <- page %>% 
  html_nodes(xpath='//*[@id="printhere"]/table[2]') %>% 
  html_table(fill = TRUE)
rst <- rst[[1]]

if(nrow(rst[rst$日期 == paste(qryyear, qrymonth, qryday, sep ='/'),]) == 1){
  
  # 組成動態查詢字串
  tfeurl <- sprintf("https://www.taifex.com.tw/DailyDownload/DailyDownloadCSV/Daily_%s_%s_%s.zip", 
                    qryyear, qrymonth, qryday)
  
  # 暫存檔名
  tmpf <- sprintf("Daily_%s_%s_%s.csv", qryyear, qrymonth, qryday)
  
  
  # 解壓縮暫存檔
  temp <- tempfile()
  
  # 下載壓縮檔
  download.file(tfeurl, temp)
  
  tx <- read.csv(unz(temp, tmpf), header = FALSE, skip = 1, sep = ",")
  
  unlink(temp)
  
  # 清整資料
  # Trim spaces 
  tx$V2 =trimws(tx$V2)
  tx$V3 =trimws(tx$V3)

  tx <- tx[(tx$V3 == curStlMth & tx$V2 == "TX"),]
  
  tx <- subset(tx, select = c("V1", "V4", "V5", "V6"))
  
  colnames(tx) <- c("日期", "時間", "成交價", "成交量")
  tx$成交價 <- as.integer(tx$成交價)
  tx$成交量 <- as.integer(tx$成交量)/2
  
  
  
  #View(tx)
  
  # 連結MySQL 寫入資料庫
  library(RMySQL)
  
  con <- dbConnect(MySQL(), user="admin", password="admin", 
                   dbname="world", host="localhost",client.flag=CLIENT_MULTI_STATEMENTS)
  
  dbWriteTable(con,"txf", tx, overwrite=T)
  
  dbDisconnect(con)
  
  
  # 送件者 收件者
  library(mailR)
  
  sender <- "xxxxx@gmail.com"
  recipients <- c("xxxxx@yahoo.com")
  
  # 發送通知
  send.mail(from = sender,
            to = recipients,
            subject = "R 排程通知",
            body = sprintf("時間: %s 存入 %s 資料", Sys.time(), as.character(Sys.Date())),
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "xxxxx@gmail.com",            
                        passwd = "xxxxx", ssl = TRUE),
            authenticate = TRUE,
            encoding = 'utf-8',
            send = TRUE)
}








