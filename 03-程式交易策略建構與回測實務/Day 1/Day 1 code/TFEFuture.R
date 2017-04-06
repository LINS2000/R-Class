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
# 僅限 證基會課程學員教學使用 2016/07/25
#
# Julian Lin 

symbolid <- 'TXF'; qyryear <- '2017'; qrymonth <- '03';qrydate <- '20'

# 組成動態查詢字串
tfeurl <- sprintf("https://www.taifex.com.tw/DailyDownload/DailyDownloadCSV/Daily_%s_%s_%s.zip", 
        qyryear, qrymonth, qrydate)

# 暫存檔名
tmpf <- sprintf("Daily_%s_%s_%s.csv", qyryear, qrymonth, qrydate)


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

tx <- tx[(tx$V3 == "201703" & tx$V2 == "TX"),]

tx <- subset(tx, select = c("V1", "V4", "V5", "V6"))

colnames(tx) <- c("Date", "Time", "Price", "Volume")

# View(tx)







