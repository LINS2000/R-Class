############################################################
## 台積電 布林通道逆勢交易策略
#  Walk Forward Analysis  
#
# 回測時間: 2007/1/1~2016/6/15
# 交易解析度: Daily
# 參數: MA 20日
#       STD 0.5
# 部位管理: 1/10 固定比率 無加減碼規則
# 
# 本金 TWD 1,000,000
# 交易成本: TWD 120
#
# WFA 方式: 滾動窗格 
# 測試時間: 1 年
# 訓練時間: 4年
#
## Julian Lin - 2016/06/15
############################################################

library(quantstrat)


rm(list=ls(all=TRUE))

###############################################################################
#1 系統啟始化 商品設定

stock.st = c("2330.TW")

currency("TWD")
stock(stock.st, currency="TWD",multiplier=1)

oldtz<-Sys.getenv('TZ')

Sys.setenv(TZ="Asia/Taipei")

# 回測時間區間
initDate = '2006-12-31'
startDate = '2007-01-01'
endDate = '2016-06-15'

initEq=1e6
tradeSize = initEq/10


###############################################################################
#2 台積電歷史日K資料

getSymbols(stock.st,from=startDate,to=endDate,index.class="POSIXct",adjust=T)


myTheme<-chart_theme()
myTheme$col$dn.col <-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
chart_Series(get(stock.st),name=stock.st,theme=myTheme)


## 定比例部位
osFixedDollar <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...)
{
  pos <- getPosQty(portfolio, symbol, timestamp)
  if( isTRUE(all.equal(pos,0)) )
  {
    ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
    orderqty <- sign(orderqty)*round(tradeSize/ClosePrice,-2)
  } else {
    orderqty <- 0
  }
  return(orderqty)
}

strat.st <- "bbands"

# rm.strat(strat.st)
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env()

###############################################################################
#3 交易策略設定 


strategy(strat.st, store=TRUE)

add.indicator(strat.st, name = "BBands",
              arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'), label='BBands')
add.signal(strat.st, name="sigCrossover",
           arguments=list(columns=c("Close","up"),relationship="gt"),
           label="Cl.gt.UpperBand")
add.signal(strat.st, name="sigCrossover",
           arguments=list(columns=c("Close","dn"),relationship="lt"),
           label="Cl.lt.LowerBand")
add.signal(strat.st, name="sigCrossover",
           arguments=list(columns=c("High","Low","mavg"),relationship="op"),
           label="Cross.Mid")


add.rule(strat.st, name='ruleSignal',
         arguments=list(sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-1000,
                        ordertype='market', orderside=NULL, threshold=NULL, osFUN=osFixedDollar,
                        orderset='ocoshort'),
         type='enter',label="SE")
add.rule(strat.st, name='ruleSignal',
         arguments=list(sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 1000,
                        ordertype='market', orderside=NULL, threshold=NULL, osFUN=osFixedDollar,
                        orderset='ocolong'),
         type='enter',label="LE")
add.rule(strat.st, name='ruleSignal',
         arguments=list(sigcol="Cross.Mid",sigval=TRUE, orderqty= 'all',
                        ordertype='market', TxnFees=-120, orderside=NULL, threshold=NULL),
         type='exit')


###############################################################################
#4 WFA 滾動窗參數設定


add.distribution(strat.st,
                 paramset.label = 'BBOPT',
                 component.type = 'indicator',
                 component.label = 'BBands',
                 variable = list(n = seq(10,30,by=5)),
                 label = 'n'
)
add.distribution(strat.st,
                 paramset.label = 'BBOPT',
                 component.type = 'indicator',
                 component.label = 'BBands',
                 variable = list(sd = seq(1,3,by=0.5)),
                 label = 'sd'
)

#
# WFA 平行CPU多核心計算設定
#
if( Sys.info()['sysname'] == "Windows" )
{
  library(parallel)
  # uncomment line below when combine function bug is fixed for Windows
  # registerDoParallel(cores=detectCores())  ## to-be-resolved by Julian
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}

###############################################################################
#5 開始執行回測

rm.strat("opt")
initPortf(name="opt", stock.st, initDate=initDate)
initAcct(name="opt", portfolios="opt",
         initDate=initDate, initEq=initEq)

initOrders(portfolio="opt", initDate=initDate)


results <- walk.forward(
  strategy.st=strat.st,
  paramset.label='BBOPT',
  portfolio.st="opt",
  account.st="opt",
  period='years',
  k.training=4,
  k.testing=1,
  nsamples=0,
  audit.prefix='wfa',
  anchored=FALSE,
  verbose=TRUE
)

###############################################################################
#6 WFA 回測結果 

# WFA回測結果總表
PerformanceAnalytics:::textplot(t(tradeStats("opt")))

# 已實現損益
txns <- getTxns("opt",stock.st)
txns$Net.Txn.Realized.PL <- round(txns$Net.Txn.Realized.PL)

# WFA 交易明細
PerformanceAnalytics:::textplot(head(txns))
PerformanceAnalytics:::textplot(tail(txns))

# Out-of-sample 樣本外損益
plot(getPortfolio("opt")$summary$Net.Trading.PL,minor.ticks=FALSE,type="h",col=4)

# 交易進出場部位圖
chart.Posn("opt",stock.st)


list.files(pattern="^wfa.*\\.RData$")

Sys.setenv(TZ=oldtz)



# 回測程式結束
###############################################################################
