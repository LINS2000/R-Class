
library(quantstrat)

#########################################
# Luxor 外匯商品交易 雙均線策略 參數設定

Sys.setenv(TZ="UTC")

initDate = '2002-10-21'
.from=initDate
.to='2002-10-31'

currency(c('GBP', 'USD'))
exchange_rate('GBPUSD', tick_size=0.0001)

getSymbols.FI(Symbols='GBPUSD',
              dir=system.file('extdata',package='quantstrat'),
              from=.from, to=.to)

GBPUSD = to.minutes30(GBPUSD)
GBPUSD = align.time(GBPUSD, 1800)


myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
chart_Series(GBPUSD,theme=myTheme)

#########################################
# 雙均線參數相關設定

# moving average lengths
.fast = 10
.slow = 30

# optimization range
.FastSMA = (1:30)
.SlowSMA = (20:80)

# trade parameters
.threshold = 0.0005
.orderqty = 100000
.txnfees = -6 # round-trip fee

# stop loss amount
.stoploss <- 0.30/100
.StopLoss = seq(0.05, 0.6, length.out=48)/100

# trading window
.timespan = 'T00:00/T23:59'

# number of optimization samples
.nsamples=80


portfolio.st = 'forex'
account.st = 'IB1'
strategy.st = 'luxor'

#########################################
# 啟始化帳本與投資組合相關資料庫

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)

strategy(strategy.st, store=TRUE)


#########################################
# 新增指標
add.indicator(strategy.st, name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlow"
)


#########################################
# 新增訊號
add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='short'
)


#########################################
# 新增交易規則

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit',
                        prefer='High',
                        threshold=.threshold,
                        orderqty=+.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit',
                        prefer='Low',
                        threshold=-.threshold,
                        orderqty=-.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2LONG'
)


#########################################
# 開始執行回測 並更新帳戶資料
out <- applyStrategy(strategy.st, portfolio.st)

updatePortf(portfolio.st, Symbols='GBPUSD',
            Dates=paste('::',as.Date(Sys.time()),sep=''))


chart.Posn(portfolio.st, "GBPUSD",
           TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)",theme=myTheme)

PerformanceAnalytics:::textplot(t(tradeStats(portfolio.st, 'GBPUSD')))

mk <- mktdata['2002-10-23 15:00::2002-10-24 03:00']
mk.df <- data.frame(Date=time(mk),coredata(mk))
PerformanceAnalytics:::textplot(mk.df,show.rownames=F)

ob <- getOrderBook(portfolio.st)$forex$GBPUSD
ob.df <- data.frame(Date=time(ob),coredata(ob))

PerformanceAnalytics:::textplot(ob.df,show.rownames=F)

PerformanceAnalytics:::textplot(perTradeStats(portfolio.st,"GBPUSD"),
                                show.rownames=F)


##################################################################################
# 設定參數掃描區間

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nFast',
                 variable = list(n = .FastSMA),
                 label = 'nFAST'
)

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nSlow',
                 variable = list(n = .SlowSMA),
                 label = 'nSLOW'
)

#########################################
# 設定參數掃描的限制條件

add.distribution.constraint(strategy.st,
                            paramset.label = 'SMA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'SMA'
)

#########################################
# 重新啟始化帳本與投資組合相關資料庫

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st,
         initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)


#########################################
# 平行運算起始化

library(doParallel)
# Mac/Linux 用doMC
# library(doMC)
detectCores()
if( Sys.info()['sysname'] == "Windows" )
{
  library(doParallel)
  registerDoParallel(cores=detectCores())
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}

#########################################
# 用目前CPU所有核心, 開始參數掃描計算

results <- apply.paramset(strategy.st, paramset.label='SMA',
                          portfolio.st=portfolio.st, account.st=account.st, nsamples=0)

#########################################
# 結果用常短天參數順序排列

tS <- results$tradeStats
idx <- order(tS[,1],tS[,2])
tS <- tS[idx,]
PerformanceAnalytics:::textplot(t(tS)[,1:10])


#########################################
# 收益與風險 熱區圖
# net profit
z <- tapply(X=tS[,"End.Equity"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)

x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("淨利(NP)")

# MaxDD
z <- tapply(X=tS[,"Max.Drawdown"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("最大回檔(MDD)")


#########################################
# 收益與最大回檔比 熱區圖

# return to maxdd
z <- tapply(X=tS[,"Profit.To.Max.Draw"],
            INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("收益與最大回檔比")


rmdd <- tS$Profit.To.Max.Draw
idx <- order(rmdd,decreasing=T)[1:30]
labs <- paste(tS$nFAST[idx],tS$nSLOW[idx],sep="/")
barplot(rmdd[idx],names.arg=labs,col=4,las=2,main="收益與最大回檔比值")


#########################################
# 參數高原3D 圖
# net profit

tradeGraphs (stats = tS, free.params = c("nFAST", "nSLOW"),
             statistics = c("Profit.To.Max.Draw","Net.Trading.PL", "Max.Drawdown",
                            "Avg.Trade.PL", "Num.Trades", "Profit.Factor"), title = '')
