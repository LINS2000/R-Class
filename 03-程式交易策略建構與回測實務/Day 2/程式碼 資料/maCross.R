library(quantstrat)

ttz<-Sys.getenv('TZ')
Sys.setenv(TZ='UTC')


if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env()

suppressWarnings(rm("order_book.macross",pos=.strategy))
suppressWarnings(rm("account.macross","portfolio.macross",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratMACROSS",'start_t','end_t'))


stock.str='AAPL' # 可以置換你想要的股票代碼
currency('USD')
stock(stock.str,currency='USD',multiplier=1)

startDate="2000-12-31"
initEq=1000000


portfolio.st='macross'
account.st='macross'
initPortf(portfolio.st,symbols=stock.str)
initAcct(account.st,portfolios=portfolio.st, initEq=initEq)
initOrders(portfolio=portfolio.st)

stratMACROSS<- strategy(portfolio.st)



stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=50),label= "ma50" )
stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)[,1]), n=200),label= "ma200")

stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(columns=c("ma50","ma200"), relationship="gte"),label="ma50.gt.ma200")
stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(column=c("ma50","ma200"),relationship="lt"),label="ma50.lt.ma200")

stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty=100, ordertype='market', orderside='long'),type='enter')
stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE, orderqty='all', ordertype='market', orderside='long'),type='exit')


getSymbols(stock.str,from=startDate)
for(i in stock.str)
  assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))

start_t<-Sys.time()
out<-applyStrategy(strategy=stratMACROSS , portfolios=portfolio.st)
end_t<-Sys.time()
print(end_t-start_t)

start_t<-Sys.time()
updatePortf(Portfolio='macross',Dates=paste('::',as.Date(Sys.time()),sep=''))
end_t<-Sys.time()
print("更新交易帳")
print(end_t-start_t)

chart.Posn(Portfolio='macross',Symbol=stock.str)
add_SMA(n=50 , on=1,col='blue')
add_SMA(n=200, on=1)

book    = getOrderBook('macross')
stats   = tradeStats('macross')
ptstats = perTradeStats('macross')
rets    = PortfReturns('macross')
txns    = getTxns('macross', stock.str)

#Date workaround, remove later
Sys.setenv(TZ=ttz)
