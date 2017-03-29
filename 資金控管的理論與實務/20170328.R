library(quantmod)

getSymbols('IBM')
IBM
head(to.weekly(IBM))
tail(to.monthly(IBM))
tail(to.yearly(IBM))

# 
chartSeries(IBM)
chartSeries(to.monthly(IBM), up.col='red', dn.col='green', theme='white')


STK <- get(getSymbols('2330.TW'))
STK
head(to.weekly(STK))
tail(to.monthly(STK))
tail(to.yearly(STK))

# 繪圖
chartSeries(STK)
chartSeries(STK, up.col='red', dn.col='green', theme='white')
chartSeries(to.weekly(STK), up.col='red', dn.col='green', theme='white')
chartSeries(to.monthly(STK)["2011-01-03::2017-09-30"], up.col='red', dn.col='green', theme='white')


# 開收高低
head(Op(STK))
head(Cl(STK))
head(Hi(STK))
head(Lo(STK))

# 移動平均
SMA(Cl(STK))
head(cbind(Cl(STK),SMA(Cl(STK))),15)
mean(Cl(STK)[1:10])  #verify

# 繪圖
addSMA()  #10日平均線
addSMA(20, col='blue')  #20日平均線
addBBands()  #不靈通道
addBBands(10,sd=3)
addRSI()
addMACD()

# ^TWII

# 若開盤買收盤賣
profit <- Cl(STK)-Op(STK)
head(profit)
head(cbind(Op(STK),Cl(STK),profit))
tail(cbind(Op(STK),Cl(STK),profit))
sum(profit)
class(profit)
profit
chartSeries(profit)
cumsum(profit)   #累積
# cumsum(1:10)
plot(cumsum(profit))


# 範例 黃金交叉買進,死亡交叉賣出
STK <- get(getSymbols('2330.TW'))
STK <- as.matrix(STK)

head(STK)
rownames(STK)

ma20=SMA(Cl(STK),20)
ma60=SMA(Cl(STK),60)

#numeric(10) #test
profit=setNames(numeric(length(rownames(STK))),rownames(STK))
head(profit)
class(profit)

ts.plot(cbind(ma20,ma60), type='l', lwd=c(2,2), col=c('red','blue'),ylab='price')

m=61
while(m<=length(rownames(STK))){
  fee <- Cl(STK)[m]*0.006
  
  if (ma20[m-1]<ma60[m-1] && ma20[m]>ma60[m]){
    long=m
    while(ma20[m]>=ma60[m] && m<length(rownames(STK))){m=m+1}
    profit[long]=Cl(STK)[m]-Cl(STK)[long]-fee
    points(long,Cl(STK)[long],pch=17)
    points(m,Cl(STK)[m],pch=20,col='gold')
  }
  m=m+1
  
}

plot(cumsum(profit),type='l',col='red',lwd=2)

profit[profit>0]
profit[profit<0]
