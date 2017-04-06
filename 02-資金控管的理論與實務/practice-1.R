

library(quantmod)

stk = get(getSymbols('2330.TW'))
stk = to.weekly(stk["2016"])
head(stk,10)
plot(stk)
chartSeries(stk)


stk = as.matrix(stk)
head(stk,10)
class(stk)
stk[1:10]
summary(stk)
stk[stk[,1]<100]
rownames(stk)
colnames(stk)

plot(stk)
