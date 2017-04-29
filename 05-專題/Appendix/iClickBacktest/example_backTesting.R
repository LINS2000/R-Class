source("iClick.backTest.SRC")
temp0=read.csv(".Close50.csv")
temp1=temp0[,-1]
rownames(temp1)=as.Date(temp0[,1])
temp=as.timeSeries(temp0)
assetReturns=returns(temp)[,2:16]
head(assetReturns,2);tail(assetReturns,2)
#iClick.backTesting(assetReturns,riskType=c("MV","CVaR"),Lambda="1m")
iClick.backTesting(assetReturns)







load(".MV_Tangency.RData")
k=6
backtestPlot(OUT.tangency[[k]], cex = 0.6, font = 1, family = "mono", which="all");netPerformance(OUT.tangency[[k]])

smoothOutput = OUT.tangency[[k]]
END=time(tail(smoothOutput$portfolio,2))
weightsDecision=smoothOutput$smoothWeights[as.character(END),]  
round(t(cbind(weightsDecision)),4)
smoothWeights = round(100*smoothOutput$smoothWeights,2)

t(tail(smoothWeights,2))

dateID=rownames(smoothWeights)
pickDate=dateID[length(dateID)]
advice0=t(smoothWeights[pickDate,])
ADVICE=data.frame(advice0[,advice0 !=0])
colnames(ADVICE)=as.character(pickDate)
ADVICE

par(mfrow=c(3,1));backtestPlot(OUT.tangency[[k]], cex = 0.6, font = 1, family = "mono", which=c(4:6));par(mfrow=c(1,1))

load(".MV_GMVP.RData")
backtestPlot(OUT.GMVP[[k]], cex = 0.6, font = 1, family = "mono", which="all");netPerformance(OUT.GMVP[[k]])
par(mfrow=c(3,1));backtestPlot(OUT.GMVP[[k]], cex = 0.6, font = 1, family = "mono", which=c(4:6));par(mfrow=c(1,1))
#smoothWeights = round(100*OUT.GMVP[[k]]$smoothWeights,2)
