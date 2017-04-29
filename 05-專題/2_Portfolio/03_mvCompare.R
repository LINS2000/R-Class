# Compare two strategies
library(fPortfolio)
infile=read.csv(".Close50.csv")
tail(infile,2) 
dateID=as.POSIXlt(as.Date(infile[,1]), format='%Y-%m-%d')
myData0=infile[,-1]
rownames(myData0)=dateID
myData0=as.timeSeries(myData0)
tail(myData0)
ncol(myData0)
myData=returns(myData0)[,-1]*100

ID=as.character(read.table("file_twii50.csv",sep=",")$V2)
colnames(myData)=ID
head(myData)
dim(myData)



#####=== Portfolio 1. GMVP Portfolio: 
SpecGMVP= portfolioSpec()
myPort.GMVP=minriskPortfolio(data = myData,spec = SpecGMVP, constraints = "LongOnly")
print(myPort.GMVP)

#####=== Portfolio 2. Tangency  Portfolio: maximal Sharpe ratio
SpecTangency = portfolioSpec()
myPort.tangency=tangencyPortfolio(data = myData,spec = SpecTangency, constraints = "LongOnly")
print(myPort.tangency)

#### Plotting Results
#==== 1. Plotting by comparing  Optimal Results of two+ Portfolios
col = divPalette(ncol(myData), "RdBu")

dev.new();par(mfrow=c(3,2))

  weightsPie(myPort.GMVP, radius = 0.7, col =  divPalette(ncol(myData), "RdBu"),cex=5)
  mtext(text = "GMVP", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)
  weightsPie(myPort.tangency, radius = 0.7, col =  divPalette(ncol(myData), "RdBu"))
  mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)

  weightedReturnsPie(myPort.GMVP, radius = 0.7, col =  divPalette(ncol(myData), "PRGn"))
  mtext(text = "GMVP", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)
  weightedReturnsPie(myPort.tangency, radius = 0.7, col =  divPalette(ncol(myData), "PRGn"))
  mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)

  covRiskBudgetsPie(myPort.GMVP, radius = 0.7, col =  divPalette(ncol(myData), "Spectral"))
  mtext(text = "GMVP", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)
  covRiskBudgetsPie(myPort.tangency, radius = 0.7, col =  divPalette(ncol(myData), "Spectral"))
  mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)
par(mfrow=c(1,1))

### Discussion: The limitation of visualiztion