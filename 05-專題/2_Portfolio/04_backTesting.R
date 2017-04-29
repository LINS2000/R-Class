setwd('D:/R-Class/05-專題/2_Portfolio')

library(fPortfolio)
###=== Step 1. Prepare the data

       ## 1-1. The data
infile=read.csv(".Close50.csv")
tail(infile,2) 
dateID=as.POSIXlt(as.Date(infile[,1]), format='%Y-%m-%d')
myData0=infile[,-1]
rownames(myData0)=dateID
myData0=as.timeSeries(myData0)
tail(myData0)
ncol(myData0)
assetReturns=returns(myData0)[,-1]*100

ID=as.character(read.table("file_twii50.csv",sep=",")$V2)
colnames(assetReturns)=ID
head(assetReturns)
dim(assetReturns)
       ## 1-2. Prepare the benchmark veriable, we use average.
#avg=rowMeans(assetReturns)
avg=returns(myData0)[,1]*100
newData = cbind(avg,assetReturns)
colnames(newData)=c("Lhs",colnames(assetReturns))
tail(newData)

###=== Step 2. Prepare the portfolio setting
load("myCOV.RData")
portSpec = portfolioSpec()
myConstraints = "LongOnly" 
if (myConstraints=="Short"){setSolver(portSpec)= "solveRshortExact"} else {setSolver(portSpec)= "solveRquadprog"}
###=== Step 3.Prepare the formula
Rhs=paste(names(newData[,-1]), collapse= " + ")
Formula = paste("Lhs ~",Rhs, sep="")

###=== Step 4. Conduct the back-testing initial conditions and estimation
backtestSpec = portfolioBacktest()
setStrategyFun(backtestSpec)=c("GMVPStrategy","tangencyStrategy")[2]
  #setSmootherLambda(backtestSpec) = "3m"
  #setWindowsHorizon(backtestSpec) = "12m"

###=== Step 5. Main backtesting process
       ## 5-1. Initial Estimation
rawOutput = portfolioBacktesting(formula = as.formula(Formula),data = newData, spec=portSpec,backtest=backtestSpec, constraints = myConstraints, trace = FALSE)
Weights = round(100*rawOutput$weights, 2)
tail(Weights,3)

       ## 5-2. Smoothing
smoothOutput = portfolioSmoothing(object = rawOutput,trace = FALSE)
END=time(tail(smoothOutput$portfolio,2))
weightsDecision=smoothOutput$smoothWeights[as.character(END),]  
round(t(cbind(weightsDecision)),4)
smoothWeights = round(100*smoothOutput$smoothWeights,2)

t(tail(smoothWeights,2))
dateID=rownames(smoothWeights)
pickDate=dateID[54]
advice0=t(smoothWeights[pickDate,])
ADVICE=data.frame(advice0[,advice0 !=0])
colnames(ADVICE)=as.character(pickDate)
ADVICE

###=== Step 6. Plotting Backtesting Results
#Sys.setlocale(category = "LC_ALL", locale = "Chinese (Traditional)_Taiwan.950")
#Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
backtestPlot(smoothOutput, which="all",cex = 0.6, font=1, family="mono")

###=== Step 7.  Index Performance Review
netPerformance(smoothOutput)
smoothOutput$stats

### R Homework   
## 1. To change Step 1-2, what can you do?
## 2. Do your best, change settings of Step 2.
## 3. Fit new dataset World24_Close.csv to this code, and report your results.