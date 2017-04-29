library(fPortfolio)

#temp=read.csv("returnsDaily24.csv")
#datan=as.timeSeries(temp)


###=== Step 0 ===### Declare a NULL set for Portfolio Specifications  	

mySpec = portfolioSpec()

###=== Step 1 ===### 	Select Type of Portfolio Risk, by measures
setType(mySpec)=c("MV","CVaR")[1]	
	# "MV" 		classic mean-variance (Markowitz) portfolio
	# "CVaR" 	mean- conditional VaR portfolio

###=== Step 2 ===### 	Select Numerical Solvers
setSolver(mySpec)= c("solveRquadprog","solveRshortExact","solveRglpk.CVAR")[1]
## "solveRquadprog"	for LongOnly
## "solveRshortExact"  for Short
## "solveRglpk.CVAR"   for CVaR type

###=== Step 3 ===### 	Select constraints

myCon=c("LongOnly","Short")[1]


###=== Step 4 ===### 	Select Objective Function
setOptimize(mySpec)="minRisk"
	# "minRisk"		#default
	# "maxReturn"

###=== Step 5 ===### 	Other Settings
	## setTargetRisk(mySpec) =
	## setTargetReturn(mySpec) =  
	## setRiskFreeRate(mySpec) =  
	## setWeights(mySpec) =  
	## setAlpha(mySpec) =

###=== Step 6 ===### 	Select Covariance
load("myCOV.RData")
setEstimator(mySpec) = "covEstimator"	
  ###=== Covariance Settings ===### 	
	# "covEstimator" 	Sample covariance estimator
	# "covLedoit"		  Improved Robust Covariance in JEF(2003)
	# "covStudent"	  Covariance based on Student t distribution
  # "ShrinkCC";    "GoldSach"; "SKCov"
	# "kendallEstimator" 	Kendall's rank estimator
	# "mcdEstimator" 		MCD, minimum covariance determinant estimator
	# "covOGKEstimator" 	Orthogonalized Gnanadesikan-Kettenring estimator


###=== Step 7 ===### Strategy, be sure to set "LongOnly" or "Short"

tangentPortfolio=tangencyPortfolio(data = datan,spec = mySpec, constraints = myCon)
print(tangentPortfolio)

GMVP=minriskPortfolio(data = datan,spec = mySpec, constraints = myCon)
print(GMVP)
	# tangencyPortfolio() 	highest return/risk ratio (Sharpe Ratio)
	# minriskPortfolio() 	  GMVP 
  # efficientPortfolio() 	QP1: setTargetReturn(mySpec)=
  # maxreturnPortfolio() 	QP2: setTargetRisk(mySpec) =
  # feasiblePortfolio() 	feasible portfolio, given weights

ret_TangencyP=datan %*% as.numeric(getWeights(tangentPortfolio))
rownames(ret_TangencyP)=rownames(datan)
colnames(ret_TangencyP)="Tangency"
ret_TangencyP=as.timeSeries(ret_TangencyP)

ret_GMVP=datan %*% as.numeric(getWeights(GMVP))
rownames(ret_GMVP)=rownames(ret_GMVP)
colnames(ret_GMVP)="Tangency"
ret_GMVP=as.timeSeries(ret_GMVP)

par(mfrow=c(2,1))

plot(ret_TangencyP,main="Portfolio returns of Tangency Portfolio",ylab="",col="blue")
plot(ret_GMVP,main="Portfolio returns of GMVP Portfolio",ylab="",col="red")
par(mfrow=c(1,1))
#### Plotting Results
#==== Plotting by comparing Optimal Results of two Portfolios
col = divPalette(ncol(datan), "RdBu")
N=ncol(datan)
par(mfrow=c(3,2))
  weightsPie(GMVP, radius = 0.7, col =  divPalette(N, "RdBu"),cex=5)
  mtext(text = "GMVP", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)

  weightsPie(tangentPortfolio, radius = 0.7, col =  divPalette(N, "RdBu"))
  mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)

  weightedReturnsPie(GMVP, radius = 0.7, col =  divPalette(N, "PRGn"))
  mtext(text = "GMVP", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)

  weightedReturnsPie(tangentPortfolio, radius = 0.7, col =  divPalette(N, "PRGn"))
  mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)

  covRiskBudgetsPie(GMVP, radius = 0.7, col =  divPalette(N, "Spectral"))
  mtext(text = "GMVP", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)

  covRiskBudgetsPie(tangentPortfolio, radius = 0.7, col =  divPalette(N, "Spectral"))
  mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)
par(mfrow=c(1,1))


