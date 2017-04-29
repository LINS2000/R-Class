library(fPortfolio)

mySpec1= portfolioSpec()
setNFrontierPoints(mySpec1) = 30
Frontier1 = portfolioFrontier(datan, mySpec1)

# Weight plots and other plots
par(mfrow=c(3,1))
	weightsPlot(Frontier1,mtext = FALSE)
	text = "Mean-Variance Portfolio - Long Only Constraints"
	mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
	weightedReturnsPlot(Frontier1, mtext = F)
	covRiskBudgetsPlot(Frontier1, mtext = F)
par(mfrow=c(1,1))


# Frontier plot
tailoredFrontierPlot(object = Frontier1, mText = "MV Portfolio - LongOnlyConstraints",risk = "CVaR")

ternaryMap(datan, FUN=NULL,locator=FALSE, N=41, palette=topo.colors, nlevels=11)
