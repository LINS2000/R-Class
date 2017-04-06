library('PerformanceAnalytics')

data(managers)
head(managers)


managers.length = dim(managers)[1]
colnames(managers)

manager.col = 1
peers.cols = c(2,3,4,5,6)
indexes.cols = c(7,8)
Rf.col = 10

trailing12.rows = ((managers.length - 11):managers.length)
trailing36.rows = ((managers.length - 35):managers.length)
trailing60.rows = ((managers.length - 59):managers.length)

############################################################


charts.PerformanceSummary(
       managers[,c(manager.col,indexes.cols)],
         lwd=2, 
         ylog=TRUE)

############################################################

chart.Drawdown(managers, 
               main="Draw Down", 
               legend.loc="bottomleft")

############################################################

View(
  t(table.CalendarReturns(
    managers[,c(manager.col,indexes.cols)]
  )
  )
)

############################################################

chart.RiskReturnScatter(managers[trailing36.rows,1:8], 
                        Rf=.03/12, 
                        main = "Trailing 36-Month Performance", 
                        colorset=c("red", rep("black",5), "orange", "green"))


############################################################


layout(rbind(c(1,2),c(3,4)))
chart.Histogram(managers[,1,drop=F], 
                main = "Plain", methods = NULL)

chart.Histogram(managers[,1,drop=F], 
                main = "Density", breaks=40, 
                methods = c("add.density", 
                            "add.normal"))

chart.Histogram(managers[,1,drop=F], 
                main = "Skew and Kurt", 
                methods = c("add.centered",
                            "add.rug")) 

chart.Histogram(managers[,1,drop=F], 
                main = "Risk Measures", 
                methods = c("add.risk"))


############################################################

View(
  table.CAPM(
    managers[trailing36.rows, c(manager.col, peers.cols)],
    managers[ trailing36.rows, 8, drop=FALSE],
    Rf = managers[ trailing36.rows, Rf.col, drop=FALSE])
)

############################################################

table.Correlation(
  managers[, c(manager.col, peers.cols)], 
  managers[, 8, drop = F], 
  legend.loc = "lowerleft")

chart.RollingCorrelation(
  managers[,c(manager.col, peers.cols)], 
  managers[, 8, drop = FALSE], 
  colorset = tim8equal, 
  lwd = 2, 
  main = "12-Month Rolling Correlation")


############################################################