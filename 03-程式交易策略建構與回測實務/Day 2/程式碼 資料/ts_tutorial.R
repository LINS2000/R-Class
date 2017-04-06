require(quantmod)

sp500 <- na.omit(
  getSymbols("^GSPC", 
             from = "1949-12-31",
             auto.assign = FALSE)
)

sp500.monthly <- sp500[endpoints(
  sp500, on ="months")]

############################################################

sp500.df <- data.frame( 
  index(sp500.monthly),
  coredata(sp500.monthly),
  stringsAsFactors=FALSE )

colnames(sp500.df ) <- c("date","sp500") 

graphics::plot.default( 
  x = sp500.df$date, 
  y = sp500.df$sp500, 
  type = "l", 
  xlab = "Date", 
  ylab = "Closing Value", 
  main = "S&P 500 (graphics::plot.default)" )


############################################################

stats::plot.ts(
  ts(Cl(sp500.monthly),
     start = c(as.numeric(
       format(index(sp500.monthly)[1],"%Y")),as.numeric(
         format(index(sp500.monthly)[1],"%m"))
     ),
     frequency = 12
  ), 
  xlab = "Date",
  ylab = "Closing Value",
  main = "S&P 500 (stats::plot.ts)"
)



############################################################

require(lattice) 

lattice::xyplot( sp500 ~ date, 
                 data = sp500.df, 
                 type = "l", 
                 main = "S&P 500 (lattice::xyplot)" )

############################################################

require(quantmod) 

quantmod::chartSeries(
  sp500.monthly, 
  theme = chartTheme("white"), 
  TA = c(addBBands()))

############################################################

xts::plot.xts( sp500.monthly, 
               ylab = "Closing Value", 
               main = "S&P 500 (xts::plot.xts)" 
)
############################################################


require(timeSeries) 

timeSeries::plot(  timeSeries(sp500.monthly$GSPC.Close), 
                   main = "S&P 500 (timeseries::plot)")


############################################################

PerformanceAnalytics:: charts.PerformanceSummary( ROC(sp500.monthly,
                                                      n = 1, 
                                                      type = "discrete"),
                                                  main = "S&P 500 (PerformanceAnalytics::charts.PerformanceSummary)")

