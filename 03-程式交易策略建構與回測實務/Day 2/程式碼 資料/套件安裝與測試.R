


## 要先安裝Rtools軟體, 安裝後需重新開機 -- https://cran.r-project.org/bin/windows/Rtools/
install.packages("devtools")
require(devtools)


install.packages("FinancialInstrument", repos="http://R-Forge.R-project.org")
install.packages("blotter", repos="http://R-Forge.R-project.org", type="source")
install.packages("quantstrat", repos="http://R-Forge.R-project.org", type="source")

install.packages("PortfolioAnalytics")

install.packages("parallel")




install.packages("RCurl")

source_https <- function(url, ...) {
  # load package
  require(RCurl)
  
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

## 測試上面安裝的套件是否成功
# 測試程式
source_https("https://raw.githubusercontent.com/R-Finance/quantstrat/master/demo/bbands.R")
