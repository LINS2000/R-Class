library(PortfolioAnalytics)
require(doParallel)

cl <- makeCluster(4)
registerDoParallel(cl)


############ 投資組合條件設定 #################
stocks <- colnames(equity.data)

# 起始化條件
portf.init <- portfolio.spec(stocks)
# Add constraints
# 加權總數為 1
portf.dn <- add.constraint(portf.init, type="weight_sum", 
                           min_sum=-0.01, max_sum=0.01)


# 箱型 限制式
portf.dn <- add.constraint(portf.dn, type="box", 
                           min=-0.2, max=0.2)
portf.dn <- add.constraint(portf.dn, type="position_limit", max_pos=20)

betas <- t(CAPM.beta(equity.data, market, Rf))

portf.dn <- add.constraint(portf.dn, type="factor_exposure", B=betas, 
                           lower=-0.25, upper=0.25)



rp <- random_portfolios(portf.dn, 10000, eliminate=TRUE)

