
##############大型權值 中型 小型股 週K    #################

load("data/crsp_weekly.rda")
equity.data <- cbind(largecap_weekly[,1:15], 
                     midcap_weekly[,1:15], 
                     smallcap_weekly[,1:5])
market <- largecap_weekly[,21]
Rf <- largecap_weekly[,22]

##### edhec 美國hedge fund 資料庫 統計各種策略屬性與歷史績效 #####

equity.data <- cbind(largecap_weekly[,1:15], 
                     midcap_weekly[,1:15], 
                     smallcap_weekly[,1:5])
market <- largecap_weekly[,21]
Rf <- largecap_weekly[,22]

##### 例子3 用 #####
# Load the updated edhec dataset
load("data/edhec.rda")

# Prep data for Examples 3 and 4
R <- edhec[,c("Convertible.Arbitrage", "Equity.Market.Neutral", 
              "Fixed.Income.Arbitrage", 
              "CTA.Global", "Emerging.Markets", "Global.Macro")]
# Abreviate column names for convenience and plotting
colnames(R) <- c("CA", "EMN", "FIA", "CTAG", "EM", "GM")
load("data/edhec.rda")

R <- edhec[,c("Convertible.Arbitrage", "Equity.Market.Neutral", 
              "Fixed.Income.Arbitrage", 
              "CTA.Global", "Emerging.Markets", "Global.Macro")]
# Abreviate column names for convenience and plotting
colnames(R) <- c("CA", "EMN", "FIA", "CTAG", "EM", "GM")