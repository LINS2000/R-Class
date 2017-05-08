install.packages('highcharter')
install.packages('dplyr')


library(highcharter)
library(dplyr)

setwd('D:/R-Class/04-機器學習/樂陞')

# Load Data
data_1 = read.table('3662ID.txt', sep=',', header=TRUE)
dataname_1 = read.table('3662Name.txt', sep=',', header=TRUE)

# Data preprocessing
data_1$Name = dataname_1[,1] 
colnames(data_1) = c('ID', 'Date', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'Name')
data_1$Date = paste(substr(data_1$Date, 1, 4), '/', substr(data_1$Date, 5, 6), '/', substr(data_1$Date, 7, 8), sep = '')
data_1 = data_1[order(data_1$ID, as.Date(data_1$Date, format='%Y/%m/%d')),]


# Kmeans
km = kmeans(data_1[,c(3,5,7)], 3, algorithm='Lloyd')
data = mutate(data_1, km=km$cluster[1:dim(data_1)[1]]) # add new variables
dataID = data_1[1,1]
dataName = dataname_1[1,1]

rownames(data) = data$Date 

data1 = data[data$km==1,]
data2 = data[data$km==2,]
data3 = data[data$km==3,]
rownames(data1) = data1$Date
rownames(data2) = data2$Date
rownames(data3) = data3$Date

hc2 = highchart() %>%
  hc_title(text=paste(dataID, dataName), style=list(color='#FF00FF', size="30px", fontFamily='微軟正黑體'), margin=30, y=30) %>%
  hc_add_series_scatter(data1$x1, data1$x3, data1$x5, Date=data1$Date, showInLegend=TRUE, label=data1$Date) %>%
  hc_add_series_scatter(data2$x1, data2$x3, data2$x5, Date=data2$Date, showInLegend=TRUE, label=data2$Date) %>%
  hc_add_series_scatter(data3$x1, data3$x3, data3$x5, Date=data3$Date, showInLegend=TRUE, label=data3$Date) %>%
  hc_tooltip(pointFormat='{point.Date}<br/>融券賣出{point.x}<br/>券增減{point.y}<br/>借券賣出{point.z}')
hc2  
