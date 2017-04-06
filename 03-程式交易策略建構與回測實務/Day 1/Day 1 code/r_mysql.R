##############################################################
# R MysQL 資料操作範例程式碼
# 僅限 證基會課程學員教學使用 
# 
# 2016/09/10 
#
# Julian Lin 

# 
# 透過 RMySQL 連線 MySQL 資料庫
#

install.packages("RMySQL")
install.packages("dplyr")


library(RMySQL)
con = dbConnect(MySQL(),user="root", password="610322",
                dbname="world", host="localhost")
query <- "SELECT Name, CountryCode, District, Population
FROM world.city
WHERE CountryCode= 'TWN' AND Population > 500000;"
myData<-dbGetQuery(con, query)
View(myData)
class(myData)

# 
# 透過dplyr 連線 MySQL 資料庫
#

library(dplyr)
conDplyr= src_mysql(dbname= "world", user = "admin",
                    password = "admin", host = "localhost")
myData<-conDplyr%>%
  tbl("city") %>%
  select(Name, CountryCode, District, Population) %>%
  filter(CountryCode== 'TWN', Population > 500000 ) %>%
  collect()
View(myData)
