require(foreach)
require(doParallel)


################################################################
## 單CPU, 單核心計算
##
cl <- makeCluster(1)
registerDoParallel(cl)


system.time(m <- foreach(i=1:100) %do% 
{ matrix(rnorm(1000*1000), ncol=5000); NULL } )


system.time(m <- foreach(i=1:100) %dopar% 
{ matrix(rnorm(1000*1000), ncol=5000); NULL } )


getDoParWorkers()

################################################################
##
##
cl <- makeCluster(1)
registerDoParallel(cl)

system.time(m <- foreach(i=1:100) %do% 
{ matrix(rnorm(1000*1000), ncol=5000); NULL } )


system.time(m <- foreach(i=1:100) %dopar% 
{ matrix(rnorm(1000*1000), ncol=5000); NULL } )


getDoParWorkers()


################################################################
## 結果資料合併方式
##

foreach(i = 1:5, .combine='c') %do% sqrt(i)  


foreach(i = 1:5, .combine='cbind') %do% sqrt(i)


foreach(i = 1:5, .combine='+') %dopar% sqrt(i)
