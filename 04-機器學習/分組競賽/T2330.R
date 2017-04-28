


setwd('D:/GitHub/R-Class/04-機器學習/分組競賽')
getwd()


data=read.csv('2330-train.csv', header=T, sep=',')

closeprice=data$closeprice

lrow=nrow(data)


#
class=rep(0, lrow)

for(i in 1:(lrow-1)){
  
  data.fun=rep(closeprice[i], lrow-i)
  data.com=closeprice[(i+1):lrow]
  data.ret=(data.com-data.fun)/data.fun
  
  com10=c()
  com5=c()
  
  com5=which(data.ret<(-0.05))
  com10=which(data.ret>=0.1)
 
  if((length(com5)!=0)&(length(com10)!=0)){
  
    if(com10[1]<com5[1]){
      
      class[i]=1
    } 
  
  }
  
  if((length(com5)==0)&(length(com10)!=0)){
    
    class[i]=1
  }
}


#
class=class[1:(lrow-250)]
data=data[1:(lrow-250), 3:ncol(data)]
data.total=data.frame(class, data)



#
library(MASS)

fit1=lm(class~., data=data.total)
stepresult=stepAIC(fit1, direction='both')

#parse stepresult$call
x=toString(stepresult$call)
x=gsub('lm, class ~ ','',x)
x=gsub(', data.total','',x)
x=gsub(' ','',x)
x=gsub('v','',x)

y=strsplit(x,split="+",fixed=T)
a=y[[1]]
v=as.numeric(a)
c=paste('v',a,sep='')

#needdata.tsmc=data.frame(class,data[,3],data[,4],data[,5],data[,11],data[,12],data[,14],data[,15],data[,16],data[,17],data[,18],data[,20],data[,21],data[,22],data[,23],data[,25],data[,26],data[,27],data[,28],data[,30],data[,31],data[,32],data[,33],data[,34],data[,36],data[,37],data[,38],data[,40],data[,41],data[,42],data[,43],data[,45],data[,46],data[,47],data[,48],data[,49],data[,51],data[,55],data[,56],data[,57],data[,59],data[,60])
needdata.tsmc=data.frame(class,data[,v])
#names(needdata.tsmc)=c('class','v3','v4','v5','v11','v12','v14','v15','v16','v17','v18','v20','v21','v22','v23','v25','v26','v27','v28','v30','v31','v32','v33','v34','v36','v37','v38','v40','v41','v42','v43','v45','v46','v47','v48','v49','v51','v55','v56','v57','v59','v60')
names(needdata.tsmc)=c('class',a)
needdata=needdata.tsmc




#prepare testing data
data1=read.csv('2330-test.csv', header=T, sep=',')

closeprice=data1$closeprice

lrow=nrow(data1)


#
class=rep(0, lrow)

for(i in 1:(lrow-1)){
  
  data.fun=rep(closeprice[i], lrow-i)
  data.com=closeprice[(i+1):lrow]
  data.ret=(data.com-data.fun)/data.fun
  
  com10=c()
  com5=c()
  
  com5=which(data.ret<(-0.05))
  com10=which(data.ret>=0.1)
  
  if((length(com5)!=0)&(length(com10)!=0)){
    
    if(com10[1]<com5[1]){
      
      class[i]=1
    } 
    
  }
  
  if((length(com5)==0)&(length(com10)!=0)){
    
    class[i]=1
  }
}


#giving real class
realmatrix=c()




