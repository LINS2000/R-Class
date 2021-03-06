

# set working-directory
setwd('D:/GitHub/R-Class/04-機器學習/分組競賽')
getwd()


#----------------------------------------------------------------------------------------------
# prepare training data
#----------------------------------------------------------------------------------------------

data=read.csv('data-all.csv', header=T, sep=',')
data=data[which(as.Date(data$date)<=as.Date('2014/12/31')),]  #只留2014/12/31前的

closeprice=data$closeprice
lrow=nrow(data)
row.train=lrow

# compute class
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


# delete tail data (-250 rows)
class=class[1:(lrow-250)]
data=data[1:(lrow-250), 3:ncol(data)]
data.total=data.frame(class, data)


get_variables <- function(stepresult.call){
  
  stepresult.call=attr(stepresult$terms, 'term.labels')
  stepresult.call=gsub('v','',stepresult.call)
  
  stepresult.variables=as.numeric(stepresult.call)
  
  return(stepresult.variables)
}

# do stepwise regression
library(MASS)

fit1=lm(class~., data=data.total)
stepresult=stepAIC(fit1, direction='both')

# select predict variables by AIC
# simplify to get stepresult's modal variables
vcol=get_variables(stepresult$call)

needdata.tsmc=data.frame(class, data[,vcol])
#needdata.tsmc=data.frame(class,data[,3],data[,4],data[,5],data[,11],data[,12],data[,14],data[,15],data[,16],data[,17],data[,18],data[,20],data[,21],data[,22],data[,23],data[,25],data[,26],data[,27],data[,28],data[,30],data[,31],data[,32],data[,33],data[,34],data[,36],data[,37],data[,38],data[,40],data[,41],data[,42],data[,43],data[,45],data[,46],data[,47],data[,48],data[,49],data[,51],data[,55],data[,56],data[,57],data[,59],data[,60])
#names(needdata.tsmc)=c('class','v3','v4','v5','v11','v12','v14','v15','v16','v17','v18','v20','v21','v22','v23','v25','v26','v27','v28','v30','v31','v32','v33','v34','v36','v37','v38','v40','v41','v42','v43','v45','v46','v47','v48','v49','v51','v55','v56','v57','v59','v60')
needdata=needdata.tsmc
#head(needdata)


#----------------------------------------------------------------------------------------------
# prepare testing data
#----------------------------------------------------------------------------------------------

data1=read.csv('data-all.csv', header=T, sep=',')

closeprice=data1$closeprice

lrow=nrow(data1)
row.all=lrow

# compute class
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


# giving real class
realmatrix=c()

for(i in (row.train+1):(row.all-120)){
  
  realmatrix[i-row.train]=as.matrix(class[i])
}

# compute class
a=row.train+1
b=row.all-120

sum(class[a:b]==1)
sum(class[a:b]==0)

# target(10%,-5%)
data1=data1[,3:ncol(data1)]
testdata.tsmc=data.frame(class, data1[,vcol])
testdata=testdata.tsmc

needdata$class=as.factor(needdata$class)
testdata$class=as.factor(testdata$class)

#----------------------------------------------------------------------------------------------
# predict testing data
#----------------------------------------------------------------------------------------------

# compute continuous error of prediction
calc_continuous_err <- function(predmx, realmx){
  
  cerr=0
  for(j in 2:length(predmx)){
    
    if((as.numeric(predmx[j])!=as.numeric(realmx[j])) & (as.numeric(predmx[j-1])!=as.numeric(realmx[j-1]))){
      
      cerr=cerr+1
    }
  }
  
  return(cerr)
}

# SVM(e1071)
library(e1071)

data.resultsvm=c()
prematrixsvm=c()


for(i in (row.train+1):(row.all-120)){
  
  # training data by svm -- create model
  tsvm=svm(class~., data=needdata[1:(row.train-250),], type='C-classification', cost=2, kernal='radial basis', gamma=0.1, scale=TRUE)
  
  # predict result by model
  pretsvm=predict(tsvm, testdata[i, -1])
  
  
  data.resultsvm[i-row.train]=sum(pretsvm==testdata$class[i])/length(pretsvm)
  #prematrixsvm[i-row.train]=as.character.factor(pretsvm)
  prematrixsvm[i-row.train]=as.character.factor(pretsvm)
}

#
summary(data.resultsvm)
(t=table(prematrixsvm, realmatrix))
sum(diag(t)/sum(t))
calc_continuous_err(prematrixsvm, realmatrix)  #連續出錯次數


# Decision Tree(C50)
library(C50)

data.resultc50=c()
prematrixc50=c()

needdata$class=as.factor(needdata$class)
testdata$class=as.factor(testdata$class)



for(i in (row.train+1):(row.all-120)){
  
  # training data by c50 -- create model
  trainc50=C5.0(class~., needdata[1:(row.train-250),], trial=5, control=C5.0Control(subset=FALSE, noGlobalPruning=TRUE, CF=0.25))
  
  # predict result by model
  predc50=predict(trainc50, testdata[i, -1], trials=5, type='class')
  
  
  data.resultc50[i-row.train]=sum(predc50==testdata$class[i])/length(predc50)
  prematrixc50[i-row.train]=as.character.factor(predc50)
  
}

#
summary(data.resultc50)
(t=table(prematrixc50, realmatrix))
sum(diag(t)/sum(t))
calc_continuous_err(prematrixc50, realmatrix)  #連續出錯次數

