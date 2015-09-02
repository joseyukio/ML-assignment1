
=====================================================
> thetaMatrix<-assignment1(xaux, theta, 1, 463714,0.05)

=========================================
> prediction(xaux, thetaMatrix,1, 463714)
[1] "OBSERVED DATA"
max=  2011 
min=  1922 
mean=  1998.386 
median=  2002 
[1] "LM Statistics"
max=  3915948 
min=  1932.309 
mean=  2006.827 
median=  1999.036 
Root Mean Squared Error=  5747.637 
[1] "Gradient Descent"
max=  -56218945 
min=  -32718833947 
mean=  -2323520796 
median=  -2090295760 
Root Mean Squared Error=  2629111659 
[1] "Gradient Descent Normalized"
max=  2012414 
min=  -55098.19 
mean=  9384.044 
median=  8934.929 
Root Mean Squared Error=  10428.09 
[1] "Normal Equation"
max=  4239.918 
min=  69.66567 
mean=  1988.727 
median=  2006.091 
Root Mean Squared Error=  137.6153 
[1] "end of process"


# rm(list = ls(all = TRUE))
# ls()
getwd()
setwd("C:/Users/edbkei/Documents/Administration/2015/UNICAMP/MO444 aprendizado de maquina/Assignment1/yearpredictionMSD/FirstDataSetTraining")
list.files()
#setwd("C:/Users/edbkei/Documents/Administration/2015/UNICAMP/MO444 aprendizado de maquina/Assignment1/yearpredictionMSD/SecondDataSetTest")
#list.files()
xaux<-as.matrix(read.table("trainingdata.csv.txt", header = TRUE, sep = ","))
x<-xaux[,2:91]
y<-xaux[,1]


costs <- function(x, y, theta) {
  #costs<-sum( (X %*% theta - y)^2 ) / (2*length(y))
  costs<-(t(x %*% theta - y)) %*% (x %*% theta - y) / (2*length(y))
# (1/(2*maxit))*t(x %*% beta-y) %*% (x %*% beta-y)
  #print(costs)
  return(costs)
}

# define the gradient function dJ/dtheata: 1/m * (h(x)-y))*x where h(x) = x*theta
# in matrix form this is as follows:
grad <- function(x, y, theta) {
  gradient <- (1/length(y))* (t(x) %*% (x %*% theta - y))
  #print(gradient)
  return(gradient)
}

# define gradient descent update algorithm
grad.descent <- function(x, y, theta, maxit,alpha){
  # theta <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=91) # Initialize the parameters
  
  # keep history
  print(alpha)
# cost_history <- double(maxit)
  theta_history <- list(maxit)
# error<-matrix(nrow=maxit,ncol=1)
  #y<-as.matrix(y)
  costinitial<-9.9e+99
  # alpha= set learning rate
  if (maxit<100) {maxit<-100}
  for (i in 1:maxit) {
#   beta<-grad(x, y, theta)
    costf<-(1/(2*length(y)))*t(x %*% theta-y) %*% (x %*% theta-y)
    #  print(i)
    error <- (x %*% theta - y)
    delta <- t(x) %*% error / length(y)
    cost_history[i]<-costf
    if(cost_history[i]>costinitial&&i>1) {} else {
       theta <- theta - alpha * delta
    }
    #print(i)
   # cost_history[i] <- costs(x, y, theta)
  #  cost_history[i]<-costf
   # theta <- theta - alpha  * beta 
    #cost_history[i] <- costs(x, y, theta)
    #  theta_history[[i]] <- theta
    #theta <- theta - alpha  * beta 
   print(c("i=",i,"cost=",cost_history[i]))
    if (cost_history[i]>costinitial) {
       return(theta)
    } else {
       diff<-abs(costinitial-cost_history[i])
       if (diff<0.03) {return(theta)}
         else { costinitial<-cost_history[i] }
    }
  }
  #print(cost_history)
  return(theta)
}


# define gradient descent update algorithm
assignment1 <- function(xaux, theta, sto, mo,learningrate){
  sp<-sto+mo
  xx<-xaux[sto:sp,1:91]
  yy<-xaux[sto:sp,1]
  for (i in 1:mo) {
    xx[i,1]<-1
  }

  # implement feature scaling/NORMALIZATION
  xx.scaled <- xx
  for (i in 1:91) {
    xx.scaled[,i] <- (xx[,i] - min(xx[,i]))/(max(xx[,i])-min(xx[,i]))
  }
  yy.scaled<-(yy-min(yy))/(max(yy)-min(yy))

  # analytical results with matrix algebra NORMAL EQUATION
  thetaN<-solve(t(xx)%*%xx)%*%t(xx)%*%yy # w/o feature scaling
  thetaNS<-solve(t(xx.scaled)%*%xx.scaled)%*%t(xx.scaled)%*%yy.scaled # w/ feature scalin
  
  #fitting a model lm function
  lmfit <- lm(y ~ x)
  thetaLM<-matrix(nrow=91,ncol=1)
  thetaLM <- as.matrix(coef(lmfit))
  #print(dim(thetaLM))
  #print(thetaLM)
  
  # GRADIENT DESCENT

  thetaGD<-grad.descent(xx, yy, theta, m,learningrate)
  thetaGDscaled<-grad.descent(xx.scaled, yy.scaled, theta, m,learningrate)
  thetaMatrix<-matrix(nrow=91,ncol=4)
  for (i in 1:91) {
    thetaMatrix[i,1]<-thetaLM[i,1]
    thetaMatrix[i,2]<-thetaGD[i,1]
    thetaMatrix[i,3]<-thetaGDscaled[i,1]
    thetaMatrix[i,4]<-thetaN[i,1]
  #  thetaMatrix[i,5]<-thetaNS[i,1]
  }
  

  return (thetaMatrix)
}

#m  sample size
# define prediction WORKING
prediction <- function(xaux, thetaMatrix, st, m){
  sp<-st+m
  x<-xaux[st:sp,1:91]
  y<-xaux[st:sp,1]
  for (i in 1:m) {
    x[i,1]<-1
  }
  
  print("OBSERVED DATA")
  theta<-thetaMatrix[,1]
  y3<-max(y)
  cat ("max= ",y3,"\n")
  y3<-min(y)
  cat ("min= ",y3,"\n")
  y3<-mean(y)
  cat ("mean= ",y3,"\n")
 y3<-median(y)
  cat ("median= ",y3,"\n")
  
  print("LM Statistics")
  theta<-thetaMatrix[,1]
  y2<-x %*% theta
  y3<-max(y2)
  cat ("max= ",y3,"\n")
  y3<-min(y2)
  cat ("min= ",y3,"\n")
  y3<-mean(y2)
  cat ("mean= ",y3,"\n")
  y3<-median(y2)
  cat ("median= ",y3,"\n")
  RMSE<-sqrt(mean((y-y2)^2))
  cat ("Root Mean Squared Error= ",RMSE,"\n")
  
  print("Gradient Descent")
  theta<-thetaMatrix[,2]
  y2<-x %*% theta
  y3<-max(y2)
  cat ("max= ",y3,"\n")
  y3<-min(y2)
  cat ("min= ",y3,"\n")
  y3<-mean(y2)
  cat ("mean= ",y3,"\n")
  y3<-median(y2)
  cat ("median= ",y3,"\n")
  RMSE<-sqrt(mean((y-y2)^2))
  cat ("Root Mean Squared Error= ",RMSE,"\n")
  
  print("Gradient Descent Normalized")
  theta<-thetaMatrix[,3]
  y2<-x %*% theta
  yN<-(y2-min(y2))/(max(y2)-min(y2))
  yd<-yN*(max(y2)-min(y2))+min(y2)
  y3<-max(yd)
  cat ("max= ",y3,"\n")
  y3<-min(yd)
  cat ("min= ",y3,"\n")
  y3<-mean(yd)
  cat ("mean= ",y3,"\n")
  y3<-median(yd)
  cat ("median= ",y3,"\n")
  RMSE<-sqrt(mean((yN-y2)^2))
  cat ("Root Mean Squared Error= ",RMSE,"\n")
  
  print("Normal Equation")
  theta<-thetaMatrix[,4]
  y2<-x %*% theta
  y3<-max(y2)
  cat ("max= ",y3,"\n")
  y3<-min(y2)
  cat ("min= ",y3,"\n")
  y3<-mean(y2)
  cat ("mean= ",y3,"\n")
  y3<-median(y2)
  cat ("median= ",y3,"\n")
  RMSE<-sqrt(mean((y-y2)^2))
  cat ("Root Mean Squared Error= ",RMSE,"\n")
  
  #return (RMSE)
return("end of process")
}
