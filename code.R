#计算真值
t=c(0,0.67,0.84,1.28,1.65,2.32,2.58,3.09,3.72)
(true=pnorm(t))
#n=100次时
n1=100
(x1=c(0,0,0,0,0,0,0,0,0))
(y1=c(rnorm(n1,mean=0,sd=1)))
for(i in 1:9){
  a=0
  for(j in 1:n1){
    if(y1[j]<=t[i]){a=a+1}
  }
  x1[i]=a/n1
}

#n=1000时
n2=1000
(x2=c(0,0,0,0,0,0,0,0,0))
(y2=c(rnorm(n2,mean=0,sd=1)))
for(i in 1:9){
  b=0
  for(j in 1:n2){
    if(y2[j]<=t[i]){b=b+1}
  }
  x2[i]=b/n2
}

#n=10000时
n3=10000
(x3=c(0,0,0,0,0,0,0,0,0))
(y3=c(rnorm(n3,mean=0,sd=1)))
for(i in 1:9){
  c=0
  for(j in 1:n3){
    if(y3[j]<=t[i]){c=c+1}
  }
  x3[i]=c/n3
}
(table=data.frame(t,true,x1,x2,x3))
#n=100时,重复100次
n=100
(x=matrix(0,100,9))
for (p in 1:100) {
  y=c(rnorm(n,mean=0,sd=1))
  for(i in 1:9){
    a=0
    for(j in 1:n){
      if(y[j]<=t[i]){a=a+1}
    }
    x[p,i]=a/n
  }
}
x
w=c(rep(true[1],100),rep(true[2],100),rep(true[3],100),rep(true[4],100),rep(true[5],100),rep(true[6],100),rep(true[7],100),rep(true[8],100),rep(true[9],100))
m=matrix(w,nrow = 100,ncol = 9,byrow = FALSE)
(e=x-m)
#为了画图,处理数据形式
E=as.vector(e)
T=c(rep(t[1],100),rep(t[2],100),rep(t[3],100),rep(t[4],100),rep(t[5],100),rep(t[6],100),rep(t[7],100),rep(t[8],100),rep(t[9],100))
(df=cbind(E,T))
(df=data.frame(df))#矩阵变为数据框,画图要求
#画图
library(ggplot2)
ggplot(df,aes(T,E,group=T))+geom_boxplot()


#n=1000时,重复100次
n=1000
(x=matrix(0,100,9))
for (p in 1:100) {
  y=c(rnorm(n,mean=0,sd=1))
  for(i in 1:9){
    a=0
    for(j in 1:n){
      if(y[j]<=t[i]){a=a+1}
    }
    x[p,i]=a/n
  }
}
x
w=c(rep(true[1],100),rep(true[2],100),rep(true[3],100),rep(true[4],100),rep(true[5],100),rep(true[6],100),rep(true[7],100),rep(true[8],100),rep(true[9],100))
m=matrix(w,nrow = 100,ncol = 9,byrow = FALSE)
(e=x-m)
#为了画图,处理数据形式
E=as.vector(e)
T=c(rep(t[1],100),rep(t[2],100),rep(t[3],100),rep(t[4],100),rep(t[5],100),rep(t[6],100),rep(t[7],100),rep(t[8],100),rep(t[9],100))
(df=cbind(E,T))
(df=data.frame(df))#矩阵变为数据框,画图要求
#画图
library(ggplot2)
ggplot(df,aes(T,E,group=T))+geom_boxplot()

#n=10000时,重复100次
n=10000
(x=matrix(0,100,9))
for (p in 1:100) {
  y=c(rnorm(n,mean=0,sd=1))
  for(i in 1:9){
    a=0
    for(j in 1:n){
      if(y[j]<=t[i]){a=a+1}
    }
    x[p,i]=a/n
  }
}
x
w=c(rep(true[1],100),rep(true[2],100),rep(true[3],100),rep(true[4],100),rep(true[5],100),rep(true[6],100),rep(true[7],100),rep(true[8],100),rep(true[9],100))
m=matrix(w,nrow = 100,ncol = 9,byrow = FALSE)
(e=x-m)
#为了画图,处理数据形式
E=as.vector(e)
T=c(rep(t[1],100),rep(t[2],100),rep(t[3],100),rep(t[4],100),rep(t[5],100),rep(t[6],100),rep(t[7],100),rep(t[8],100),rep(t[9],100))
(df=cbind(E,T))
(df=data.frame(df))#矩阵变为数据框,画图要求
#画图
library(ggplot2)
ggplot(df,aes(T,E,group=T))+geom_boxplot()
