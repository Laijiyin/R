#贪心算法
Cnn <- function(x){
  b <- 0
  D1 <- D
  a <- sample(1:nrow(D),1)
  c <- a
  D1[,a] <- NA
  for(i in 1:(nrow(D)-1)){
    b <- b+min(D1[a,],na.rm = T)
    a <- which.min(D1[a,])
    D1[,a] <- NA
  }
  b <- b+D[a,c]
  b
}
#路径构建函数
route<-function(D,alpha,beta,Eta,Tau){
  T<-matrix(0,m,(nrow(D)+1))
  for(i in 1:m){
    city <- c(1:nrow(D))
    #随机选择一个城市作为其出发城市
    a <- sample(city,1)
    C<-c()
    C<-a
    #还未访问的城市
    city <- city[-a]
    while(length(city)!=0){
      #计算城市间转移概率
      P <- c()
      for(k in 1:length(city)){
        P[k] <- Tau[a,city[k]]^alpha*Eta[a,city[k]]^beta
      }
      P <- P/sum(P)
      #赌轮选择城市
      z <- 0
      r<-runif(1,0,1)
      for(j in 1:length(city)){
        z <- z+P[j]
        if(r<=z){
          a<-city[j]
          C<-c(C,a)
          city<-city[-j]
          break
        }
      }
    }
    T[i,]<-c(C,C[1])
  }
  T
}
#路径长度函数
xroute<-function(m,T){
  L<-c()
  for(i in 1:m){
    L[i]<-0
    for(j in 1:(ncol(T)-1))
      L[i]<-L[i]+D[T[i,j],T[i,j+1]]
  }
  L
}
#信息素更新函数
update<-function(Tau,T){
  t<-matrix(0,nrow(Tau),ncol(Tau))
  for(i in 1:m){
    for(j in 1:(ncol(T)-1))
      t[T[i,j],T[i,j+1]]<-t[T[i,j],T[i,j+1]]+(1/L[i])
  }
  Tau<-(1-rho)*Tau
  for(i  in 1:nrow(Tau)){
    for(j in 1:ncol(Tau)){
      Tau[i,j]<-Tau[i,j]+t[i,j]
    }
  }
  Tau
}

D <- matrix(c(NA,3,1,2,3,NA,5,4,1,5,NA,2,2,4,2,NA),4,4)
colnames(D) <- c("A","B","C","D")
rownames(D) <- c("A","B","C","D")

m <- 3                #蚂蚁数量
alpha <- 1            #信息素重要程度因子
beta <- 2             #启发函数重要程度因子
rho <- 0.5            #信息素挥发因子
Eta <- 1/D            #启发式信息

#使用贪心算法得到初始化信息素Tau0
Tau0 <- m/Cnn(D)
Tau <- matrix(Tau0,4,4)  #信息素矩阵

for(i in 1:100){
#3只蚂蚁路径构建
T<-route(D,alpha,beta,Eta,Tau)
#3只蚂蚁的路径长度
L<-xroute(m,T)
#信息素更新
Tau<-update(Tau,T)
}
R<-c("A","B","C","D")
cat(c("最短路径是：",R[T[which.min(L),]],"路径长度为：",min(L)))
