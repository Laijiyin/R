a<-c(0,4,2,Inf,Inf,Inf)
b<-c(4,0,1,5,Inf,Inf)
c<-c(2,1,0,8,10,Inf)
d<-c(Inf,5,8,0,2,6)
e<-c(Inf,Inf,10,2,0,3)
z<-c(Inf,Inf,Inf,6,3,0)
P<-matrix(c(a,b,c,d,e,z),6,6)

Dijkstra<-function(x,n){
  D<-x[n,]
  D1<-x[n,]
  D1[n]<-Inf
  u<-c(n)
  for(i in 0:ncol(x)-1){
  u<-c(u,which.min(D1))
  sum_D<-0
  t<-c()
  #选择下一结点之前各个结点的距离
  for(k in 1:length(u)-1){
    t<-c(t,P[u[k],u[k+1]])
    sum_D<-sum(t)}
  for(j in 1:ncol(x)){
    #判断更新距离是否小于已知距离
    if(D[j]>sum_D+x[u[length(u)],j]){
      D[j]<-sum_D+x[u[length(u)],j]
      D1[j]<-sum_D+x[u[length(u)],j]} 
  }
   D1[u[length(u)]]<-Inf 
  }
  return(D)
}
dance<-Dijkstra(P,1)

sink("C:\\Users\\Matcha\\Desktop\\最短路径.txt")
cat("a点到a,b,c,d,e,z的最短路径为：",dance)
sink()