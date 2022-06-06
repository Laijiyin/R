#输入路径矩阵
a <- c(0,4,15,0,7,0,28)
b <- c(4,0,9,0,0,0,0)
c <- c(15,9,0,25,5,0,0)
d <- c(0,0,25,0,32,16,12)
e <- c(7,0,5,32,0,0,30)
f <- c(0,0,0,16,0,0,20)
g <- c(28,0,0,12,30,20,0)
W1 <- rbind(a,b,c,d,e,f,g)
colnames(W1) <- c("a","b","c","d","e","f","g")
######初始化######
node <- c("a","b","c","d","e","f","g")
#将上三角以及为零的数设为NA,方便排序
W1[which(W1 == 0)] <- NA
W1[upper.tri(W1)] <- NA
Sort_W1<-order(W1)[1:length(sort(W1))]
S<-arrayInd(Sort_W1[],dim(W1))
#s为生成的树，num为权值
s<-c()
num<-0
##################
#采用并查集的方法来判断是否为圈
#设置所有父结点为零
fat<-c(rep(0,7))
#并查集
for(i in 1:nrow(S)){
  if(fat[S[i,1]] == 0||fat[S[i,2]] ==0){
    if(length(which(fat[S[i,]]==0))==1){
      fat[S[i,]]<-fat[S[i,which(fat[S[i,]]!=0)]]
      #将满足条件的点以及权值赋值给对应变量
      s<-c(s,paste(node[S[i,]],collapse = "-"))
      num<-num+W1[S[i,1],S[i,2]]
       } else {fat[S[i,]]<-S[i,1]
    #将满足条件的点以及权值赋值给对应变量
    s<-c(s,paste(node[S[i,]],collapse = "-"))
    num<-num+W1[S[i,1],S[i,2]]}
     } else if(fat[S[i,1]]==fat[S[i,2]]){
       #父结点一样的不做处理
       }else {fat[which(fat[]==fat[S[i,2]])]<-fat[S[i,1]]
        #将满足条件的点以及权值赋值给对应变量
        s<-c(s,paste(node[S[i,]],collapse ="-"))
        num<-num+W1[S[i,1],S[i,2]]}
}
#输出为txt格式
sink("C:\\Users\\Matcha\\Desktop\\最小生成树.txt")
cat("最小生成树为：",s," 权值为：",num)
sink()