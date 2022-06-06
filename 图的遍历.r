#创建邻接矩阵
q <- matrix(c(0,1,1,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,
             0,1,1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,
             0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,1,0,0,1,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),9,9,byrow = T)

#############深度优先遍历###############
#初始化
q1 <- q
q[,1] <- 0
list1 <- c(1)
next1 <- 1
tree1 <- c(1)

while( next1 != 9){
#判断目前所在根结点是否可以遍历到其他结点
  if(length(which(q1[list1[length(list1)],] == 1)) != 0){
#从当前根结点遍历到其他结点
#生成树
    a<-sample(length(which(q1[list1[length(list1)],] == 1)),1)
    tree1 <- c(tree1,which(q1[list1[length(list1)],] == 1)[a])
#更新邻接矩阵  
    q1[,tree1[length(tree1)]] <- 0
    q1[list1[length(list1)],tree1[length(tree1)]] <- 0
#更新list1
    list1 <- c(list1,tree1[length(tree1)])
#更新next1
    next1 <- next1+1
  } else {list1 <- list1[-length(list1)]}
}
########################################

#############广义优先遍历###############
#初始化
q2 <- q
q[,1] <- 0
list2 <- c(1)
next2 <- 1
tree2 <- c(1)

while( next2 != 9){
if(length(which(q2[list2[1],] == 1)) != 0){
a<-which(q2[list2[1],] == 1)
#从当前根结点遍历到其他结点
#生成树
tree2 <- c(tree2,a)
#更新邻接矩阵 
q2[,a] <- 0
#更新list2
list2 <- c(list2,a)
list2 <- list2[-1]
#更新next2
next2 <- next2+length(a)
} else {list2 <- list2[-1]}
}
########################################
#输出为txt格式
sink("C:\\Users\\Matcha\\Desktop\\图的遍历.txt")
cat("深度优先遍历为：",tree1,"   广义优先遍历为：",tree2)
sink()

