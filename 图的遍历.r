#�����ڽӾ���
q <- matrix(c(0,1,1,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,
             0,1,1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,
             0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,1,0,0,1,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),9,9,byrow = T)

#############������ȱ���###############
#��ʼ��
q1 <- q
q[,1] <- 0
list1 <- c(1)
next1 <- 1
tree1 <- c(1)

while( next1 != 9){
#�ж�Ŀǰ���ڸ�����Ƿ���Ա������������
  if(length(which(q1[list1[length(list1)],] == 1)) != 0){
#�ӵ�ǰ�����������������
#������
    a<-sample(length(which(q1[list1[length(list1)],] == 1)),1)
    tree1 <- c(tree1,which(q1[list1[length(list1)],] == 1)[a])
#�����ڽӾ���  
    q1[,tree1[length(tree1)]] <- 0
    q1[list1[length(list1)],tree1[length(tree1)]] <- 0
#����list1
    list1 <- c(list1,tree1[length(tree1)])
#����next1
    next1 <- next1+1
  } else {list1 <- list1[-length(list1)]}
}
########################################

#############�������ȱ���###############
#��ʼ��
q2 <- q
q[,1] <- 0
list2 <- c(1)
next2 <- 1
tree2 <- c(1)

while( next2 != 9){
if(length(which(q2[list2[1],] == 1)) != 0){
a<-which(q2[list2[1],] == 1)
#�ӵ�ǰ�����������������
#������
tree2 <- c(tree2,a)
#�����ڽӾ��� 
q2[,a] <- 0
#����list2
list2 <- c(list2,a)
list2 <- list2[-1]
#����next2
next2 <- next2+length(a)
} else {list2 <- list2[-1]}
}
########################################
#���Ϊtxt��ʽ
sink("C:\\Users\\Matcha\\Desktop\\ͼ�ı���.txt")
cat("������ȱ���Ϊ��",tree1,"   �������ȱ���Ϊ��",tree2)
sink()

