n<-c(5,6,7,10)
k<-c(1,1,2,4)
num<-18
r<-c()
for(a in k[1]:n[1]){
  for(b in k[2]:n[2]){
    for(c in k[3]:n[3]){
      for(d in k[4]:n[4]){
        if(a+b+c+d==num){
          r=c(r,c(a,b,c,d))
        }
      }
    }
  }
}
r<-matrix(r,,4,byrow = T)
colnames(r)<-c("¼×","ÒÒ","±û","¶¡")
write.table(r,"C:\\Users\\Public\\Desktop\\create.txt",row.names = F)
