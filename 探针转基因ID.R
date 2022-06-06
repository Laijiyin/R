#下载GEOquery包
if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
  BiocManager::install("GEOquery")}

#导入基因矩阵
expeSet <- read.table('GSE21815_series_matrix.txt',
                    sep = '\t',quote="",fill = T,
                    comment.char = "!",header = T)
rownames(expeSet) <- expeSet[,1]
expeSet <- expeSet[,-1]

#获取探针与基因之间的对应关系
library(GEOquery)
gpl <- getGEO('GPL6480', destdir=".")
ids <- Table(gpl)[,c(1,7)]

#过滤基因矩阵在对应平台没有的探针基因
ids_c1 <- paste("\"",ids$ID,"\"",sep = "")
expeSet <- expeSet[rownames(expeSet) %in% ids_c1,]

#过滤有多个探针的基因
ids <- ids[match(rownames(expeSet),ids_c1),]
tmp <- by(expeSet,
          ids$GENE_SYMBOL,
          function(x) rownames(x)[which.max(rowMeans(x))])
probes <- as.character(tmp)
expeSet <- expeSet[rownames(expeSet) %in% probes,]

#将基因命名
ids_c2 <- paste("\"",ids$ID,"\"",sep = "")
Name <- ids[match(rownames(expeSet),ids_c2),2]
rownames(expeSet) <- Name