#GTAP MRIO平减
#数据源：中科院的GTAP MRIOS，依据NAMAD推导得出的产业价格指数
#方法：RAS方法
#时间：revised on 2019-08-04 based on 2019-06-15
#修改说明：补充双重平减法
#作者：陈湘杰
#输出格式：R版本与.csv版本


#第一步：设定路径导入基本数据
setwd("D:\\科研文件\\C_Database\\GTAP 2004、2007、2011、2014")

path<-getwd( )
pathdata<-paste(path,"/data",sep="")
pathinc<-paste(path,"/inc",sep="");dir.create(pathinc)
pathout<-paste(path,"/20230829+GTAP MRIOs deflated",sep="");dir.create(pathout)
year<-c(2004,2007,2011,2014,2017)

tt<-Sys.time()
for(i in 1:length(year)){
  t0<-Sys.time()
  print(paste("Begin--",year[i]))
  #读取投入产出数据及价格数据
  if (year[i] != 2017){
    GTAP_MRIO<-read.csv(paste(pathdata,"/V10-MRIO-",year[i],"-Final(141reg65sec).csv", sep=""),header=T)
  }else{
    GTAP_MRIO<-read.csv("I:/我的云端硬盘/IDB Project/Data/GTAP-V11/2017年全球141区域65部门MRIO.csv",header = T)
  }
  
  MRIO<-GTAP_MRIO[,-1]
  MRIO<-apply(MRIO,2,as.numeric)
  
  #Basic elements
  AX<-MRIO[1:9165,1:9165]
  FD<-MRIO[1:9165,9166:9589]#最后一列是VST,每个国家三类最终需求
  VA<-colSums(MRIO[9166:9170,1:9165])
  X<-rowSums(AX)+rowSums(FD)
  
  #reg and sec name
  regsecnam<-as.character(GTAP_MRIO[1:9165,1])
  regnam<-unique(substr(regsecnam,5,7))
  if (year[i] == 2017) regnam[90] <- 'xer'
  secnam<-unique(substr(regsecnam,1,3))
  fdregsec<-as.character(GTAP_MRIO[1,9166:9589])
  G<-length(regnam);N<-length(secnam);GN<-G*N
  rm(GTAP_MRIO,MRIO);gc()
  
  Price0<-read.csv(paste(pathdata,"/NAMAD数据库/Deflator for GTAP MRIO/out_20230704/",
                         year[i],"GTAP141reg65sec deflator.csv", sep=""),header=T)
  #重新整理价格指数中的国家顺序
  prinam <- rownames(Price0)
  Price0 <- t(apply(Price0,1,as.numeric))
  Price1 <- array(0,dim = dim(Price0))
  for (x in 1:141) {
    Price1[x,] <- Price0[which(prinam %in% regnam[x]),] 
  }
  Price <- t(Price1)
  dim(Price) <- c(1,141*65)
  Price <- as.vector(Price)
  
  print(paste(year[i],"年数据导入耗时",round(Sys.time()-t0,3)))
  
  
  #需要进行处理的数据具体包括：中间流量矩阵；原始的FD矩阵老；增加值；总投入/总产出。
  #掌握一个原则，进行平减只对原始数据进行处理，之后有什么需要做的在这个基础上去做。
  #第二步：进行平减
  print(paste("开始双重平减法测算"))
  X_dd <- X/Price*100
  AX_dd <- AX/Price*100#检查
  FD_dd <- FD/Price*100
  VA_dd <- X_dd- colSums(AX_dd)
  names(X_dd)<-regsecnam;names(VA_dd)<-regsecnam
  rownames(AX_dd)<-regsecnam;colnames(AX_dd)<-regsecnam
  rownames(FD_dd)<-regsecnam;colnames(FD_dd)<-fdregsec
  
  #测算一些基本元素并输出结果为R.data格式
  AX<-AX_dd;X<-X_dd;VA<-VA_dd;FD<-FD_dd
  A<-t(t(AX)/X)
  A[is.na(A)]<-0;A[A==Inf]<-0
  II<-diag(141*65)
  B<-solve(II-A)
  B[is.na(B)]<-0; B[B==Inf]<-0
  FD_reg<-array(0,dim=c(9165,142))
  for( j in 1:length(regnam)){
    m=1+(j-1)*N;n=j*N
    for( q in 1:length(regnam)){
      z=1+(q-1)*3;y=q*3
      FD_reg[m:n,q]<-rowSums(FD[m:n,z:y])
    }
  }
  FD_reg[,142]<-FD[,y+1]
  flnm<-paste(pathinc,"/",year[i],"GTAP Deflated 141reg65sec(DD)_Aug2023.RData",sep="")
  save(AX,X,VA,FD,A,B,FD_reg,G,N,GN,regnam,secnam,regsecnam,file=flnm)
  print(paste(year[i],"年计算耗时：",round(Sys.time()-t0,3)))
}


#第四步：检验各国的GDP数据
GDP<-array(0,dim=c(141,length(year)))
for(i in 1:length(year)){
  flnm<-paste(pathinc,"/",year[i],"GTAP Deflated 141reg65sec(DD)_Aug2023.RData",sep="");load(flnm)
  for (k in 1:length(regnam)) {
    m=65*(k-1)+1;n=65*k
    GDP[k,i]<-sum(VA[m:n])/100
  }
}
colnames(GDP)<-paste(year,"DD");rownames(GDP)<-regnam
fnm = paste( pathout,"/","Deflated GDP141reg65sec five years(100 million)_Aug2023.csv",sep="" )
write.table(GDP, file=fnm, sep=",")
