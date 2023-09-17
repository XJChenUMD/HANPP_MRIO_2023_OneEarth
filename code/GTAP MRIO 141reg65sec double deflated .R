#GTAP MRIOƽ��
#����Դ���п�Ժ��GTAP MRIOS������NAMAD�Ƶ��ó��Ĳ�ҵ�۸�ָ��
#������RAS����
#ʱ�䣺revised on 2019-08-04 based on 2019-06-15
#�޸�˵��������˫��ƽ����
#���ߣ������
#�����ʽ��R�汾��.csv�汾


#��һ�����趨·�������������
setwd("D:\\�����ļ�\\C_Database\\GTAP 2004��2007��2011��2014")

path<-getwd( )
pathdata<-paste(path,"/data",sep="")
pathinc<-paste(path,"/inc",sep="");dir.create(pathinc)
pathout<-paste(path,"/20230829+GTAP MRIOs deflated",sep="");dir.create(pathout)
year<-c(2004,2007,2011,2014,2017)

tt<-Sys.time()
for(i in 1:length(year)){
  t0<-Sys.time()
  print(paste("Begin--",year[i]))
  #��ȡͶ��������ݼ��۸�����
  if (year[i] != 2017){
    GTAP_MRIO<-read.csv(paste(pathdata,"/V10-MRIO-",year[i],"-Final(141reg65sec).csv", sep=""),header=T)
  }else{
    GTAP_MRIO<-read.csv("I:/�ҵ��ƶ�Ӳ��/IDB Project/Data/GTAP-V11/2017��ȫ��141����65����MRIO.csv",header = T)
  }
  
  MRIO<-GTAP_MRIO[,-1]
  MRIO<-apply(MRIO,2,as.numeric)
  
  #Basic elements
  AX<-MRIO[1:9165,1:9165]
  FD<-MRIO[1:9165,9166:9589]#���һ����VST,ÿ������������������
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
  
  Price0<-read.csv(paste(pathdata,"/NAMAD���ݿ�/Deflator for GTAP MRIO/out_20230704/",
                         year[i],"GTAP141reg65sec deflator.csv", sep=""),header=T)
  #���������۸�ָ���еĹ���˳��
  prinam <- rownames(Price0)
  Price0 <- t(apply(Price0,1,as.numeric))
  Price1 <- array(0,dim = dim(Price0))
  for (x in 1:141) {
    Price1[x,] <- Price0[which(prinam %in% regnam[x]),] 
  }
  Price <- t(Price1)
  dim(Price) <- c(1,141*65)
  Price <- as.vector(Price)
  
  print(paste(year[i],"�����ݵ����ʱ",round(Sys.time()-t0,3)))
  
  
  #��Ҫ���д��������ݾ���������м���������ԭʼ��FD�����ϣ�����ֵ����Ͷ��/�ܲ�����
  #����һ��ԭ�򣬽���ƽ��ֻ��ԭʼ���ݽ��д�����֮����ʲô��Ҫ���������������ȥ����
  #�ڶ���������ƽ��
  print(paste("��ʼ˫��ƽ��������"))
  X_dd <- X/Price*100
  AX_dd <- AX/Price*100#���
  FD_dd <- FD/Price*100
  VA_dd <- X_dd- colSums(AX_dd)
  names(X_dd)<-regsecnam;names(VA_dd)<-regsecnam
  rownames(AX_dd)<-regsecnam;colnames(AX_dd)<-regsecnam
  rownames(FD_dd)<-regsecnam;colnames(FD_dd)<-fdregsec
  
  #����һЩ����Ԫ�ز�������ΪR.data��ʽ
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
  print(paste(year[i],"������ʱ��",round(Sys.time()-t0,3)))
}


#���Ĳ������������GDP����
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