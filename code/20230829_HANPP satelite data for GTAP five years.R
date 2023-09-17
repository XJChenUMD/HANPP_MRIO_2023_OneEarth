#Theme:HANPP satelite data for GTAP
#Author:Xiangjie Chen
#Date:2019-03-23  revised on 2019-05-28
#update for new HANPP accounting on 2023-07-02

##Step1: read FAO crop data and match it with agricultural sectors in GTAP
#----------------------------------------------------------------
setwd("D:\\科研文件\\C_Database\\GTAP 2004、2007、2011、2014\\data")
path<-getwd()
pathdata<-paste(path,"\\HANPP Satellite",sep = "")
pathout<-paste(path,"\\HANPP Satellite\\20230829_HANPP update",sep = "");dir.create(pathout)
Production.Crop<-read.csv(paste(pathdata,"/FAO_Corp_Production.csv",sep = ""),
                     header = T,sep = ",")
Area.Crop<-read.csv(paste(pathdata,"/FAO_Corp_Area.csv",sep = ""),
                          header = T,sep = ",")
#输出FAO Crop作物种类、形成GTAP与FAO农产品的对照表
FAO.Crop<-cbind(unique(Production.Crop[,3]),unique(as.character(Production.Crop[,4])))
colnames(FAO.Crop)<-colnames(Production.Crop)[3:4]
fnm = paste( pathout,"/FAO.Crop.class.csv",sep="" )
write.table(FAO.Crop, file=fnm, sep=",",row.names = F)
#注意：其中的四位数代码是汇总类别，不可纳入计算。 整理后一共168种农产品
#依据UNCTAD的CPCv.1以及GTAP网站的GSC2 - CPC对照表手动生成FAO-GSC2的对照表。存储于：FAOtoGTAP.csv
#---------------------------------------------------------------------

##Step2:将各国农产品数据汇总至八个GTAP农业部门。
#---------------------------------------------------------------------
FAOtoGTAP<-read.csv(paste(pathdata,"/FAOtoGTAP.csv",sep = ""),
                    header = T,sep = ",")
#数据预处理：（1）将空缺值负值为0；（2）删去四位码数据
Production.Crop[is.na(Production.Crop)]<-0
Area.Crop[is.na(Area.Crop)]<-0
a<-Production.Crop[,3]>1000;Production.Crop<-Production.Crop[-which(a %in% T),]
a<-Area.Crop[,3]>1000;Area.Crop<-Area.Crop[-which(a %in% T),]
#定义基本变量
regnam<-unique(as.character(Production.Crop[,2]))
secnam<-unique(as.character(FAOtoGTAP[,6]))
year<-c(1961:2017)
GTAP.Production<-array(0,dim = c(length(regnam),length(secnam)))
GTAP.Area<-array(0,dim = c(length(regnam),length(secnam)))
colnames(GTAP.Production)<-secnam;rownames(GTAP.Production)<-regnam
dimnames(GTAP.Area)<-dimnames(GTAP.Production)
#开始测算
for (j in 1:length(year)) {#所有年份循环处理，8:64列
  print(paste("the begining of:", year[j]))
  t0<-Sys.time()
  k=j+7
  for(i in 1:length(regnam)){#国家循环
    Country.Production<-Production.Crop[which(Production.Crop[,2] %in% regnam[i]),]
    Country.Production<-merge(Country.Production,FAOtoGTAP,by="Item.Code")
    Country.Area<-Area.Crop[which(Area.Crop[,2] %in% regnam[i]),]
    Country.Area<-merge(Country.Area,FAOtoGTAP,by="Item.Code")
    fac<-Country.Production$GTAP.Number
    GTAP.Production[i,as.numeric(rownames(rowsum(Country.Production[,k],fac))) ]<-rowsum(Country.Production[,k],fac)
    fac2<-Country.Area$GTAP.Number
    GTAP.Area[i,as.numeric(rownames(rowsum(Country.Area[,k],fac2)))]<-rowsum(Country.Area[,k],fac2)
  }
  #输出当年结果
  print(paste(year[j],"time cost", round(Sys.time()-t0,2)))
  fnm = paste( pathout,"/",year[j],"_GTAP.Production(tonnes).csv",sep="" );write.table(GTAP.Production, file=fnm, sep=",")
  fnm = paste( pathout,"/",year[j],"_GTAP.Area(ha).csv",sep="" );write.table(GTAP.Area, file=fnm, sep=",")
}
#FAO数据已经准备完毕，可以形成农业部门的HANPP分配系数
#--------------------------------------------------------------

##Step3:将HANPP以及NPP数据匹配至GTAP的农业（1:8）、畜牧业（9）、林业（13）
##Step4:将现有结果匹配值GTAP的国家产业维度中
#---------------------------------------------------------------------
GTAPregsec<-read.csv(paste(pathdata,"/GTAP10 regsec.csv",sep = ""),header = T,sep = ",")
DB_Corr<-read.csv(paste(pathdata,"/HANPP GTAP FAO地区名对照_230702.csv",sep = ""),header = T,sep = ",")
GTAPreg<-as.character(GTAPregsec[1:141,2]);GTAPREG<-as.character(GTAPregsec[1:141,3])
GTAPsec<-as.character(GTAPregsec[1:65,5])
GTAP_HANPP<-array(0,dim = c(length(GTAPsec),length(GTAPreg)))  
colnames(GTAP_HANPP)<-GTAPreg;rownames(GTAP_HANPP)<-GTAPsec
Str.Production<-array(0,dim = c(length(GTAPreg),length(secnam)))
Str.Area<-array(0,dim = c(length(GTAPreg),length(secnam)))
colnames(Str.Production)<-secnam;rownames(Str.Production)<-GTAPreg
dimnames(Str.Area)<-dimnames(Str.Production)

library(xlsx)
#对原始数据进行整理
YEAR<-c(2004,2007,2011,2014,2017)

# data<- read.xlsx2(paste(pathdata,"/HANPP_Version2023_Baseline_June19.xlsx",sep = ""), sheetIndex  = 1)[c(1:237),-c(2,3)]
data<- read.xlsx2(paste(pathdata,"/HANPP_Version2023_HighEstimate_July11.xlsx",sep = ""), sheetIndex  = 1)[c(1:237),-c(2,3)]
# data<- read.xlsx2(paste(pathdata,"/HANPP_Version2023_LowEstimate_July11.xlsx",sep = ""), sheetIndex  = 1)[c(1:237),-c(2,3)]

Boundary_global <- array(NA,dim = c(6,length(YEAR)))
rownames(Boundary_global) <- c("NPP","Boundary","HANPP","Remain","HANPPpercent","Remainpercent")
colnames(Boundary_global) <- YEAR
Boundary_global[1,] <- colSums(apply(data[,2:6],2,as.numeric))
Boundary_global[2,] <- Boundary_global[1,]*0.4
Boundary_global[3,] <- colSums(apply(data[,22:26],2,as.numeric))
Boundary_global[4,] <- Boundary_global[2,]-Boundary_global[3,]
Boundary_global[5,] <- Boundary_global[3,]/ Boundary_global[2,]
Boundary_global[6,] <- Boundary_global[4,]/ Boundary_global[2,]
# fnm = paste( pathout,"/Boundary_global_Baseline.csv",sep="" );write.table(Boundary_global, file=fnm, sep=",")
fnm = paste( pathout,"/Boundary_global_HighEstimate.csv",sep="" );write.table(Boundary_global, file=fnm, sep=",")
# fnm = paste( pathout,"/Boundary_global_LowEstimate.csv",sep="" );write.table(Boundary_global, file=fnm, sep=",")

#分年份结果输出
for (g in 1:length(YEAR)) {#年份循环
  Tar <- c("NAME_EN",
       paste(c("HANPP_TOTAL_","HANPP_Cropland_","HANPP_Grazing_","HANPP_Forestry_"),YEAR[g],sep = ""))
  X<-data[,which(colnames(data) %in% Tar)]
  colnames(X)<-c("region","Crop","Animal","Forest","Total")
  fnm = paste( pathout,"/",YEAR[g],"_HANPP(PgC·a).csv",sep="" );write.table(X, file=fnm, sep=",",row.names = F)

  for (s in 1:length(GTAPREG)) {
    #定位出待测区域的HANPP位置，处理林业和牧业
    a<-as.character(DB_Corr[which(DB_Corr[,2] %in% GTAPREG[s]),1])#HANPP国名
    b<-which(X[,1] %in% a)
    if(length(b)==1){#"Crop","Animal","Forest"
      NPP<-as.numeric(X[b,2:4])
    }else{
      NPP<-c(0,0,0)
      for (v in 1:length(b)) NPP<-NPP+as.numeric(X[b[v],2:4])
    }
    GTAP_HANPP[9,s]<-NPP[2]#"Animal"
    GTAP_HANPP[13,s]<-NPP[3]#"Forest"
    
    #定位出待测区域的FAO数据位置，处理八个农业部门
    c<-as.character(DB_Corr[which(DB_Corr[,2] %in% GTAPREG[s]),3])#FAO国名
    
     
    #2. 按面积
    #--------
    fnm = paste( pathout,"/",YEAR[g],"_GTAP.Area(ha).csv",sep="" )
    GTAP.Area<-read.csv(fnm,header = T,sep = ",")
    d<-which(rownames(GTAP.Area) %in% c)
    if(length(d)==1){#"Crop","Animal","Forest"
     Area<-GTAP.Area[d,]
    }else{
     Area<-colSums(GTAP.Area[d,])
    }
    GTAP_HANPP[1:8,s]<-unlist(NPP[1]*Area/sum(Area))#"Crop"
    Str.Area[s,]<-unlist(Area/sum(Area))
    #--------
  }
  GTAP_HANPP[is.na(GTAP_HANPP)] <- 0
  
  # fnm = paste( pathout,"/",YEAR[g],"_GTAP.HANPP141reg65sec(byArea)_Baseline.csv",sep="" );write.table(GTAP_HANPP, file=fnm, sep=",")
  # fnm = paste( pathout,"/",YEAR[g],"_Str.Area141reg65sec_Baseline.csv",sep="" );write.table(Str.Area, file=fnm, sep=",")
  fnm = paste( pathout,"/",YEAR[g],"_GTAP.HANPP141reg65sec(byArea)_HighEstimate.csv",sep="" );write.table(GTAP_HANPP, file=fnm, sep=",")
  fnm = paste( pathout,"/",YEAR[g],"_Str.Area141reg65sec_HighEstimate.csv",sep="" );write.table(Str.Area, file=fnm, sep=",")
  # fnm = paste( pathout,"/",YEAR[g],"_GTAP.HANPP141reg65sec(byArea)_LowEstimate.csv",sep="" );write.table(GTAP_HANPP, file=fnm, sep=",")
  # fnm = paste( pathout,"/",YEAR[g],"_Str.Area141reg65sec_LowEstimate.csv",sep="" );write.table(Str.Area, file=fnm, sep=",")
}
#--------------------------------------------------------------


