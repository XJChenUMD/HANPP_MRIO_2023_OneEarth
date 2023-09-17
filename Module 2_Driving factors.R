#Driving factors of cHANPP
#Xiangjie Chen @GEOG, UMD
#xjchen@umd.edu


HANPP_est <- c("_Baseline","_LowEstimate","_HighEstimate")
for (k in 1:length(HANPP_est)) {
  #----------------------------------------------------
  for(yy in 1:1){
    if(yy==1) YEAR<-c(2004,2017)
    # if(yy==2) YEAR<-c(2004,2007)
    # if(yy==3) YEAR<-c(2007,2011)
    # if(yy==4) YEAR<-c(2011,2014)
    
    t0<-Sys.time()
    for(i in 1:length(YEAR)){
      print(str_c("Begin-- Year ",HANPP_est[k],YEAR[i]))
      #Read MRIO data
      flnm<-str_c(pathinc,"/",YEAR[i],"GTAP Deflated 141reg65sec(DD)_Aug2023.RData");load(flnm)
      #(AX,X,VA,FD,A,B,FD_reg,G,N,GN,regnam,secnam,regsecnam)
      FD_reg<-FD_reg[,-142]
      GTAP_NPP<-read.csv(str_c(pathdata,"/HANPP Satellite/20230829_HANPP update/",
                               YEAR[i],"_GTAP.HANPP141reg65sec(byArea)",HANPP_est[k],".csv"),header=T)
      
      GTAP_NPP<-as.matrix(GTAP_NPP) 
      GTAP_NPP[is.na(GTAP_NPP)] <- 0
      #uniform the national order in HANPP satellite and MRIO
      NPP <- array(0,dim = dim(GTAP_NPP))
      for (x in 1:141) {
        NPP[,x] <- GTAP_NPP[,which(colnames(GTAP_NPP) %in% regnam[x])] 
      }
      colnames(NPP) <- regnam;rownames(NPP) <- secnam
      NPP[is.na(NPP)] <- 0
      
      FD_D <- array(0,dim = dim(FD_reg))#Local and foreign demand str/scale
      for(m in 1:length(regnam)){
        p=1+(m-1)*N;q=N+(m-1)*N
        FD_D[p:q,m]<-FD_reg[p:q,m]
      }
      FD_F <- FD_reg-FD_D
      FD_D_lev <- diag(colSums(FD_D));FD_F_lev <- diag(colSums(FD_F))
      FD_D_str <- t(t(FD_D)/colSums(FD_D));FD_F_str <- t(t(FD_F)/colSums(FD_F))
      FD_D_str[is.nan(FD_D_str)] <- 0;FD_F_str[is.nan(FD_F_str)] <- 0
      
      dim(NPP)<-c(141*65,1);NPP<-as.vector(NPP)
      w<-NPP/X;w[is.na(w)] <- 0;w[is.nan(w)] <- 0
      
      if (i==1){
        FD_reg0<-FD_reg;FD_D_lev0 <- FD_D_lev;FD_D_str0 <- FD_D_str
        FD_F_lev0 <- FD_F_lev;FD_F_str0 <- FD_F_str;w0<-w;B0<-B
        X0 <- X
      }else{
        FD_reg1<-FD_reg;FD_D_lev1 <- FD_D_lev;FD_D_str1 <- FD_D_str
        FD_F_lev1 <- FD_F_lev;FD_F_str1 <- FD_F_str;w1<-w;B1<-B
        X1 <- X
      }
    }
    rm(AX,X,VA,FD,A,B,FD_reg);gc()
    
    
    load(str_c(pathout,"/","All year Footprint",HANPP_est[k],".Rdata"))
    Chg_Con.NPP<-Con.NPP[,which(year %in% YEAR[2])]-Con.NPP[,which(year %in% YEAR[1])]
    SDA_Con.NPP<-array(0,dim = c(length(regnam),9))
    rownames(SDA_Con.NPP)<-regnam
    colnames(SDA_Con.NPP)<-c("Changes in cHANPP","Domestic HANPP intensity","Foreign HANPP intensity",
                             "Domestic production structure","Foreign production structure",
                             "Consumption patterns for domestic products","Consumption patterns for foreign products",
                             "Consumption level for domestic products","Consumption level for foreign products")
    
    SDA_Con.NPP[,1]<-Chg_Con.NPP
    
    #Method1: Two polar average decomposition
    #---------------------------------------
    w_effect <- 0.5*(diag(w1)-diag(w0))%*%(B0%*%FD_reg0+B1%*%FD_reg1)
    B_effect <- 0.5*(diag(w0)%*%((B1-B0)%*%FD_reg1)+diag(w1)%*%((B1-B0)%*%FD_reg0))
    
    w_effect_G<- array(0,dim=c(G,G));B_effect_G<- array(0,dim=c(G,G))
    
    for(m in 1:length(regnam)){
      p=1+(m-1)*N;q=N+(m-1)*N
      w_effect_G[m,]<-colSums(w_effect[p:q,])
      B_effect_G[m,]<-colSums(B_effect[p:q,])
    }
    
    SDA_Con.NPP[,2] <- diag(w_effect_G)[1:G]#domestic intensity effect
    SDA_Con.NPP[,3] <- colSums(w_effect_G)[1:G]-diag(w_effect_G)[1:G]#foreign intensity effect
    SDA_Con.NPP[,4] <- diag(B_effect_G)[1:G]#domestic production structure effect
    SDA_Con.NPP[,5] <- colSums(B_effect_G)[1:G]-diag(B_effect_G)[1:G]#foreign production structure effect
    
    F_Str_effect_d <- 0.5*(diag(w0)%*%(B0%*%((FD_D_str1-FD_D_str0)%*%FD_D_lev1))+
                             diag(w1)%*%(B1%*%((FD_D_str1-FD_D_str0)%*%FD_D_lev0)))
    F_Str_effect_f <- 0.5*(diag(w0)%*%(B0%*%((FD_F_str1-FD_F_str0)%*%FD_F_lev1))+
                             diag(w1)%*%(B1%*%((FD_F_str1-FD_F_str0)%*%FD_F_lev0)))
    
    SDA_Con.NPP[,6] <- colSums(F_Str_effect_d)[1:G]
    SDA_Con.NPP[,7] <- colSums(F_Str_effect_f)[1:G]
    
    F_Lev_effect_d <- 0.5*(diag(w1)%*%((B1%*%FD_D_str1)%*%(FD_D_lev1-FD_D_lev0))+
                             diag(w0)%*%((B0%*%FD_D_str0)%*%(FD_D_lev1-FD_D_lev0)))
    F_Lev_effect_f <- 0.5*(diag(w1)%*%((B1%*%FD_F_str1)%*%(FD_F_lev1-FD_F_lev0))+
                             diag(w0)%*%((B0%*%FD_F_str0)%*%(FD_F_lev1-FD_F_lev0)))
    
    SDA_Con.NPP[,8] <- colSums(F_Lev_effect_d)[1:G]
    SDA_Con.NPP[,9] <- colSums(F_Lev_effect_f)[1:G]
    
    SDA_Con.NPP_TP <- SDA_Con.NPP
    #----------------------------------------------------
    
    #Method2: All decomposition
    #----------------------------------------------------
    w_effect <- (1/4)*(diag(w1)-diag(w0))%*%(B0%*%FD_reg0+B1%*%FD_reg1)+
      (1/12)*(diag(w1)-diag(w0))%*%(B0%*%(FD_D_str0%*%FD_D_lev1+FD_F_str0%*%FD_F_lev1)
                                    +B0%*%(FD_D_str1%*%FD_D_lev0+FD_F_str1%*%FD_F_lev0)
                                    +B1%*%FD_reg0
                                    +B0%*%FD_reg1
                                    +B1%*%(FD_D_str1%*%FD_D_lev0+FD_F_str1%*%FD_F_lev0)
                                    +B1%*%(FD_D_str0%*%FD_D_lev1+FD_F_str0%*%FD_F_lev1))
    
    B_effect <- (1/4)*(diag(w0)%*%((B1-B0)%*%FD_reg0)+diag(w1)%*%((B1-B0)%*%FD_reg1))+
      (1/12)*(diag(w0)%*%((B1-B0)%*%(FD_D_str0%*%FD_D_lev1+FD_F_str0%*%FD_F_lev1))
              +diag(w0)%*%((B1-B0)%*%(FD_D_str1%*%FD_D_lev0+FD_F_str1%*%FD_F_lev0))
              +diag(w1)%*%((B1-B0)%*%FD_reg0)
              +diag(w0)%*%((B1-B0)%*%FD_reg1)
              +diag(w1)%*%((B1-B0)%*%(FD_D_str1%*%FD_D_lev0+FD_F_str1%*%FD_F_lev0))
              +diag(w1)%*%((B1-B0)%*%(FD_D_str0%*%FD_D_lev1+FD_F_str0%*%FD_F_lev1)))
    
    w_effect_G<- array(0,dim=c(G,G));B_effect_G<- array(0,dim=c(G,G))
    
    for(m in 1:length(regnam)){
      p=1+(m-1)*N;q=N+(m-1)*N
      w_effect_G[m,]<-colSums(w_effect[p:q,])
      B_effect_G[m,]<-colSums(B_effect[p:q,])
    }
    
    SDA_Con.NPP[,2] <- diag(w_effect_G)[1:G]#domestic intensity effect
    SDA_Con.NPP[,3] <- colSums(w_effect_G)[1:G]-diag(w_effect_G)[1:G]#foreign intensity effect
    SDA_Con.NPP[,4] <- diag(B_effect_G)[1:G]#domestic production structure effect
    SDA_Con.NPP[,5] <- colSums(B_effect_G)[1:G]-diag(B_effect_G)[1:G]#foreign production structure effect
    
    F_Str_effect_d <- (1/4)*(diag(w0)%*%(B0%*%(FD_D_str1-FD_D_str0)%*%FD_D_lev0)+
                               diag(w1)%*%(B1%*%(FD_D_str1-FD_D_str0)%*%FD_D_lev1))+
      (1/12)*(diag(w0)%*%(B0%*%(FD_D_str1-FD_D_str0)%*%FD_D_lev1)
              +diag(w0)%*%(B1%*%(FD_D_str1-FD_D_str0)%*%FD_D_lev0)
              +diag(w1)%*%(B0%*%(FD_D_str1-FD_D_str0)%*%FD_D_lev0)
              +diag(w0)%*%(B1%*%(FD_D_str1-FD_D_str0)%*%FD_D_lev1)
              +diag(w1)%*%(B1%*%(FD_D_str1-FD_D_str0)%*%FD_D_lev0)
              +diag(w1)%*%(B0%*%(FD_D_str1-FD_D_str0)%*%FD_D_lev1))
    
    F_Str_effect_f <- (1/4)*(diag(w0)%*%(B0%*%(FD_F_str1-FD_F_str0)%*%FD_F_lev0)+
                               diag(w1)%*%(B1%*%(FD_F_str1-FD_F_str0)%*%FD_F_lev1))+
      (1/12)*(diag(w0)%*%(B0%*%(FD_F_str1-FD_F_str0)%*%FD_F_lev1)
              +diag(w0)%*%(B1%*%(FD_F_str1-FD_F_str0)%*%FD_F_lev0)
              +diag(w1)%*%(B0%*%(FD_F_str1-FD_F_str0)%*%FD_F_lev0)
              +diag(w0)%*%(B1%*%(FD_F_str1-FD_F_str0)%*%FD_F_lev1)
              +diag(w1)%*%(B1%*%(FD_F_str1-FD_F_str0)%*%FD_F_lev0)
              +diag(w1)%*%(B0%*%(FD_F_str1-FD_F_str0)%*%FD_F_lev1))
    
    SDA_Con.NPP[,6] <- colSums(F_Str_effect_d)[1:G]
    SDA_Con.NPP[,7] <- colSums(F_Str_effect_f)[1:G]
    
    F_Lev_effect_d <- (1/4)*(diag(w0)%*%((B0%*%FD_D_str0)%*%(FD_D_lev1-FD_D_lev0))+
                               diag(w1)%*%((B1%*%FD_D_str1)%*%(FD_D_lev1-FD_D_lev0)))+
      (1/12)*(diag(w0)%*%((B0%*%FD_D_str1)%*%(FD_D_lev1-FD_D_lev0))
              +diag(w0)%*%((B1%*%FD_D_str0)%*%(FD_D_lev1-FD_D_lev0))
              +diag(w1)%*%((B0%*%FD_D_str0)%*%(FD_D_lev1-FD_D_lev0))
              +diag(w0)%*%((B1%*%FD_D_str1)%*%(FD_D_lev1-FD_D_lev0))
              +diag(w1)%*%((B1%*%FD_D_str0)%*%(FD_D_lev1-FD_D_lev0))
              +diag(w1)%*%((B0%*%FD_D_str1)%*%(FD_D_lev1-FD_D_lev0)))
    
    F_Lev_effect_f <- (1/4)*(diag(w0)%*%((B0%*%FD_F_str0)%*%(FD_F_lev1-FD_F_lev0))+
                               diag(w1)%*%((B1%*%FD_F_str1)%*%(FD_F_lev1-FD_F_lev0)))+
      (1/12)*(diag(w0)%*%((B0%*%FD_F_str1)%*%(FD_F_lev1-FD_F_lev0))
              +diag(w0)%*%((B1%*%FD_F_str0)%*%(FD_F_lev1-FD_F_lev0))
              +diag(w1)%*%((B0%*%FD_F_str0)%*%(FD_F_lev1-FD_F_lev0))
              +diag(w0)%*%((B1%*%FD_F_str1)%*%(FD_F_lev1-FD_F_lev0))
              +diag(w1)%*%((B1%*%FD_F_str0)%*%(FD_F_lev1-FD_F_lev0))
              +diag(w1)%*%((B0%*%FD_F_str1)%*%(FD_F_lev1-FD_F_lev0)))
    
    SDA_Con.NPP[,8] <- colSums(F_Lev_effect_d)[1:G]
    SDA_Con.NPP[,9] <- colSums(F_Lev_effect_f)[1:G]
    
    SDA_Con.NPP_Full <- SDA_Con.NPP
    rm(B0,B1,FD_reg0,FD_reg1,w1,w0);gc()
    #----------------------------------------------------
    
    save(SDA_Con.NPP_Full,SDA_Con.NPP_TP,
         file = str_c(pathout,"/",YEAR[1],"-",YEAR[2],HANPP_est[k],"SDA_cHANPP.Rdata"))
    
    print(paste("Time cost --- Year", YEAR[1],"-",YEAR[2],HANPP_est[k],round(Sys.time( )-t0,2)))
  }
}
#----------------------------------------------------






