#HANPP footprint and projection under five SSP scenarios
#Xiangjie Chen @ GEOG, UMD
#xjchen@umd.edu

#Prepare the SSPs scenarios
#----------
Pop_projection <- read.csv(str_c(pathdata_SPP,"/SSPs Population projection.csv"))
GDP_projection <- read.csv(str_c(pathdata_SPP,"/SSPs GDP projection.csv"))
IncPara <- read.csv(str_c(pathdata_SPP,"/INCP_2014.csv"))
IncPara <- IncPara[,-c(1,143)]
IncPara <- IncPara/65
SSPs <- str_c("SSP",1:5)
Reg_conco <- read.csv(str_c(pathdata_SPP,"/Reg RCP SSP Concordances to GTAP.csv"))

flnm <- paste(pathinc,"/SSPs.RData",sep="")
save(Pop_projection,GDP_projection,IncPara,Reg_conco,file=flnm)
#----------

HANPP_est <- c("_Baseline","_LowEstimate","_HighEstimate")
GDP_reg <- read.csv(str_c(pathdata,"/Deflated GDP141reg65sec five years(100 million)_Aug2023.csv"),header=T)
Pop_reg <- read.csv(str_c(pathdata,"/GTAP population 141reg65sec five years_Aug2023.csv"),header=T)
Pop_reg <- apply(Pop_reg[,-1], 2, as.numeric)

for (k in 1:length(HANPP_est)) {
  #================
  print(str_c("Begin-- HANPP scenario: ",HANPP_est[k]))
  
  tt<-Sys.time()
  
  year<-c(2004,2007,2011,2014,2017)
  OUT<-array(0,dim=c(length(year),3))#HANPP, HANPP embodied in trade, Trade/HANPP
  Pro.NPP<-array(0,dim = c(141,length(year)))
  Con.NPP<-array(0,dim = c(141,length(year)))
  InOut.NPP<-array(0,dim = c(141,5))
  
  for(i in 1:length(year)){
    t0<-Sys.time()
    print(str_c("Begin-- Year ",year[i]))
    #Read MRIO data
    flnm<-str_c(pathinc,"/",year[i],"GTAP Deflated 141reg65sec(DD)_Aug2023.RData");load(flnm)
    #(AX,X,VA,FD,A,B,FD_reg,G,N,GN,regnam,secnam,regsecnam)
    FD_reg<-FD_reg[,-142]
    GTAP_NPP<-read.csv(str_c(pathdata,"/HANPP Satellite/20230829_HANPP update/",
                             year[i],"_GTAP.HANPP141reg65sec(byArea)",HANPP_est[k],".csv"),header=T)
    GTAP_NPP<-as.matrix(GTAP_NPP) 
    GTAP_NPP[is.na(GTAP_NPP)] <- 0
    #uniform the national order in HANPP satellite and MRIO
    NPP <- array(0,dim = dim(GTAP_NPP))
    for (x in 1:141) {
      NPP[,x] <- GTAP_NPP[,which(colnames(GTAP_NPP) %in% regnam[x])] 
    }
    colnames(NPP) <- regnam;rownames(NPP) <- secnam
    NPP[is.na(NPP)] <- 0
    
    #Production based HANPP
    Pro.NPP[,i]<-colSums(NPP)
    dim(NPP)<-c(141*65,1);NPP<-as.vector(NPP)
    w<-NPP/X;w[is.na(w)] <- 0;w[is.nan(w)] <- 0
    
    #HANPP tranfer matrix
    NPPinTrade<-array(0,dim=c(length(regnam),length(regnam)))
    Sec.Trade<-diag(w)%*%(B%*%FD_reg)#GN.GN*GN.GN*GN.G  foorptint
    wB <- w%*%B#sectoral embodied HANPP intensity: will be used for projection
    
    rownames(Sec.Trade)<-regsecnam;colnames(Sec.Trade)<-regnam
    
    for(m in 1:length(regnam)){
      p=1+(m-1)*65;q=65+(m-1)*65
      NPPinTrade[m,]<-colSums(Sec.Trade[p:q,])
    }
    rownames(NPPinTrade)<-regnam;colnames(NPPinTrade)<-regnam
    
    OUT[i,1]<-sum(NPP);OUT[i,2]<-sum(NPPinTrade)-sum(diag(NPPinTrade));OUT[i,3]<-OUT[i,2]/OUT[i,1]
    
    InOut.NPP[,1]<-colSums(NPPinTrade)-diag(NPPinTrade)#Inflow
    InOut.NPP[,2]<-rowSums(NPPinTrade)-diag(NPPinTrade)#Outflow
    InOut.NPP[,3]<-diag(NPPinTrade)#domestic
    InOut.NPP[,4]<-Pro.NPP[,i]#territory
    InOut.NPP[,5]<-colSums(NPPinTrade)#consumption
    colnames(InOut.NPP)<-c("Inflow","Outflow","Domestic","territory","consumpution");rownames(InOut.NPP)<-regnam

    #Consumption based HANPP
    Con.NPP[,i]<-colSums(NPPinTrade)
    
    save(wB,NPPinTrade,InOut.NPP,Sec.Trade,
         file = str_c(pathout,"/",year[i],"HANPP footprint",HANPP_est[k],".Rdata"))
    
    print(str_c("Time cost--Year ", round(Sys.time( )-t0,2)))
  }
  rownames(OUT)<-year;colnames(OUT)<-c("HANPP(Pg C/yr)","Trade(Pg C/yr)","Trade share(%)")
  rownames(Pro.NPP)<-regnam;colnames(Pro.NPP)<-year;dimnames(Con.NPP)<-dimnames(Pro.NPP)
  
  NetTradeNPP <- Con.NPP-Pro.NPP
  
  #Intensity
  Con.NPP.perUSD <- Con.NPP/GDP_reg*(10^4);Con.NPP.perCap <- Con.NPP/Pop_reg*(10^3)
  Pro.NPP.perUSD <- Pro.NPP/GDP_reg*(10^4);Pro.NPP.perCap <- Pro.NPP/Pop_reg*(10^3)
  #Original units: Pg/100 million；Pg/per cap; Transform to: Kg/USD,t/Percapita   Pg=10^15g
  
  save(OUT,Pro.NPP, Con.NPP, NetTradeNPP, Con.NPP.perUSD,Pro.NPP.perUSD,
       Con.NPP.perCap,Pro.NPP.perCap,year,
       file = str_c(pathout,"/","All year Footprint",HANPP_est[k],".Rdata"))
  #=====================
  
  
  #PART 2: HANPP projection
  #======================
  #sectoral eHANPP intensity projection
  #-----------
  load(str_c(pathout,"/",2004,"HANPP footprint",HANPP_est[k],".Rdata"));wB2004 <- wB
  load(str_c(pathout,"/",2007,"HANPP footprint",HANPP_est[k],".Rdata"));wB2007 <- wB
  load(str_c(pathout,"/",2011,"HANPP footprint",HANPP_est[k],".Rdata"));wB2011 <- wB
  load(str_c(pathout,"/",2014,"HANPP footprint",HANPP_est[k],".Rdata"));wB2014 <- wB
  load(str_c(pathout,"/",2017,"HANPP footprint",HANPP_est[k],".Rdata"));wB2017 <- wB
  load(str_c(pathout,"/","All year Footprint",HANPP_est[k],".Rdata"))
  
  HANPPint_Trend <- rep(0,length(wB))
  for (f in 1:length(wB)) {
    a <- log(c(wB2004[f],wB2007[f],wB2011[f],wB2014[f],wB2017[f]))
    a[is.infinite(a)] <- NA
    b <- c(2004,2007,2011,2014,2017)
    if(sum(is.na(a)) < 4){
      c <- summary(lm(a~b,data = data.frame(cbind(a,b)),na.action = na.omit))
      HANPPint_Trend[f] <- 1+(c$coefficients[2,1])
    }
  }
  HANPPint_Trend[is.na(HANPPint_Trend)] <- 0
  HANPPint_Trend[HANPPint_Trend < 0] <- 0
  #-----------
  
  #Final demand and HANPP projection
  #----------
  SSPs_year <- seq(2020,2100,5)
  flnm<-str_c(pathinc,"/2017GTAP Deflated 141reg65sec(DD)_Aug2023.RData");load(flnm)
  
  for (i in 1:length(SSPs)){
    t0 <- Sys.time()
    #SSP
    #-----------
    HANPP_Footprint_Project <- array(NA,dim = c(G,length(SSPs_year)+1))
    Pop_Project <- array(NA,dim = c(G,length(SSPs_year)+1))
    rownames(HANPP_Footprint_Project) <- regnam
    colnames(HANPP_Footprint_Project) <- c("2017",SSPs_year)
    dimnames(Pop_Project) <- dimnames(HANPP_Footprint_Project)
    GDP_Project <- Pop_Project
    HANPP_Footprint_Project[,1] <- Con.NPP[,5]
    Pop_Project[,1] <- Pop_reg[,5]
    GDP_Project[,1] <- GDP_reg[,5]/10
    HANPP_Footprint_Inten <- HANPP_Footprint_Project
    HANPP_Footprint_Pop <- HANPP_Footprint_Project
    HANPP_Footprint_Inc <- HANPP_Footprint_Project
    
    for (j in 1:length(SSPs_year)) {
      #Future cHANPP intensity
      #----------
      x <- wB*(HANPPint_Trend^(SSPs_year[j]-2017))
      x[x > wB] <- wB[x > wB]
      cHANPP_inten <- x
      #----------
      
      #Future population and GDP per capita
      Pop_projection %>% filter(Model == "OECD Env-Growth", Scenario == SSPs[i]) %>%
        select(Region,str_c("X",c(2015,SSPs_year[j]))) -> SSP_Pop
      GDP_projection %>% filter(Model == "OECD Env-Growth", Scenario == SSPs[i])  %>%
        select(Region,str_c("X",c(2015,SSPs_year[j])))-> SSP_GDP
      
      colnames(SSP_GDP) <- c("Region","GDP2015","GDPTar")
      colnames(SSP_Pop) <- c("Region","Pop2015","PopTar")
      
      AAA <- merge(merge(SSP_GDP,SSP_Pop,by = "Region",all = T),Reg_conco,by = "Region")
      Socio_Eco <- rowsum(AAA[,2:5],AAA$GTAPreg,na.rm = T)
      rm(AAA);gc()
      
      Scenario <- array(0,dim = c(G,4))
      rownames(Scenario) <- regnam
      colnames(Scenario) <- c("Pop_Chg","GDP_per_Chg","Pop","GDP")
      Scenario[match(rownames(Socio_Eco),toupper(regnam)),1] <- Socio_Eco$PopTar/Socio_Eco$Pop2015
      Scenario[match(rownames(Socio_Eco),toupper(regnam)),2] <- 
        (Socio_Eco$GDPTar/Socio_Eco$PopTar)/(Socio_Eco$GDP2015/Socio_Eco$Pop2015)
      Scenario[match(rownames(Socio_Eco),toupper(regnam)),3] <- Socio_Eco$PopTar
      Scenario[match(rownames(Socio_Eco),toupper(regnam)),4] <- Socio_Eco$GDPTar
      Scenario[is.na(Scenario)] <- 0
      Scenario <- as.data.frame(Scenario)
      
      #Final demand
      #-----------
      FD_reg_Pop <- t(t(FD_reg[,1:G])*Scenario$Pop_Chg)
      
      Demand_Incom_int <- t(t(IncPara)*((Scenario$GDP_per_Chg-1)*colSums(FD_reg_Pop[,1:G])))
      Demand_Ori <- rowsum(FD_reg_Pop,rep(secnam,G),reorder = F)
      
      #re-adjust the demand structure based on the original structural.
      Demand_Incom <- Demand_Incom_int
      Ela <- unique(rowSums(IncPara))
      for (f in 1:length(Ela)) {
        a <- which(rowSums(IncPara) %in% Ela[f])
        if (length(a) != 1){
          Demand_Incom[a,] <- t(t(Demand_Ori[a,])/colSums(Demand_Ori[a,]))[,1:G]*Demand_Incom_int[a,]*length(a)
        }
      }
      Demand_Incom[is.na(Demand_Incom)] <- 0
      
      FD_reg_Inc <- array(0,dim = dim(FD_reg_Pop))
      for (p in 1:G) {
        m <- (p-1)*N+1;n <- p*N
        FD_reg_Inc[m:n,1:G] <- FD_reg_Pop[m:n,1:G]/Demand_Ori[,1:G]*Demand_Incom+FD_reg_Pop[m:n,1:G]
      }
      FD_reg_Inc[is.na(FD_reg_Inc)] <- 0
      #-----------
      
      #Simulation
      #-----------
      HANPP_Footprint_Project[,j+1] <- cHANPP_inten%*%FD_reg_Inc
      HANPP_Footprint_Inten[,j+1] <- (cHANPP_inten-wB2017)%*%FD_reg[,1:G]
      HANPP_Footprint_Pop[,j+1] <- cHANPP_inten%*%(FD_reg_Pop-FD_reg[,1:G])
      HANPP_Footprint_Inc[,j+1] <- cHANPP_inten%*%(FD_reg_Inc-FD_reg_Pop)
      Pop_Project[,j+1] <- Scenario$Pop
      GDP_Project[,j+1] <- Scenario$GDP
      #-----------
    }
    
    save(HANPP_Footprint_Project,Pop_Project,GDP_Project,
         HANPP_Footprint_Inten,HANPP_Footprint_Pop,HANPP_Footprint_Inc,
         file = str_c(pathout,"/",SSPs[i],"cHANPP projection",HANPP_est[k],".Rdata"))
    print(paste("Time cost: ",SSPs[i],round(Sys.time()-t0,2)))
  }
}
#----------
#======================
rm(A,AX,B,c,cHANPP_inten,Con.NPP,Con.NPP.perCap,Con.NPP.perUSD,
   Demand_Incom,Demand_Ori,Demand_Incom_int,FD,FD_reg,FD_reg_Inc,FD_reg_Pop,
   GDP_projection,GDP_reg,GTAP_NPP,HANPP_Footprint_Project,HANPPint_Trend, IncPara,
   InOut.NPP,NetTradeNPP,NPPinTrade,OUT,Pop_projection,Pop_reg,Pro.NPP,Pro.NPP.perCap,
   Pro.NPP.perUSD,Reg_conco,Scenario,Sec.Trade,Socio_Eco,SSP_GDP,SSP_Pop,
   wB,wB2004,wB2007,wB2011,wB2014,x,a,b,Ela,i,j,k,m,n,N,NPP,p,q,SSPs,SSPs_year,
   VA,w,X)
gc()
#end

