#Data preparation for Visulization 
#Xiangjie Chen @ GEOG, UMD
#xjchen@umd.edu


HANPP_est <- c("_Baseline","_LowEstimate","_HighEstimate")
for (k in 1:length(HANPP_est)) {
  load(str_c(pathout,"/","All year Footprint",HANPP_est[k],".Rdata"))
  regclass <- read.csv(str_c(pathdata,"/WB GNI percap Income level Region class.csv"))
  
  GTAP_pop <- read.csv(str_c(pathdata,"/GTAP population 141reg65sec five years_Aug2023.csv"),header=T)
  GTAP_pop <- apply(GTAP_pop[,-1], 2, as.numeric)
  GTAP_GDP <- read.csv(str_c(pathdata,"/Deflated GDP141reg65sec five years(100 million)_Aug2023.csv"),header=T)
  
  #-----------------
  #total amount
  eHANPP <- Con.NPP
  HANPP <- Pro.NPP
  netExHANPP <- -NetTradeNPP
  netImHANPP <- NetTradeNPP
  
  #per $
  eHANPP.perUSD <- Con.NPP.perUSD
  HANPP.perUSD <- Pro.NPP.perUSD
  netExHANPP.perUSD <- netExHANPP/GTAP_GDP*(10^4)
  netImHANPP.perUSD <- -1*netExHANPP.perUSD
  
  #per person
  eHANPP.perCap <- Con.NPP.perCap
  HANPP.perCap <- Pro.NPP.perCap
  netExHANPP.perCap <- netExHANPP/GTAP_pop*(10^3)
  netImHANPP.perCap <- -1*netExHANPP.perCap
  
  #inflow and outfolw
  Inflow <- array(0,dim =  c(141,5))
  Outflow <- array(0,dim =  c(141,5))
  year <- c(2004,2007,2011,2014,2017)
  for (i in 1:length(year)) {
    load(str_c(pathout,"/",year[i],"HANPP footprint",HANPP_est[k],".Rdata"))
    Inflow[,i] <- InOut.NPP[,1]
    Outflow[,i] <- InOut.NPP[,2]
  }
  Inflow.perCap <- Inflow/GTAP_pop*(10^3)
  Outflow.perCap <- Outflow/GTAP_pop*(10^3)
  Inflow.perUSD <- Inflow/GTAP_GDP*(10^4)
  Outflow.perUSD <- Outflow/GTAP_GDP*(10^4)
  
  #GDP PerCap
  PerCap <- GTAP_GDP/GTAP_pop/10#k$
  PerCap <- as.matrix(PerCap);dim(PerCap) <- c(141*length(year),1)
  RegClass <- rbind(regclass[,c(3,5,6)],regclass[,c(3,5,6)],
                    regclass[,c(3,5,6)],regclass[,c(3,5,6)],
                    regclass[,c(3,5,6)])
  #-----------------
  
  #-----------------
  #Aggregate all data
  Reg <- rep(rownames(HANPP),length(year))
  
  Y <- rep(year,each=141)
  HANPP <- as.matrix(HANPP);dim(HANPP) <- c(141*length(year),1)
  HANPP.perUSD <- as.matrix(HANPP.perUSD);dim(HANPP.perUSD) <- c(141*length(year),1)
  HANPP.perCap <- as.matrix(HANPP.perCap );dim(HANPP.perCap ) <- c(141*length(year),1)
  Var <- rep("HANPP",141*length(year))
  A <- cbind(Reg,Var,Y,HANPP,HANPP.perUSD,HANPP.perCap,PerCap,RegClass)
  colnames(A) <- c("Region","Variable","Year","by_Region","Per_USD","Per_capita","GDP_Per_capita","RegFullNam","WB_Class","DC")
  
  eHANPP <- as.matrix(eHANPP);dim(eHANPP) <- c(141*length(year),1)
  eHANPP.perUSD <- as.matrix(eHANPP.perUSD);dim(eHANPP.perUSD) <- c(141*length(year),1)
  eHANPP.perCap <- as.matrix(eHANPP.perCap );dim(eHANPP.perCap ) <- c(141*length(year),1)
  Var <- rep("eHANPP",141*length(year))
  B <- cbind(Reg,Var,Y,eHANPP,eHANPP.perUSD,eHANPP.perCap,PerCap,RegClass)
  colnames(B) <- c("Region","Variable","Year","by_Region","Per_USD","Per_capita","GDP_Per_capita","RegFullNam","WB_Class","DC")
  
  netExHANPP <- as.matrix(netExHANPP);dim(netExHANPP) <- c(141*length(year),1)
  netExHANPP.perUSD <- as.matrix(netExHANPP.perUSD);dim(netExHANPP.perUSD) <- c(141*length(year),1)
  netExHANPP.perCap <- as.matrix(netExHANPP.perCap );dim(netExHANPP.perCap ) <- c(141*length(year),1)
  Var <- rep("netExHANPP",141*length(year))
  C <- cbind(Reg,Var,Y,netExHANPP,netExHANPP.perUSD,netExHANPP.perCap,PerCap,RegClass)
  colnames(C) <- c("Region","Variable","Year","by_Region","Per_USD","Per_capita","GDP_Per_capita","RegFullNam","WB_Class","DC")
  
  netImHANPP <- as.matrix(netImHANPP);dim(netImHANPP) <- c(141*length(year),1)
  netImHANPP.perUSD <- as.matrix(netImHANPP.perUSD);dim(netImHANPP.perUSD) <- c(141*length(year),1)
  netImHANPP.perCap <- as.matrix(netImHANPP.perCap );dim(netImHANPP.perCap ) <- c(141*length(year),1)
  Var <- rep("netImHANPP",141*length(year))
  D <- cbind(Reg,Var,Y,netImHANPP,netImHANPP.perUSD,netImHANPP.perCap,PerCap,RegClass)
  colnames(D) <- c("Region","Variable","Year","by_Region","Per_USD","Per_capita","GDP_Per_capita","RegFullNam","WB_Class","DC")
  
  
  Inflow <- as.matrix(Inflow);dim(Inflow) <- c(141*length(year),1)
  Inflow.perUSD <- as.matrix(Inflow.perUSD);dim(Inflow.perUSD) <- c(141*length(year),1)
  Inflow.perCap <- as.matrix(Inflow.perCap );dim(Inflow.perCap ) <- c(141*length(year),1)
  Var <- rep("Inflow",141*length(year))
  E <- cbind(Reg,Var,Y,Inflow,Inflow.perUSD,Inflow.perCap,PerCap,RegClass)
  colnames(E) <- c("Region","Variable","Year","by_Region","Per_USD","Per_capita","GDP_Per_capita","RegFullNam","WB_Class","DC")
  
  Outflow <- as.matrix(Outflow);dim(Outflow) <- c(141*length(year),1)
  Outflow.perUSD <- as.matrix(Outflow.perUSD);dim(Outflow.perUSD) <- c(141*length(year),1)
  Outflow.perCap <- as.matrix(Outflow.perCap );dim(Outflow.perCap ) <- c(141*length(year),1)
  Var <- rep("Outflow",141*length(year))
  G <- cbind(Reg,Var,Y,Outflow,Outflow.perUSD,Outflow.perCap,PerCap,RegClass)
  colnames(G) <- c("Region","Variable","Year","by_Region","Per_USD","Per_capita","GDP_Per_capita","RegFullNam","WB_Class","DC")
  
  Alldata <- rbind(A,B,C,D,E,G)
  Alldata[,1] <- toupper(Alldata[,1])
  gtap_reg <- read.csv2(str_c(pathdata,"/GTAP 10 regsec(From GTAP website).csv"),sep = ",",header = T)[,2:3]
  #-----------------
  
  #Boundary analysis
  #-----------------
  Boundary_global <- read.csv(str_c(pathdata,
                                    "/HANPP Satellite/20230829_HANPP update/Boundary_global",
                                    HANPP_est[k],".csv"))
  
  Boundary_country <- merge(gtap_reg,regclass,by = "Code",sort = F)
  Boundary_country <- Boundary_country %>% select(!c(IncomeGroup,DC.LDC,Number,Description.y))
  colnames(Boundary_country) <- c("Code","Description","IncomeGroup")
  Boundary_country$ConHANPP_2017 <- Con.NPP[,5]#Pg·C
  Boundary_country$ConHANPPpc_2017 <- Con.NPP.perCap[,5]#Pg·C
  Boundary_country$Boundary_2017 <- Boundary_global$X2017[2]/sum(GTAP_pop[,5])*(10^3)
  Boundary_country$BoundaryRatio <- Boundary_country$ConHANPPpc_2017/Boundary_country$Boundary_2017
  #-----------------
  
  
  save(Alldata,gtap_reg,GTAP_GDP,GTAP_pop,regclass,Boundary_global,Boundary_country,
       Con.NPP,Pro.NPP,
       file = str_c(pathout_fig, "/Alldata_for Fig2",HANPP_est[k],".Rdata"))
}
