#Visulization
#Xiangjie Chen @ GEOG, UMD
#xjchen@umd.edu


library(ggh4x)
library(ggrepel)
library(RColorBrewer)
library(ggpubr)
library(viridis)

HANPP_est <- c("_Baseline","_LowEstimate","_HighEstimate")
for (k in 1:length(HANPP_est)) {
  #Fig1 All countries. cHANPP, eHANPP by per capita and per USA. Trade
  #---------------------------------
  load(str_c(pathout_fig, "/Alldata_for Fig2",HANPP_est[k],".Rdata"))
  DATA <- Alldata
  colnames(DATA)
  unique(DATA$Variable)
  
  year <-  c(2004, 2007, 2011, 2014,2017)
  for (i in 1:length(year)) {
    fit1 <-  theme_test()+
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text( face = "bold"),
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))+
      theme(legend.position="bottom",legend.title=element_blank())
    
    #cHANPP and pHANPP----------
    DATA %>% filter(Year %in% year[i]) %>% 
      filter(Variable %in% c("HANPP","eHANPP")) %>% 
      select (Region,Variable,by_Region,WB_Class) %>% 
      pivot_wider(id_cols = c(Region,WB_Class),
                  names_from = Variable,
                  values_from = by_Region) -> A
    
    A %>% arrange(HANPP) %>% slice(132:141) -> A1
    A %>% arrange(eHANPP) %>% slice(132:141) -> A2
    Reglabel <- unique(c(A1$Region,A2$Region))
    A$RegLABEL <- NA
    A$RegLABEL[match(Reglabel, A$Region)] <- A$Region[match(Reglabel, A$Region)]
    
    limit <- c(0,range(A$eHANPP)[2]+0.05)
    
    A %>% ggplot(aes(HANPP,eHANPP,fill=WB_Class)) + 
      geom_point(shape=21,colour="black",
                 stroke=0.25, alpha=0.7)+
      geom_abline(intercept = 0, slope = 1, color = "red")+
      scale_fill_viridis(discrete=TRUE,option = "H",
                         breaks = c("Low income","Lower middle income","Upper middle income","High income")) +
      # scale_fill_manual(breaks = c("Low income","Lower middle income","Upper middle income","High income"),
      #                   values = brewer.pal(n= 4,"Set1"))+
      scale_x_continuous(limits = limit)+
      scale_y_continuous(limits = limit)+
      geom_text_repel(label=A$RegLABEL,size=2.5,max.overlaps = 15, position = "jitter")+
      labs(x = "Territorial HANPP (Pg C/yr)",
           y = "Consumption-based HANPP (Pg C/yr)")+
      theme_test()+
      guides(fill=guide_legend(title = "Income group"))+
      theme(legend.position= "bottom") + 
      fit1 ->  Fig1a
    #------------
    
    #Trade---------
    DATA %>% filter(Year %in% year[i]) %>% 
      filter(Variable %in% c("eHANPP","HANPP","Inflow","Outflow")) %>% 
      select (Region,Variable,by_Region,WB_Class) %>% 
      pivot_wider(id_cols = c(Region,WB_Class),
                  names_from = Variable,
                  values_from = by_Region) %>% 
      mutate(ImShare = Inflow/eHANPP,
             OutShare = Outflow/HANPP,
             NetIm = Inflow-Outflow) %>% 
      select(-c(Inflow,Outflow,eHANPP,HANPP)) ->A
    
    A %>% ggplot(aes(OutShare,ImShare,fill=WB_Class)) + 
      geom_point(shape=21,colour="black",
                 stroke=0.25, alpha=0.7)+
      scale_fill_viridis(discrete=TRUE,option = "H",
                         breaks = c("Low income","Lower middle income","Upper middle income","High income")) +
      # scale_fill_manual(breaks = c("Low income","Lower middle income","Upper middle income","High income"),
      #                   values = brewer.pal(n= 4,"Set1"))+
      geom_text_repel(label=A$Region,size=1.8,max.overlaps = 15, position = "jitter")+
      labs(y = "Import share in consumption-based HANPP",
           x = "Export share in territorial HANPP")+
      theme_test()+
      guides(fill=guide_legend(title = "Income group"))+
      theme(legend.position= "bottom")  + 
      fit1 -> Fig1b
    
    ggarrange(Fig1a,Fig1b,legend = "bottom",common.legend = T)
    ggsave(paste(pathout_fig,"/",year[i],HANPP_est[k],"HANPP and Trade.jpg",sep = ""),
           width = 8,height = 4,dpi = 1000)
    #---------------------
    
    #cHANPP by person and USD---------
    DATA %>% filter(Year %in% year[i]) %>% 
      filter(Variable %in% "eHANPP") %>% 
      select (Region,Variable,by_Region,Per_USD,Per_capita,WB_Class) -> A
    
    A %>% arrange(Per_USD) %>% slice(132:141) -> A1
    A %>% arrange(Per_capita) %>% slice(132:141) -> A2
    A %>% arrange(by_Region) %>% slice(132:141) -> A3
    Reglabel <- unique(c(A1$Region,A2$Region,A3$Region))
    A$RegLABEL <- NA
    A$RegLABEL[match(Reglabel, A$Region)] <- A$Region[match(Reglabel, A$Region)]
    
    A %>% ggplot(aes(Per_USD,Per_capita,size = by_Region, fill=WB_Class)) + 
      geom_point(shape=21,colour="black",
                 stroke=0.25, alpha=0.7)+
      scale_fill_viridis(discrete=TRUE,option = "H",
                         breaks = c("Low income","Lower middle income","Upper middle income","High income")) +
      # scale_fill_manual(breaks = c("Low income","Lower middle income","Upper middle income","High income"),
      #                   values = brewer.pal(n= 4,"Set1"))+
      scale_size_continuous(range = c(0,12))+
      geom_text_repel(label=A$RegLABEL,size=3,max.overlaps = 15, position = "jitter")+
      labs(x = "Consumption-based HANPP per GDP (Kg C/$)",
           y = "Consumption-based HANPP per capita (t C/person)")+
      guides(size=guide_legend(title = "Consumption-based\nHANPP (Pg C/yr)"),
             fill=guide_legend(title = "Income group"))+
      theme_test()+
      theme(legend.position= c(0.85,0.27))+
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave (paste(pathout_fig, "/",year[i],HANPP_est[k],"_cHANPP by USD and per capita.png",sep = ""),
            dpi = 1000,width = 6, height = 6)
    #------------
  }
  #-------------
  
  #Consumption-based Planetary boundary
  #---------------------------------
  Boundary_global %>% 
    mutate(variable = rownames(Boundary_global)) %>% 
    pivot_longer(-variable) %>% 
    mutate(name = substr(name, 2,5)) %>% 
    filter(variable %in% c("HANPP","Remain","NPP","Boundary")) %>% 
    mutate(variable = factor(variable, 
                             levels = c("NPP","Boundary","HANPP","Remain"))) %>% 
    ggplot(aes(name,value))+
    facet_wrap(~variable, nrow =4, scales = "free_y",
               labeller = labeller(variable = c(HANPP = "HANPP\n(Pg C/yr)",
                                           Remain = "Remaining Space\nPg C/yr)",
                                           NPP = "Potential NPP\n(Pg C/yr)",
                                           Boundary = "Planetary Boundary\n(Pg C/yr) ")))+
    geom_line(aes(group = variable),color = "red")+
    geom_point(shape = 16)+
    theme_test()+
    labs(x=NULL, y = NULL)+ 
    theme(strip.text.x = element_text(face = "bold"),
          strip.background.x = element_rect(fill = "gray", colour = "black")) +
    theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black")) -> Fig3a
  
  Boundary_country %>% 
  ggplot(aes(IncomeGroup,BoundaryRatio,group=Description))+
    geom_point(aes(size=ConHANPP_2017,fill=IncomeGroup),shape=21,alpha=0.7)+
    # geom_jitter(aes(size=ConHANPP_2017,fill=IncomeGroup),
    #             position = position_jitter(0.5), 
    #             shape=21,colour="black",
    #             stroke=0.25, alpha=0.7)+
    geom_hline(aes(yintercept=1),linetype="dashed",color="black",linewidth=0.8)+
    geom_text_repel(label=Boundary_country$Code,size=2.2,max.overlaps = 25)+
    scale_x_discrete(limits = c("Low income","Lower middle income","Upper middle income","High income"))+
    scale_size_continuous(range = c(0,8))+
    guides(size=guide_legend(title = "Consumption-based\nHANPP (Pg C/yr)"))+
    scale_fill_viridis(discrete=TRUE,option = "H",guide = "none",
                       breaks = c("Low income","Lower middle income","Upper middle income","High income")) +
    # scale_fill_manual(values= brewer.pal(n= 4,"Set1"),
    #                    breaks= c("Low income","Lower middle income","Upper middle income","High income"),
    #                    guide = "none")+
    labs(x=NULL, y = "The ratio of consumption-based HANPP to planetary boundary")+
    theme_test()+
    theme(axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          axis.text.x = element_text(color="black"),
          axis.text.y = element_text(color="black"))+
    theme(legend.position= c(0.5,0.85),
          legend.direction = "horizontal") ->  Fig3b
  
  ggarrange(Fig3a,Fig3b,widths = c(2,5))
  ggsave(paste(pathout_fig,"/",HANPP_est[k],"Boundary analysis.jpg",sep = ""),
         width = 8,height = 8*5/7,dpi = 1000)
  #----------------------------------------
  
  #Changes analysis
  #--------------------------------
  b <- DATA %>% filter(Variable %in% "HANPP",Year %in% "2004")#start year
  f <- DATA %>% filter(Variable %in% "eHANPP",Year %in% "2004")#start year
  d <- DATA %>% filter(Variable %in% "HANPP",Year %in% "2017")#end year
  e <- DATA %>% filter(Variable %in% "eHANPP",Year %in% "2017")#end year
  
  a <- cbind(b[,c(1,8,4)],f[,4],d[,c(4)],e[,c(4,9)],GTAP_GDP[,c(1,4)])
  colnames(a) <- c("regcode","regnam","HANPP2004","eHANPP2004","HANPP2017","eHANPP2017","WB_Class","GDP2004","GDP2017")
  
  #Changes
  a$ChangeHANPP <- a$HANPP2017-a$HANPP2004
  a$ChangeeHANPP <- a$eHANPP2017-a$eHANPP2004
  
  xrng <- c(range(a$ChangeHANPP)[1]-0.1,range(a$ChangeHANPP)[2]+0.1)
  yrng <- c(range(a$ChangeeHANPP)[1]-0.1,range(a$ChangeeHANPP)[2]+0.1)
  
  lab <- rep(NA,141)
  lab[which(abs(a$ChangeHANPP) > 0.05 | abs(a$ChangeeHANPP) > 0.08)] <- 
    a$regcode[which(abs(a$ChangeHANPP) > 0.05 | abs(a$ChangeeHANPP) > 0.08)]
  
  Fig.c <- ggplot(a,aes(ChangeHANPP,ChangeeHANPP))+
    geom_rect(aes(ymin = 0, ymax =yrng[2], xmin= xrng[1], xmax =0,fill = "A"),alpha =0.3)+
    geom_rect(aes(ymin = 0, ymax =yrng[2], xmin= 0, xmax =xrng[2],fill = "B"),alpha =0.3)+
    geom_rect(aes(ymin = yrng[1], ymax =0, xmin= xrng[1], xmax =0,fill = "C"),alpha =0.3)+
    geom_rect(aes(ymin = yrng[1], ymax =0, xmin= 0, xmax =xrng[2],fill = "D"),alpha =0.3)+
    geom_point(aes(color=WB_Class),alpha=0.7,position = "identity")+
    geom_text_repel(label=lab,size=3,max.overlaps = 15, position = "jitter")+
    labs(x="Change in territorial HANPP (Pg C/yr)", 
         y = "Change in consumption-based HANPP (Pg C/yr)")+
    scale_color_viridis(discrete=TRUE,option = "H",
                       breaks = c("Low income","Lower middle income","Upper middle income","High income")) +
    # scale_color_manual(values= brewer.pal(n= 4,"Set1"),
    #                    breaks= c("Low income","Lower middle income","Upper middle income","High income"))+guides(color=guide_legend(title=""))+
    scale_fill_manual(guide = "none",values = brewer.pal(n= 4,"Pastel1"))+
    scale_x_continuous(limit= c(xrng[1],xrng[2]),expand = c(0,0))+
    scale_y_continuous(limit= c(yrng[1],yrng[2]),expand = c(0,0))+
    theme_test()+
    fit1+
    theme(legend.position = c(0.8,0.87),
          legend.background = element_rect("transparent"))
  #--------------------------------
  
  #SDA
  #---------------------
  load(str_c(pathout,"/2004-2017",HANPP_est[k],"SDA_cHANPP.Rdata", sep="" ))
  SDA <- as.data.frame(SDA_Con.NPP_TP)
  SDA$Reg <- rownames(SDA)
  
  SDA %>%
    mutate(cHANPP2004 = Con.NPP[,1], cHANPP2017 = Con.NPP[,5]) %>%
    filter(Reg %in% c("jpn","chn","usa","bra")) %>% 
    select(-`Changes in cHANPP`) %>%
    pivot_longer(-Reg)   -> B
  
  B$name[which(B$name %in% 'cHANPP2004')] <- "Consumption-based HANPP for 2004"
  B$name[which(B$name %in% 'cHANPP2017')] <- "Consumption-based HANPP for 2017"
  
  B %>% group_by(Reg) %>% 
    mutate(Rank = c(2:9,1,10)) %>% 
    arrange(Reg, Rank)  %>% 
    mutate(NamFac = fct_reorder (name, Rank)) %>%
    mutate(Start = cumsum(value)-value, End = cumsum(value)) ->B
  
  B$End[which(B$name %in% "Consumption-based HANPP for 2017")] <- B$value[which(B$name %in% "Consumption-based HANPP for 2017")] 
  B$Start[which(B$name %in% "Consumption-based HANPP for 2017")] <- 0
  
  TEXT <- c()
  TEXT[which (B$End-B$Start > 0)] <-  B$End[which (B$End-B$Start  > 0)]*1.1 
  TEXT[which (B$End-B$Start <= 0)] <-  B$End[which (B$End-B$Start <= 0)]*0.85
  
  breaks <- unique(B$name)

  labels <- c("cHANPP for 2004",
              "HANPP intensity(d)","HANPP intensity(f)",
              "Production structure(d)","Production structure(f)", 
              "Consumption patterns(d)","Consumption patterns(f)",
              "Consumption level(d)",   
              "Consumption level(f)","cHANPP for 2017"  )
  
  
  Fig.d <- B %>% ggplot()+
    geom_segment(aes(x = NamFac, xend = NamFac, y = Start, yend =End, color = NamFac),
                 size = 6)+
    geom_text(aes(x= NamFac, y = TEXT, 
                  label = round(value,2)),fontface = "italic",
              size =2)+
    scale_color_viridis(discrete=TRUE,option = "C",
                        breaks = unique(B$NamFac)) +
    # scale_color_manual(breaks = unique(B$NamFac), values = brewer.pal(10, "Paired"))+
    facet_wrap(~Reg, nrow =2, scales = "free_y",
               labeller = labeller(Reg = c(jpn = "Japan",
                                           usa = "USA",
                                           chn = "China",
                                           bra = "Brazil")))+
    labs(x = NULL, y = "Consumption-based HANPP (Pg C/yr)")+
    scale_x_discrete(breaks = breaks, labels = labels)+
    theme_test()+
    fit1+
    theme(strip.text.x = element_text(face = "bold"),
          strip.background.x = element_rect(fill = "gray", colour = "black"))+
    theme(axis.text.x = element_text(color = "black",
                                     angle = 420,hjust = 1,vjust = 1),
          axis.text.y = element_text(color = "black"),
          axis.title.y =  element_text(face = "bold"))+
    guides(color = "none")
  ggarrange(Fig.c,Fig.d,common.legend = F,widths = c(2,2))
  ggsave(paste(pathout_fig,"/",HANPP_est[k],"Changes in HANPP and SDA.jpg",sep = ""),
         width = 9,height = 9*2/4,dpi = 1000)
  #---------------------
  
  #SSP projection
  #---------------------
  SSPs <- str_c("SSP",1:5)
  load(str_c(pathout,"/",SSPs[1],"cHANPP projection",HANPP_est[k],".Rdata"))
  Project_S1 <- HANPP_Footprint_Project;Pop_S1 <- Pop_Project;GDP_S1 <- GDP_Project
  load(str_c(pathout,"/",SSPs[2],"cHANPP projection",HANPP_est[k],".Rdata"))
  Project_S2 <- HANPP_Footprint_Project;Pop_S2 <- Pop_Project;GDP_S2 <- GDP_Project
  load(str_c(pathout,"/",SSPs[3],"cHANPP projection",HANPP_est[k],".Rdata"))
  Project_S3 <- HANPP_Footprint_Project;Pop_S3 <- Pop_Project;GDP_S3 <- GDP_Project
  load(str_c(pathout,"/",SSPs[4],"cHANPP projection",HANPP_est[k],".Rdata"))
  Project_S4 <- HANPP_Footprint_Project;Pop_S4 <- Pop_Project;GDP_S4 <- GDP_Project
  load(str_c(pathout,"/",SSPs[5],"cHANPP projection",HANPP_est[k],".Rdata"))
  Project_S5 <- HANPP_Footprint_Project;Pop_S5 <- Pop_Project;GDP_S5 <- GDP_Project
  
  
  SSPs_year <- c(2017,seq(2020,2100,5))
  #global
  Global <- array(0,dim = c(5,length(SSPs_year)))
  rownames(Global) <- SSPs;colnames(Global) <- SSPs_year 
  Global[1,] <- colSums(Project_S1);Global[2,] <- colSums(Project_S2)
  Global[3,] <- colSums(Project_S3);Global[4,] <- colSums(Project_S4)
  Global[5,] <- colSums(Project_S5)
  Global <- as.data.frame(Global)
  Global$Scenarios <- SSPs
  
  Fig.a <- Global %>% pivot_longer(-Scenarios,
                                   names_to = "Year",
                                   values_to = "HANPP") %>% 
    ggplot()+
    geom_hline(yintercept = Boundary_global$X2017[1],color = "gray",linetype = "dotted",size =1)+
    geom_hline(yintercept = Boundary_global$X2017[2],color = "gray",size =1)+
    annotate("text",x = "2085", y = Boundary_global$X2017[1]+3, 
             label = "Global potential NPP in 2017",fontface = "italic")+
    annotate("text",x = "2080", y = Boundary_global$X2017[2]+3, 
             label = "Global planetary boundary of HANPP in 2017",fontface = "italic")+
    geom_path(aes(Year,HANPP,group = Scenarios,color = Scenarios),alpha = 0.5,size=1)+
    geom_point(aes(Year,HANPP,group = Scenarios,color = Scenarios),size =1,alpha = 0.7)+
    labs(x=NULL,y="Consumption-based HANPP (Pg C/yr)")+
    scale_x_discrete(breaks = c(seq(2025,2100,15)))+
    scale_color_viridis(discrete=TRUE,option = "C",
                        breaks = SSPs,labels = SSPs) +
    # scale_color_manual(breaks = SSPs,values = brewer.pal(5,"Set1"),labels = SSPs)+
    theme_test() + fit1+
    theme(legend.position= c(0.65,0.8))

    
  Region <- rbind(rowsum(Project_S1,regclass$IncomeGroup2,reorder = F),
                  rowsum(Project_S2,regclass$IncomeGroup2,reorder = F),
                  rowsum(Project_S3,regclass$IncomeGroup2,reorder = F),
                  rowsum(Project_S4,regclass$IncomeGroup2,reorder = F),
                  rowsum(Project_S5,regclass$IncomeGroup2,reorder = F))
  
  
  round(t(rowsum(Project_S1,regclass$IncomeGroup2,reorder = F)[,c(1,18)])/colSums(rowsum(Project_S1,regclass$IncomeGroup2,reorder = F)[,c(1,18)]),3)
  round(t(rowsum(Project_S2,regclass$IncomeGroup2,reorder = F)[,c(1,18)])/colSums(rowsum(Project_S2,regclass$IncomeGroup2,reorder = F)[,c(1,18)]),3)
  round(t(rowsum(Project_S3,regclass$IncomeGroup2,reorder = F)[,c(1,18)])/colSums(rowsum(Project_S3,regclass$IncomeGroup2,reorder = F)[,c(1,18)]),3)
  round(t(rowsum(Project_S4,regclass$IncomeGroup2,reorder = F)[,c(1,18)])/colSums(rowsum(Project_S4,regclass$IncomeGroup2,reorder = F)[,c(1,18)]),3)
  round(t(rowsum(Project_S5,regclass$IncomeGroup2,reorder = F)[,c(1,18)])/colSums(rowsum(Project_S5,regclass$IncomeGroup2,reorder = F)[,c(1,18)]),3)
  
  Region <- as.data.frame(Region)
  
  Region$Scenarios <- rep(SSPs,each = 4)
  Region$Class <- rep(unique(regclass$IncomeGroup2))
  
  Fig.b <- Region %>% pivot_longer(-c(Scenarios,Class),
                                   names_to = "Year",
                                   values_to = "HANPP") %>% 
    ggplot()+
    geom_path(aes(Year,HANPP,group = Scenarios,color = Scenarios),
              alpha = 0.5,size =0.3)+
    geom_point(aes(Year,HANPP,group = Scenarios,color = Scenarios),
               alpha = 0.7,size =0.3)+
    facet_wrap(~Class,nrow = 2)+
    labs(x=NULL,y=NULL)+
    scale_x_discrete(breaks = c(seq(2025,2100,15)))+
    scale_color_viridis(discrete=TRUE,option = "C",
                        breaks = SSPs,labels = SSPs, guide = "none")+
    # scale_color_manual(breaks = SSPs,values = brewer.pal(5,"Set1"),
    #                    labels = SSPs, guide = "none")+
    theme_test()+
    fit1+
    theme(strip.text.x = element_text(face = "bold"),
          strip.background.x = element_rect(fill = "gray", colour = "black")) +
    theme(panel.background = element_rect(fill = "transparent"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5,vjust = 0.5))
  
  main_y_range <- layer_scales(Fig.a)$y$get_limits()
  
  Fig.a +
    annotation_custom(
      ggplotGrob(Fig.b),
      xmin = "2017", xmax = "2060",
      ymin = main_y_range[2]/2, ymax = main_y_range[2]+5
    )
  ggsave (paste(pathout_fig, "/",HANPP_est[k],"Global and Region projection.jpg",sep = ""),
          dpi = 1000,width = 7, height = 5)
  #---------------------
  
  #SSP GDP and Pop projection----------
  #GDP
  Global_GDP <- array(0,dim = c(5,length(SSPs_year)))
  rownames(Global_GDP) <- SSPs;colnames(Global_GDP) <- SSPs_year 
  Global_GDP[1,] <- colSums(GDP_S1);Global_GDP[2,] <- colSums(GDP_S2)
  Global_GDP[3,] <- colSums(GDP_S3);Global_GDP[4,] <- colSums(GDP_S4)
  Global_GDP[5,] <- colSums(GDP_S5)
  Global_GDP <- as.data.frame(Global_GDP)
  Global_GDP$Scenarios <- SSPs
  Fig.a <- Global_GDP %>% pivot_longer(-Scenarios,
                                   names_to = "Year",
                                   values_to = "GDP") %>% 
    ggplot()+
    geom_path(aes(Year,GDP/10^3,group = Scenarios,color = Scenarios),alpha = 0.5,size=1)+
    geom_point(aes(Year,GDP/10^3,group = Scenarios,color = Scenarios),size =1,alpha = 0.7)+
    labs(x=NULL,y="GDP projection (trillion $)")+
    scale_x_discrete(breaks = c(seq(2025,2100,15)))+
    scale_color_viridis(discrete=TRUE,option = "C",
                        breaks = SSPs,labels = SSPs) +
    theme_test() + fit1
  
  #GDP
  Global_Pop <- array(0,dim = c(5,length(SSPs_year)))
  rownames(Global_Pop) <- SSPs;colnames(Global_Pop) <- SSPs_year 
  Global_Pop[1,] <- colSums(Pop_S1);Global_Pop[2,] <- colSums(Pop_S2)
  Global_Pop[3,] <- colSums(Pop_S3);Global_Pop[4,] <- colSums(Pop_S4)
  Global_Pop[5,] <- colSums(Pop_S5)
  Global_Pop <- as.data.frame(Global_Pop)
  Global_Pop$Scenarios <- SSPs
  Fig.b <- Global_Pop %>% pivot_longer(-Scenarios,
                                       names_to = "Year",
                                       values_to = "POP") %>% 
    ggplot()+
    geom_path(aes(Year,POP,group = Scenarios,color = Scenarios),alpha = 0.5,size=1)+
    geom_point(aes(Year,POP,group = Scenarios,color = Scenarios),size =1,alpha = 0.7)+
    labs(x=NULL,y="Population projection (million)")+
    scale_x_discrete(breaks = c(seq(2025,2100,15)))+
    scale_color_viridis(discrete=TRUE,option = "C",
                        breaks = SSPs,labels = SSPs) +
    theme_test() + fit1
  
  Global_GDPper <- Global_GDP
  Global_GDPper[,1:18] <- Global_GDP[,1:18]/Global_Pop[,1:18]
  Fig.c <- Global_GDPper %>% pivot_longer(-Scenarios,
                                       names_to = "Year",
                                       values_to = "GDPper") %>% 
    ggplot()+
    geom_path(aes(Year,GDPper,group = Scenarios,color = Scenarios),alpha = 0.5,size=1)+
    geom_point(aes(Year,GDPper,group = Scenarios,color = Scenarios),size =1,alpha = 0.7)+
    labs(x=NULL,y="GDP per capita projection (thousand $)")+
    scale_x_discrete(breaks = c(seq(2025,2100,15)))+
    scale_color_viridis(discrete=TRUE,option = "C",
                        breaks = SSPs,labels = SSPs) +
    theme_test() + fit1
  ggarrange(Fig.c,Fig.b,legend = "bottom",common.legend = T)
  ggsave(paste(pathout_fig,"/Pop and GDP percaita projection under basic SSPs.jpg",sep = ""),
         width = 8,height = 4,dpi = 1000)
  #-----------
}
