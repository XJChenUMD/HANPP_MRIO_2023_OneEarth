#HANPP analysis
#Main file
#2022-12-31
#Xiangjie Chen, GEOG, UMD
#xjchen@umd.edu

#Path
#=============
# Sys.setlocale("LC_ALL","Chinese")
library(stringr)
library(tidyverse)

setwd("D:\\科研文件\\C_Database\\GTAP 2004、2007、2011、2014")
path<-getwd( )
pathdata<-str_c(path,"/data")
pathdata_SPP <- c("I:\\我的云端硬盘\\IDB Project\\Data\\SSPs")
pathinc<-str_c(path,"/inc")
pathout<-str_c(path,"/20230829+HANPP footprint, Driver, Projection_141reg65sec(DD Area)");dir.create(pathout)
pathcode <- str_c(path,"/code/HANPP/20230829 new code")
pathout_fig <- paste(pathout,"/Figs",sep = "");dir.create(pathout_fig)
#=============



#Modules
#=============
#Step 1: Footprint, drivers and projection
source(str_c(pathcode,"/","Module 1_HANPP Footprint and Projection.R"))

#Step 2: Driving factors
source(str_c(pathcode,"/","Module 2_Driving factors.R"))

#Step 3: Prepare Visualization
source(str_c(pathcode,"/","Module 3_Prepare Visualization.R"))

#Step 4: Visualization
source(str_c(pathcode,"/","Module 4_Visualization.R"))
#=============
